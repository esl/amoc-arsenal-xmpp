%%==============================================================================
%% @copyright 2019-2020 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%% @end
%%
%% @doc
%% In this scenario users are sending multiple messages to their neighbours in
%% intervals while some are reading messages from MAM.
%%
%% == User steps: ==
%%
%% 1. Connect to the XMPP host given by the `mim_host' variable.
%%
%% 2. Choose a role based on the user ID. Every `mam_reader_sessions_indicator'
%% a user will be assigned the role of `mam_reader'. Other users will become
%% `senders', who send normal messages.
%%
%% === Sender users ===
%%
%% 3. Set filter on incoming stanzas so that only messages are received.
%%
%% 4. Send presence `available' and wait for 5 seconds.
%%
%% 5. Select neighbouring users with lower and greater IDs defined by the
%% `number_of_prev_users' and `number_of_next_users' values.
%%
%% 6. Send messages to every neighbour multiple times (defined by
%% `number_of_send_message_repeats') in a round-robin fashion. After each
%% message wait for `message_interval'.
%%
%% 7. Having sent all messages wait for 10 seconds before sending presence
%% `unavailable' and disconnect.
%%
%% === MAM readers ===
%%
%% 3. Send presence `available'.
%%
%% 4. In a loop, read message archive divided into chunks based on a timestamp.
%% Set filter for iq and message stanzas, query messages from MAM from the last
%% timestamp and receive results. Log received messages and update corresponding
%% metrics. After the end of messages from the message archive set filtering
%% back to messages only and wait for `mam_read_archive_interval'.
%%
%% 5. Continue execution in the loop from point 4.
%%
%% == Metrics exposed by this scenario: ==
%%
%%   === Counters: ===
%%     - messages_sent - it is updated with every sent message by the
%%     `amoc_xmpp_handlers:measure_sent_messages/0' handler.
%%
%%     - mam_lookups - updated with every successful MAM lookup.
%%
%%     - mam_failed_lookups - updated with every failed MAM lookup.
%%
%%   === Times: ===
%%     - message_ttd - it is updated with every received message by the
%%     `amoc_xmpp_handlers:measure_ttd/3' handler.
%%
%%     - mam_lookup_response_time - updated with every successful MAM lookup.
%%
%% @end
%%==============================================================================
-module(mongoose_mam).

-behaviour(amoc_scenario).

-include_lib("exml/include/exml.hrl").
-include_lib("kernel/include/logger.hrl").

-define(SLEEP_TIME_AFTER_SCENARIO, 10000). %% wait 10s after scenario before disconnecting

-define(V(X), fun amoc_config_validation:X/1).

-required_variable([
    #{name => message_interval, default_value => 180, verification => ?V(nonnegative_integer),
      description => "Wait time between sent messages (seconds, def: 180)"},
    #{name => number_of_prev_users, default_value => 1, verification => ?V(nonnegative_integer),
      description => "Number of users before current one to use (def: 1)"},
    #{name => number_of_next_users, default_value => 1, verification => ?V(nonnegative_integer),
      description => "Number of users after current one to use (def: 1)"},
    #{name => number_of_send_message_repeats, default_value => 73, verification => ?V(positive_integer),
      description => "Number of send message (to all neighours) repeats (def: 73)"},
    #{name => mam_reader_sessions_indicator, default_value => 53, verification => ?V(positive_integer),
      description => "How often a MAM reader is created, like every 53th session (def: 53)"},
    #{name => mam_read_archive_interval, default_value => 60, verification => ?V(positive_integer),
      description => "Wait time between reads from MAM for each reader (seconds, def: 60)"},
    #{name => mim_host, default_value => <<"localhost">>, verification => ?V(binary),
      description => "The virtual host served by the server (def: <<\"localhost\">>)"}
]).

%% Wait at most 5s for MAM responses (IQ or message)
-define(MAM_STANZAS_TIMEOUT, 5000).

-export([start/1]).
-export([init/0]).

-define(NS_MAM, <<"urn:xmpp:mam:2">>).

-define(MAM_LOOKUPS_CT, mam_lookups).
-define(MAM_FAILED_LOOKUPS_CT, mam_failed_lookups).
-define(MAM_LOOKUP_RESP_TIME, mam_lookup_response_time).

-type binjid() :: binary().

-spec init() -> ok.
init() ->
    amoc_metrics:init(counters, messages_sent),
    amoc_metrics:init(counters, ?MAM_LOOKUPS_CT),
    amoc_metrics:init(counters, ?MAM_FAILED_LOOKUPS_CT),
    amoc_metrics:init(times, message_ttd),
    amoc_metrics:init(times, ?MAM_LOOKUP_RESP_TIME),
    ok.

-spec start(amoc_scenario:user_id()) -> any().
start(MyId) ->
    ExtraSpec = [{server, amoc_config:get(mim_host)}, {socket_opts, socket_opts()}] ++
                amoc_xmpp:pick_server([[{host, "127.0.0.1"}]]) ++
                send_and_recv_escalus_handlers(),
    {ok, Client, _} = amoc_xmpp:connect_or_exit(MyId, ExtraSpec),

    MAMReaderIndicator = amoc_config:get(mam_reader_sessions_indicator),
    SessionIndicator = session_indicator(MyId, MAMReaderIndicator),

    do(SessionIndicator, MyId, Client),

    timer:sleep(?SLEEP_TIME_AFTER_SCENARIO),
    escalus_session:send_presence_unavailable(Client),
    escalus_connection:stop(Client),
    ok.

session_indicator(MyId, MAMReader) when MyId rem MAMReader == 0 ->
    mam_reader;
session_indicator(_, _) ->
    sender.

do(sender, MyId, Client) ->
    %% We allow only message stanzas to be delivered to the client process,
    %% there is escalus handler set for such messages so they'll be processed by the handler
    escalus_connection:set_filter_predicate(Client, fun escalus_pred:is_message/1),

    escalus_session:send_presence_available(Client),
    escalus_connection:wait(Client, 5000),

    Prev = amoc_config:get(number_of_prev_users),
    Next = amoc_config:get(number_of_next_users),
    NeighbourIds = lists:delete(MyId, lists:seq(max(1, MyId - Prev),
                                                MyId + Next)),
    MessageInterval = amoc_config:get(message_interval),
    send_messages_many_times(Client, timer:seconds(MessageInterval), NeighbourIds);
do(mam_reader, _MyId, Client) ->
    escalus_session:send_presence_available(Client),
    read_archive_forever(Client, erlang:timestamp()).

%%%%%
%% Scenario helpers
%%%%%

-spec read_archive_forever(escalus:client(), erlang:timestamp()) -> no_return().
read_archive_forever(Client, Timestamp) ->
    CurrentTimestamp = erlang:timestamp(),
    read_messages_from_archive_since_timestamp(Client, Timestamp, ?MAM_STANZAS_TIMEOUT),
    Interval = amoc_config:get(mam_read_archive_interval),
    escalus_connection:wait(Client, timer:seconds(Interval)),
    read_archive_forever(Client, CurrentTimestamp).

-spec send_messages_many_times(escalus:client(), timeout(), [binjid()]) -> ok.
send_messages_many_times(Client, MessageInterval, NeighbourIds) ->
    S = fun(_) ->
                send_messages_to_neighbors(Client, NeighbourIds, MessageInterval)
        end,
    SendMessageRepeats = amoc_config:get(number_of_send_message_repeats),
    lists:foreach(S, lists:seq(1, SendMessageRepeats)).

-spec send_messages_to_neighbors(escalus:client(), [amoc_scenario:user_id()], timeout()) -> list().
send_messages_to_neighbors(Client, TargetIds, SleepTime) ->
    [
     send_message(Client, TargetId, SleepTime) ||
     TargetId <- TargetIds
    ].

-spec send_message(escalus:client(), amoc_scenario:user_id(), timeout()) -> ok.
send_message(Client, ToId, SleepTime) ->
    Body = base64:encode(<<"Message_random_", (crypto:strong_rand_bytes(80 + rand:uniform(40)))/binary>>),
    Msg = escalus_stanza:chat_to_with_id_and_timestamp(amoc_xmpp_users:make_jid(ToId), Body),
    escalus_connection:send(Client, Msg),
    escalus_connection:wait(Client, SleepTime).

-spec send_and_recv_escalus_handlers() -> [{atom(), any()}].
send_and_recv_escalus_handlers() ->
    [{received_stanza_handlers,
      amoc_xmpp_handlers:make_stanza_handlers(
        [{fun escalus_pred:is_message/1, fun amoc_xmpp_handlers:measure_ttd/3}])},
     {sent_stanza_handlers,
      amoc_xmpp_handlers:make_stanza_handlers(
        [{fun escalus_pred:is_message/1, fun amoc_xmpp_handlers:measure_sent_messages/0}])}
    ].

%%%%%
%% MAM helpers
%%%%%

-spec read_messages_from_archive_since_timestamp(
        Client :: escalus:client(),
        Timestamp :: erlang:timestamp(),
        Timeout :: non_neg_integer()
       ) -> any().
read_messages_from_archive_since_timestamp(Client, Timestamp, Timeout) ->
    case catch do_read_messages_from_archive_since_timestamp(Client,
                                                             Timestamp,
                                                             Timeout) of
        {timeout, What} ->
            ?LOG_WARNING("Failed to read archive timeout=~p", [What]),
            amoc_metrics:update_counter(?MAM_FAILED_LOOKUPS_CT, 1);
        {'EXIT', What} ->
            ?LOG_WARNING("Failed to read archive error=~p", [What]),
            amoc_metrics:update_counter(?MAM_FAILED_LOOKUPS_CT, 1);
        ResponseTimeMicros when is_integer(ResponseTimeMicros) ->
            amoc_metrics:update_time(?MAM_LOOKUP_RESP_TIME, ResponseTimeMicros),
            amoc_metrics:update_counter(?MAM_LOOKUPS_CT, 1)
    end.

-spec do_read_messages_from_archive_since_timestamp(
        Client :: escalus:client(),
        Timestamp :: erlang:timestamp(),
        Timeout :: non_neg_integer()
       ) -> ResponseTimeMicroseconds :: integer() |
            no_return(). % escalus throws an exception after Timeout
do_read_messages_from_archive_since_timestamp(Client, Timestamp, Timeout) ->
    filter_out_all_but_mam_archived_messages_and_iqs(Client),
    IQSet = mam_archive_query_since_timestamp(<<"query1">>, Timestamp),
    escalus_connection:send(Client, IQSet),
    {Micros, _} = timer:tc(
                    fun() ->
                            receive_mam_messages_until_end(Client, Timeout)
                    end),
    escalus_connection:set_filter_predicate(Client, fun escalus_pred:is_message/1),
    Micros.

-spec receive_mam_messages_until_end(
        Client :: escalus_connection:client(),
        Timeout :: non_neg_integer()) -> ok | no_return().
receive_mam_messages_until_end(Client, Timeout) ->
    Stanza = escalus_connection:get_stanza(Client, mam_message_timeout, Timeout),
    ?LOG_DEBUG("Stanza = ~p", [Stanza]),
    case is_mam_archived_message(Stanza) of
        false ->
            maybe_mam_fin_message(Stanza, Client, Timeout);
        true ->
            ?LOG_DEBUG("Received MAM archived message=~p", [Stanza]),
            receive_mam_messages_until_end(Client, Timeout)
    end.

-spec maybe_mam_fin_message(
        Stanza :: exml:element(),
        Client :: escalus_connection:client(),
        Timeout :: non_neg_integer()) -> ok | no_return().
maybe_mam_fin_message(Stanza, Client, Timeout) ->
    case is_mam_fin_complete_message(Stanza) of
        true ->
            ?LOG_DEBUG("Received MAM result stanza=~p", [Stanza]),
            ok;
        false ->
            ?LOG_DEBUG("Received stanza=~p when waiting for MAM archived message ~n", [Stanza]),
            receive_mam_messages_until_end(Client, Timeout)
    end.

timestamp_to_isotime({_, _, _} = Timestamp) ->
    FmtStr = "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ",
    {{Y, Mo, D}, {H, Mn, S}} = calendar:now_to_datetime(Timestamp),
    IsoStr = io_lib:format(FmtStr, [Y, Mo, D, H, Mn, S]),
    iolist_to_binary(IsoStr).

%%%%%%
%% Escalus helpers
%%%%%


-spec filter_out_all_but_mam_archived_messages_and_iqs(escalus:client()) -> ok.
filter_out_all_but_mam_archived_messages_and_iqs(Client) ->
    escalus_connection:set_filter_predicate(
      Client,
      fun(Stanza) ->
              is_mam_archived_message(Stanza) orelse
              escalus_pred:is_iq(Stanza)
      end).

%%%%%%
%% User helpers
%%%%%

-spec socket_opts() -> [gen_tcp:option()].
socket_opts() ->
    [binary,
     {reuseaddr, false},
     {nodelay, true}].


%%%%
%% XMPP helpers
%%%%%

mam_archive_query_since_timestamp(QueryId, Timestamp) when is_binary(QueryId) ->
    escalus_stanza:iq_set(?NS_MAM, [mam_lookup_after_date_xml(Timestamp)]).

mam_lookup_after_date_xml(Timestamp) ->
    IsoTime = timestamp_to_isotime(Timestamp),
    TimeValueEl = value_xml(IsoTime),
    MamVsnValueEl = value_xml(?NS_MAM),
    QueryFields =
      [
       field_xml(
         [{<<"var">>, <<"FORM_TYPE">>},
          {<<"type">>, <<"hidden">>}],
         [MamVsnValueEl]),
       field_xml(
         [{<<"var">>, <<"start">>}],
         [TimeValueEl])
      ],
    #xmlel{name = <<"x">>,
           attrs = [
                    {<<"xmlns">>, <<"jabber:x:data">>},
                    {<<"type">>, <<"submit">>}
                   ],
           children = QueryFields
          }.

field_xml(Attrs, Children) ->
    #xmlel{name = <<"field">>,
           attrs = Attrs,
           children = Children}.

value_xml(Data) ->
    #xmlel{name = <<"value">>,
           children = [
                       #xmlcdata{
                          content = Data
                         }
                      ]}.

-spec is_mam_archived_message(exml:element()) -> boolean().
is_mam_archived_message(#xmlel{name = <<"message">>} = Stanza) ->
    NS = exml_query:path(Stanza, [{element, <<"result">>}, {attr, <<"xmlns">>}]),
    NS == ?NS_MAM;

is_mam_archived_message(_) ->
    false.

-spec is_mam_fin_complete_message(exml:element()) -> boolean().
is_mam_fin_complete_message(#xmlel{} = Stanza) ->
    case exml_query:path(Stanza, [{element, <<"fin">>}]) of
        undefined  ->
            false;
        FinEl ->
            exml_query:attr(FinEl, <<"xmlns">>) == ?NS_MAM andalso
                exml_query:attr(FinEl, <<"complete">>) == <<"true">>
    end.
