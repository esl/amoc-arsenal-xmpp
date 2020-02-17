%%==============================================================================
%% Copyright 2015-2019 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%
%% In this scenario users are sending message to their neighbours
%% (users with lower and greater IDs defined by NUMBER_OF_*_NEIGHBOURS values)
%% Messages will be send NUMBER_OF_SEND_MESSAGE_REPEATS to every selected neighbour
%% after every message given the user will wait MESSAGE_INTERVAL s
%%
%%==============================================================================
-module(mongoose_mam_read_and_send_msgs_with_metrics).

-behaviour(amoc_scenario).

-include_lib("exml/include/exml.hrl").
-include_lib("kernel/include/logger.hrl").

-define(SLEEP_TIME_AFTER_SCENARIO, 10000). %% wait 10s after scenario before disconnecting
-required_variable({message_interval,               <<"Wait time between sent messages (seconds, def: 180)"/utf8>>,                  180,             nonnegative_integer}).
-required_variable({number_of_prev_users,           <<"Number of users before current one to use (def: 1)"/utf8>>,                   1,               nonnegative_integer}).
-required_variable({number_of_next_users,           <<"Number of users after current one to use (def: 1)"/utf8>>,                    1,               nonnegative_integer}).
-required_variable({number_of_send_message_repeats, <<"Number of send message (to all neighours) repeats (def: 73)"/utf8>>,          73,              positive_integer}).
-required_variable({mam_reader_sessions_indicator,  <<"How often a MAM reader is created, like every 53th session (def: 53)"/utf8>>, 53,              positive_integer}).
-required_variable({mam_read_archive_interval,      <<"Wait time between reads from MAM for each reader (seconds, def: 60)"/utf8>>,  60,              positive_integer}).
-required_variable({mim_host,                       <<"The virtual host served by the server (def: <<\"localhost\">>)"/utf8>>,       <<"localhost">>, bitstring}).

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
      amoc_xmpp_handlers:stanza_handlers(
        [{fun escalus_pred:is_message/1, fun amoc_xmpp_handlers:measure_ttd/3}])},
     {sent_stanza_handlers,
      amoc_xmpp_handlers:stanza_handlers(
        [{fun escalus_pred:is_message/1, fun amoc_xmpp_handlers:measure_sent_messages/0}])}
    ].

%%%%%
%% MAM helpers
%%%%%

-spec read_messages_from_archive_since_timestamp(
        Client :: escalus:client(),
        Timestamp :: erlang:timestamp(),
        Timeout :: timer:time()
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
        Timeout :: timer:time()
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
        Timeout :: timer:time()) -> ok | no_return().
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
        Timeout :: timer:time()) -> ok | no_return().
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

