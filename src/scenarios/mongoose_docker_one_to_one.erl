%%==============================================================================
%% Copyright 2019 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%
%% In this scenario users are sending messages to their neighbours
%% (users with lower and greater IDs defined by NUMBER_OF_*_NEIGHBOURS values)
%% Messages will be sent NUMBER_OF_SEND_MESSAGE_REPEATS to every selected neighbour
%% after every message given the script will wait SLEEP_TIME_AFTER_EVERY_MESSAGE s
%% Message TTD is calculated by the `received_stanza_handler`.
%%
%%==============================================================================
-module(mongoose_docker_one_to_one).

-behaviour(amoc_scenario).

-include_lib("exml/include/exml.hrl").
-include_lib("kernel/include/logger.hrl").

-define(SLEEP_TIME_AFTER_SCENARIO, 10000). %% wait 10s after scenario before disconnecting

-define(V(X), fun amoc_config_validation:X/1).

-required_variable([
    #{name => number_of_prev_neighbours, default_value => 4, verification => ?V(nonnegative_integer),
      description => "Number of users before current one to use (def: 4)"},
    #{name => number_of_next_neighbours, default_value => 4, verification => ?V(nonnegative_integer),
      description => "Number of users after current one to use (def: 4)"},
    #{name => number_of_send_message_repeats, default_value => 73, verification => ?V(positive_integer),
      description => "Number of send message (to all neighours) repeats (def: 73)"},
    #{name => sleep_time_after_every_message, default_value => 20, verification => ?V(nonnegative_integer),
      description => "Wait time between sent messages (seconds, def: 20)"},
    #{name => mim_host, default_value => <<"localhost">>, verification => ?V(binary),
      description => "The virtual host served by the server (def: <<\"localhost\">>)"}
]).

-export([start/1]).
-export([init/0]).

-spec init() -> ok.
init() ->
    ?LOG_INFO("init metrics"),
    amoc_metrics:init(counters, messages_sent),
    amoc_metrics:init(times, message_ttd),
    ok.

-spec start(amoc_scenario:user_id()) -> any().
start(MyId) ->
    ExtraProps = [{server, amoc_config:get(mim_host)}, {socket_opts, socket_opts()}] ++
                amoc_xmpp:pick_server([[{host, "127.0.0.1"}]]) ++
                send_and_recv_escalus_handlers(),
    {ok, Client, _} = amoc_xmpp:connect_or_exit(MyId, ExtraProps),

    do(MyId, Client),

    escalus_connection:wait(Client, ?SLEEP_TIME_AFTER_SCENARIO),
    escalus_session:send_presence_unavailable(Client),
    escalus_connection:stop(Client).

-spec do(amoc_scenario:user_id(), escalus:client()) -> any().
do(MyId, Client) ->
    escalus_connection:set_filter_predicate(Client, fun escalus_pred:is_message/1),

    escalus_session:send_presence_available(Client),
    escalus_connection:wait(Client, 5000),

    PrevNeighbours = amoc_config:get(number_of_prev_neighbours),
    NextNeighbours = amoc_config:get(number_of_next_neighbours),
    NeighbourIds = lists:delete(MyId, lists:seq(max(1, MyId - PrevNeighbours),
                                                MyId + NextNeighbours)),
    SleepTimeAfterMessage = amoc_config:get(sleep_time_after_every_message),
    send_messages_many_times(Client, timer:seconds(SleepTimeAfterMessage), NeighbourIds).

-spec send_messages_many_times(escalus:client(), timeout(), [amoc_scenario:user_id()]) -> ok.
send_messages_many_times(Client, MessageInterval, NeighbourIds) ->
    S = fun(_) ->
                send_messages_to_neighbors(Client, NeighbourIds, MessageInterval)
        end,
    SendMessageRepeats = amoc_config:get(number_of_send_message_repeats),
    lists:foreach(S, lists:seq(1, SendMessageRepeats)).


-spec send_messages_to_neighbors(escalus:client(), [amoc_scenario:user_id()], timeout()) -> list().
send_messages_to_neighbors(Client, TargetIds, SleepTime) ->
    [send_message(Client, TargetId, SleepTime)
     || TargetId <- TargetIds].

-spec send_message(escalus:client(), amoc_scenario:user_id(), timeout()) -> ok.
send_message(Client, ToId, SleepTime) ->
    Body = base64:encode(<<"Message_random_", (crypto:strong_rand_bytes(80 + rand:uniform(40)))/binary>>),
    MsgIn = escalus_stanza:chat_to_with_id_and_timestamp(amoc_xmpp_users:make_jid(ToId), Body),
    escalus_connection:send(Client, MsgIn),
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

-spec socket_opts() -> [gen_tcp:option()].
socket_opts() ->
    [binary,
     {reuseaddr, false},
     {nodelay, true}].
