%==============================================================================
%% @copyright 2019-2020 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%% @end
%%
%% @doc
%% In this scenario, users are entering MUC Rooms and exchanging messages.
%% Each room has one sender.
%%
%% == User steps: ==
%%
%% 1. Connect to the XMPP host.
%%
%% 2. Send presence `available'.
%%
%% 3. Enter selected MUC rooms. After each entered room, wait for
%%    `delay_after_entering_room' before joining another MUC room.
%%
%% 4. Wait for the `delay_before_sending_messages'.
%%
%% 5. Start sending messages to the selected MUC rooms. The number of messages
%%    to be sent per room is defined by the `messages_to_send_per_room' variable.
%%    The rate of messages that is being sent is defined by the
%%    `message_interval_per_room' variable.
%%
%% 6. Receive messages, notifications and presences from the MUC rooms and update
%%    the metrics accordingly.
%%
%% == Metrics exposed by this scenario: ==
%%
%%  === Counters: ===
%%    - muc_rooms_created - incremented for every MUC room that has been created.
%%
%%    - muc_occupants - incremented for every user that joins a MUC room.
%%
%%    - muc_messages_sent - incremented for every MUC message that is being sent.
%%
%%    - muc_messages_received - incremented for every MUC message that is being received.
%%
%%    - muc_presences_received - incremented for every received MUC presence.
%%
%%    - muc_notifications_received - incremented for every received MUC notification.
%%
%%    - timeouts - incremented for every request that resulted in a timeout.
%%
%%  === Times: ===
%%   - response - response time for every request that was sent.
%%
%%   - muc_message_tdd - MUC message time to delivery
%%
%% @end
%%==============================================================================
-module(mongoose_muc).

-behaviour(amoc_scenario).

-export([init/0,
         start/1]).

-include_lib("exml/include/exml.hrl").
-include_lib("kernel/include/logger.hrl").

-required_variable([
    #{name => rooms_per_user, default_value => 10,
      description => "rooms per user"},
    #{name => users_per_room, default_value => 20,
      description => "users per room"},
    #{name => delay_before_sending_messages, default_value => 0,
      description => "delay before sending messages"},
    #{name => delay_after_entering_room, default_value => 10000,
      description => "delay after entering room"},
    #{name => messages_to_send_per_room, default_value => 1000,
      description => "messages to send per room"},
    #{name => message_interval_per_room, default_value => 1000,
      description => "message interval per room"}
]).

-spec init() -> ok.
init() ->
    ?LOG_INFO("init the scenario"),
    amoc_metrics:init(counters, muc_rooms_created),
    amoc_metrics:init(counters, muc_occupants),
    amoc_metrics:init(counters, muc_messages_sent),
    amoc_metrics:init(counters, muc_messages_received),
    amoc_metrics:init(counters, muc_presences_received),
    amoc_metrics:init(counters, muc_notifications_received),
    amoc_metrics:init(counters, timeouts),
    amoc_metrics:init(times, response),
    amoc_metrics:init(times, muc_message_ttd),
    ok.

-spec start(amoc_scenario:user_id()) -> no_return().
start(Id) ->
    {ok, Client, _Spec} = amoc_xmpp:connect_or_exit(Id, extra_user_spec()),
    send_presence_available(Client),
    RoomIds = amoc_xmpp_muc:rooms_to_join(Id, cfg(rooms_per_user), cfg(users_per_room)),
    RoomJids = [room_jid(RoomId) || RoomId <- RoomIds],
    enter_rooms(Client, RoomJids),

    escalus_connection:wait(Client, cfg(delay_before_sending_messages)),

    %% 'rooms_to_create/3' assigns one creator to each room
    %% In this case it is used to assign one sender to each room
    RoomIdsToSend = amoc_xmpp_muc:rooms_to_create(Id, cfg(rooms_per_user), cfg(users_per_room)),
    RoomJidsToSend = [room_jid(RoomId) || RoomId <- RoomIdsToSend],
    send_messages(Client, my_timetable(RoomJidsToSend), 0),

    escalus_connection:wait_forever(Client).

extra_user_spec() ->
    [{sent_stanza_handlers, sent_stanza_handlers()},
     {received_stanza_handlers, received_stanza_handlers()}].

send_presence_available(Client) ->
    Pres = escalus_stanza:presence(<<"available">>),
    Pred = fun(Stanza) ->
                   escalus_pred:is_presence_with_type(<<"available">>, Stanza)
                       andalso escalus_pred:is_stanza_from(Client, Stanza)
           end,
    amoc_xmpp:send_request_and_get_response(Client, Pres, Pred, response, 10000).

enter_rooms(_Client, []) -> ok;
enter_rooms(Client, [RoomJid | Rest]) ->
    enter_room(Client, RoomJid),
    escalus_connection:wait(Client, cfg(delay_after_entering_room)),
    enter_rooms(Client, Rest).

enter_room(Client, RoomJid) ->
    Nick = escalus_client:username(Client),
    RoomFullJid = room_full_jid(RoomJid, Nick),
    Req = stanza_muc_enter_room(RoomFullJid),
    Resp = amoc_xmpp:send_request_and_get_response(
             Client, Req, fun(Stanza) -> is_muc_presence_resp(Req, Stanza) end, response, 10000),
    amoc_metrics:update_counter(muc_presences_received),
    case room_entry_response_type(Resp) of
        created ->
            amoc_metrics:update_counter(muc_rooms_created),
            ?LOG_INFO("~s created room ~s", [Nick, RoomJid]);
        joined ->
            ?LOG_INFO("~s joined room ~s", [Nick, RoomJid])
    end,
    amoc_metrics:update_counter(muc_occupants).

send_messages(Client, [{Time, MessageType} | TimeTable], TimePassed) ->
    TimeDiff = max(0, Time - TimePassed),
    escalus_connection:wait(Client, TimeDiff),
    send_message(Client, MessageType),
    send_messages(Client, TimeTable, TimePassed + TimeDiff);
send_messages(_Client, [], _TimePassed) -> ok.

send_message(Client, {muc_message, RoomJid}) ->
    escalus_connection:send(Client, message_to_room(RoomJid)).

message_to_room(RoomJid) ->
    Timestamp = integer_to_binary(os:system_time(microsecond)),
    escalus_stanza:groupchat_to(RoomJid, Timestamp).

my_timetable(RoomJidsToSend) ->
    lists:merge([room_message_timetable(RoomJid) || RoomJid <- RoomJidsToSend]).

room_message_timetable(RoomJid) ->
    Count = cfg(messages_to_send_per_room),
    Interval = cfg(message_interval_per_room),
    Offset = rand:uniform(Interval),
    timetable({muc_message, RoomJid}, Count, Interval, Offset).

timetable(Event, Count, Interval, Offset) ->
    [{Interval * I + Offset, Event} || I <- lists:seq(0, Count - 1)].

stanza_muc_enter_room(RoomJid) ->
    escalus_stanza:to(escalus_stanza:presence(<<"available">>), RoomJid).

room_entry_response_type(Stanza) ->
    case exml_query:attr(Stanza, <<"type">>) of
        undefined ->
            StatusList = exml_query:paths(Stanza, [{element, <<"x">>},
                                                   {element, <<"status">>},
                                                   {attr, <<"code">>}]),
            [Affiliation] = exml_query:paths(Stanza, [{element, <<"x">>},
                                                      {element, <<"item">>},
                                                      {attr, <<"affiliation">>}]),
            [Role] = exml_query:paths(Stanza, [{element, <<"x">>},
                                               {element, <<"item">>},
                                               {attr, <<"role">>}]),
            room_entry_success_response_type(lists:sort(StatusList), Affiliation, Role);
        <<"error">> ->
            true = escalus_pred:is_error(<<"cancel">>, <<"item-not-found">>, Stanza),
            locked
    end.

room_entry_success_response_type([<<"110">>, <<"201">>], <<"owner">>, <<"moderator">>) -> created;
room_entry_success_response_type([<<"110">>], <<"none">>, <<"participant">>) -> joined.

%% Handlers

sent_stanza_handlers() ->
    amoc_xmpp_handlers:make_stanza_handlers(
      [{fun is_muc_message/1,
        fun() -> amoc_metrics:update_counter(muc_messages_sent) end}]).

received_stanza_handlers() ->
    amoc_xmpp_handlers:make_stanza_handlers(
      [{fun is_muc_presence/1,
        fun() -> amoc_metrics:update_counter(muc_presences_received) end},
       {fun is_muc_subject_notification/1,
        fun() -> amoc_metrics:update_counter(muc_notifications_received) end},
       {fun is_muc_message/1,
        fun(_, Stanza, Metadata) ->
                amoc_metrics:update_counter(muc_messages_received),
                amoc_metrics:update_time(muc_message_ttd, ttd(Stanza, Metadata))
        end},
       {fun(_) -> true end,
        fun(_, Stanza) -> ?LOG_WARNING("Skipping received stanza ~p", [Stanza]) end}]).

%% Predicates

is_muc_presence_resp(Req, Resp = #xmlel{name = <<"presence">>}) ->
    is_muc_presence(Resp) andalso
        escalus_utils:jid_to_lower(exml_query:attr(Req, <<"to">>)) =:=
        escalus_utils:jid_to_lower(exml_query:attr(Resp, <<"from">>));
is_muc_presence_resp(_, _) -> false.

is_muc_presence(Stanza) ->
    exml_query:path(Stanza, [{element, <<"x">>}, {attr, <<"xmlns">>}]) =:= ns(muc_user).

is_muc_subject_notification(Stanza) ->
    is_muc_message(Stanza)
        andalso exml_query:subelement(Stanza, <<"subject">>) =/= undefined
        andalso lists:member(exml_query:path(Stanza, [{element, <<"body">>}, cdata]), [<<>>, undefined]).

is_muc_message(Stanza = #xmlel{name = <<"message">>}) ->
    exml_query:attr(Stanza, <<"type">>) =:= <<"groupchat">>;
is_muc_message(_) -> false.

%% Helpers

ttd(Stanza, #{recv_timestamp := Recv}) ->
    SentBin = exml_query:path(Stanza, [{element, <<"body">>}, cdata]),
    Recv - binary_to_integer(SentBin).

cfg(Name) ->
    amoc_config:get(Name).

room_full_jid(RoomJid, Nick) ->
    <<RoomJid/binary, $/, Nick/binary>>.

room_jid(RoomId) ->
    <<(room_name(RoomId))/binary, $@, (muc_host())/binary>>.

room_name(RoomId) ->
    <<"room_", (integer_to_binary(RoomId))/binary>>.

muc_host() ->
    <<"muc.localhost">>.

ns(muc_user) ->
    <<"http://jabber.org/protocol/muc#user">>.
