%==============================================================================
%% @copyright 2019-2020 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%% @end
%%
%% @doc
%% In this scenario, users are creating MUC light Rooms. Users are being added
%% to these rooms and receiving messages from the user that created the room.
%%
%% == User steps: ==
%%
%% 1. Connect to the XMPP host given by the `mim_host' variable.
%%
%% 2. Send presence `available'.
%%
%% 3. Create MUC light rooms and add its members. After each created room,
%%    wait for `delay_after_creating_room' before creating another room.
%%
%% 4. Wait for the `delay_before_sending_messages'.
%%
%% 5. Start sending messages to the rooms created. The number of messages
%%    to be sent per room is defined by the `messages_to_send_per_room' variable.
%%    The rate of messages that is being sent is defined by the
%%    `message_interval_per_room' variable.
%%
%% 6. Receive messages and notifications from the affiliated MUC light rooms
%%    and update the metrics accordingly.
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
-module(mongoose_muc_light).

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
    #{name => delay_after_creating_room, default_value => 10000,
      description => "delay after creating room"},
    #{name => messages_to_send_per_room, default_value => 1000,
      description => "messages to send per room"},
    #{name => message_interval_per_room, default_value => 1000,
      description => "message interval per room"},
    #{name => mim_host, default_value => <<"localhost">>,
      description => "The virtual host served by the server"}
]).

-spec init() -> ok.
init() ->
    ?LOG_INFO("init the scenario"),
    amoc_metrics:init(counters, muc_rooms_created),
    amoc_metrics:init(counters, muc_occupants),
    amoc_metrics:init(counters, muc_messages_sent),
    amoc_metrics:init(counters, muc_messages_received),
    amoc_metrics:init(counters, muc_notifications_received),
    amoc_metrics:init(counters, timeouts),
    amoc_metrics:init(times, response),
    amoc_metrics:init(times, muc_message_ttd),
    ok.

-spec start(amoc_scenario:user_id()) -> any().
start(Id) ->
    {ok, Client, _Spec} = amoc_xmpp:connect_or_exit(Id, extra_user_spec()),
    send_presence_available(Client),
    RoomsToCreate = amoc_xmpp_muc:rooms_to_create(Id, cfg(rooms_per_user), cfg(users_per_room)),
    create_rooms(Client, Id, RoomsToCreate),

    escalus_connection:wait(Client, cfg(delay_before_sending_messages)),

    RoomJids = [room_jid(RoomId) || RoomId <- RoomsToCreate],
    send_messages(Client, my_timetable(RoomJids), 0),

    escalus_connection:wait_forever(Client).

extra_user_spec() ->
    amoc_xmpp:pick_server([[{host, "127.0.0.1"}]]) ++
    [{server, amoc_config:get(mim_host)},
     {sent_stanza_handlers, sent_stanza_handlers()},
     {received_stanza_handlers, received_stanza_handlers()}].

send_presence_available(Client) ->
    Pres = escalus_stanza:presence(<<"available">>),
    Pred = fun(Stanza) ->
                   escalus_pred:is_presence_with_type(<<"available">>, Stanza)
                       andalso escalus_pred:is_stanza_from(Client, Stanza)
           end,
    amoc_xmpp:send_request_and_get_response(Client, Pres, Pred, response, 10000).

create_rooms(_Client, _Id, []) -> ok;
create_rooms(Client, Id, [RoomId | Rest]) ->
    create_room(Client, Id, RoomId),
    escalus_connection:wait(Client, cfg(delay_after_creating_room)),
    create_rooms(Client, Id, Rest).

create_room(Client, CreatorId, RoomId) ->
    MemberIds = amoc_xmpp_muc:room_members(CreatorId, cfg(users_per_room)),
    MemberJids = [amoc_xmpp_users:make_jid(MemberId) || MemberId <- MemberIds],
    Req = stanza_create_room(RoomId, MemberJids),
    amoc_xmpp:send_request_and_get_response(
      Client, Req, fun(Stanza) -> escalus_pred:is_iq_result(Req, Stanza) end, response, 10000),
    amoc_metrics:update_counter(muc_rooms_created).

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

stanza_create_room(RoomId, MemberJids) ->
    ConfigFields = [kv_el(<<"roomname">>, room_name(RoomId))],
    ConfigElement = #xmlel{name = <<"configuration">>, children = ConfigFields},
    UserFields = [user_element(Jid, <<"member">>) || Jid <- MemberJids],
    OccupantsElement = #xmlel{name = <<"occupants">>, children = UserFields},
    escalus_stanza:to(escalus_stanza:iq_set(ns(muc_light_create), [ConfigElement, OccupantsElement]),
                      room_jid(RoomId)).

user_element(Jid, Aff) ->
    #xmlel{name = <<"user">>,
           attrs = #{<<"affiliation">> => Aff},
           children = [#xmlcdata{content = Jid}]}.

kv_el(K, V) ->
    #xmlel{name = K,
           children = [#xmlcdata{content = V}]}.

%% Handlers

sent_stanza_handlers() ->
    amoc_xmpp_handlers:make_stanza_handlers(
      [{fun is_muc_message/1,
        fun() -> amoc_metrics:update_counter(muc_messages_sent) end}]).

received_stanza_handlers() ->
    amoc_xmpp_handlers:make_stanza_handlers(
      [{fun is_muc_notification/1,
        fun() -> amoc_metrics:update_counter(muc_notifications_received) end},
       {fun is_muc_message/1,
        fun(_, Stanza, Metadata) ->
                amoc_metrics:update_counter(muc_messages_received),
                amoc_metrics:update_time(muc_message_ttd, ttd(Stanza, Metadata))
        end},
       {fun(_) -> true end,
        fun(_, Stanza) -> ?LOG_WARNING("Skipping received stanza ~p", [Stanza]) end}]).

%% Predicates

is_muc_notification(Message = #xmlel{name = <<"message">>}) ->
    exml_query:attr(Message, <<"type">>) =:= <<"groupchat">>
        andalso lists:member(exml_query:path(Message, [{element, <<"x">>}, {attr, <<"xmlns">>}]),
                             [ns(muc_light_affiliations),
                              ns(muc_light_configuration)]);
is_muc_notification(_) -> false.

is_muc_message(Stanza = #xmlel{name = <<"message">>}) ->
    exml_query:attr(Stanza, <<"type">>) =:= <<"groupchat">>;
is_muc_message(_) -> false.

%% Helpers

ttd(Stanza, #{recv_timestamp := Recv}) ->
    SentBin = exml_query:path(Stanza, [{element, <<"body">>}, cdata]),
    Recv - binary_to_integer(SentBin).

cfg(Name) ->
    amoc_config:get(Name).

room_jid(RoomId) ->
    <<(room_name(RoomId))/binary, $@, (muc_host())/binary>>.

room_name(RoomId) ->
    <<"room_", (integer_to_binary(RoomId))/binary>>.

muc_host() ->
    <<"muclight.localhost">>.

ns(muc_light_create) -> <<"urn:xmpp:muclight:0#create">>;
ns(muc_light_affiliations) -> <<"urn:xmpp:muclight:0#affiliations">>;
ns(muc_light_configuration) -> <<"urn:xmpp:muclight:0#configuration">>.
