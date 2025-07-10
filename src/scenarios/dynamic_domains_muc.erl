%% @doc Each user performs the following steps:
%%   - Create the domain dynamically when needed (there is one creator per domain)
%%   - Log in to the dynamically created domain (see dynamic_domains.erl)
%%   - Send presence: available (see amoc_xmpp_presence.erl)
%%   - Enter all rooms assigned to this user, creating non-existing ones
%%     (conflict between users is avoided, see amoc_xmpp_muc.erl for details)
%%   - Send group chat messages to the rooms periodically
%%   - Send presence: unavailable and disconnect

-module(dynamic_domains_muc).

-include_lib("kernel/include/logger.hrl").

-define(V(X), (fun amoc_config_validation:X/1)).

-required_variable(
   [#{name => messages_sent_per_room, default_value => 60, verification => ?V(nonnegative_integer),
      description => "Number of messages sent per room"},
    #{name => room_entry_interval, default_value => 20, verification => ?V(nonnegative_integer),
      description => "Delay between entering consecutive rooms (in seconds)"},
    #{name => room_message_interval, default_value => 10, verification => ?V(nonnegative_integer),
      description => "Interval (in seconds) between sending consecutive messages to each room"},
    #{name => delay_before_entering_rooms, default_value => 60, verification => ?V(nonnegative_integer),
      description => "Delay before entering the first room (in seconds)"},
    #{name => delay_before_sending_messages, default_value => 60, verification => ?V(nonnegative_integer),
      description => "Delay between entering the last room and sending messages (in seconds)"},
    #{name => delay_after_sending_messages, default_value => 60, verification => ?V(nonnegative_integer),
      description => "Delay after sending messages (in seconds)"}
   ]).

-behaviour(amoc_scenario).

-export([init/0, start/1]).

-type groupchat_event() :: {groupchat, RoomJid :: binary()}.

-spec init() -> ok.
init() ->
    ?LOG_INFO("init metrics"),
    dynamic_domains:init(),
    amoc_xmpp_presence:init(),
    amoc_xmpp_ping:init(),
    amoc_xmpp_muc:init(),
    amoc_metrics:init(counters, timeouts),
    amoc_metrics:init(times, response),
    amoc_metrics:init(times, message_ttd),
    ok.

-spec start(amoc_scenario:user_id()) -> any().
start(MyId) ->
    Spec = amoc_xmpp_handlers:make_props(received_handler_spec(), sent_handler_spec()),
    {ok, Client, _} = dynamic_domains:connect_or_exit(MyId, Spec),
    amoc_xmpp_presence:start(Client),
    do(MyId, Client),
    amoc_xmpp_presence:stop(Client).

-spec do(amoc_scenario:user_id(), escalus:client()) -> any().
do(MyId, Client) ->
    escalus_connection:wait(Client, cfg(delay_before_entering_rooms)),
    RoomsToJoin = amoc_xmpp_muc:rooms_to_join(MyId),
    enter_rooms(Client, MyId, RoomsToJoin),

    escalus_connection:wait(Client, cfg(delay_before_sending_messages)),
    %% 'rooms_to_create/3' assigns one creator to each room
    %% In this case it is used to assign one sender to each room
    RoomIdsToSend = amoc_xmpp_muc:rooms_to_create(MyId),
    send_messages(Client, MyId, RoomIdsToSend),
    escalus_connection:wait(Client, cfg(delay_after_sending_messages)).

-spec enter_rooms(escalus:client(), amoc_scenario:user_id(), [pos_integer()]) -> [].
enter_rooms(Client, MyId, MyRooms) ->
    TimeTable = timetable:new(enter_room, length(MyRooms), cfg(room_entry_interval)),
    timetable:do(Client, fun(_, enter_room, [RoomId | Rest]) ->
                                 enter_room(Client, MyId, RoomId),
                                 Rest
                         end, TimeTable, MyRooms).

-spec enter_room(escalus:client(), amoc_scenario:user_id(), amoc_xmpp_muc:room_id()) -> ok.
enter_room(Client, MyId, RoomId) ->
    amoc_xmpp_muc:enter_muc_room(Client, room_jid(RoomId, MyId)).

-spec send_messages(escalus:client(), amoc_scenario:user_id(), [amoc_xmpp_muc:room_id()]) -> ok.
send_messages(Client, MyId, MyRooms) ->
    TimeTable = message_timetable(MyId, MyRooms),
    timetable:do(Client, fun send_stanza/2, TimeTable).

-spec message_timetable(amoc_scenario:user_id(), [amoc_xmpp_muc:room_id()]) ->
          timetable:timetable(groupchat_event()).
message_timetable(MyId, RoomIdsToSend) ->
    timetable:merge([room_message_timetable(MyId, RoomId) || RoomId <- RoomIdsToSend]).

-spec room_message_timetable(amoc_scenario:user_id(), amoc_xmpp_muc:room_id()) ->
          timetable:timetable(groupchat_event()).
room_message_timetable(MyId, RoomId) ->
    timetable:new({groupchat, room_jid(RoomId, MyId)},
                  cfg(messages_sent_per_room), cfg(room_message_interval)).

%% Groupchat messages

-spec send_stanza(escalus:client(), groupchat_event()) -> ok.
send_stanza(Client, {groupchat, RoomJid}) ->
    amoc_xmpp_muc:send_message_to_room(Client, RoomJid).

room_jid(RoomId, MyId) ->
    amoc_xmpp_muc:muc_room_jid(RoomId, dynamic_domains:domain_name(MyId)).

%% Stanza handlers

-spec sent_handler_spec() -> [amoc_xmpp_handlers:handler_spec()].
sent_handler_spec() ->
    amoc_xmpp_muc:sent_handler_spec() ++
    amoc_xmpp_presence:sent_handler_spec() ++
    amoc_xmpp_ping:sent_handler_spec().

-spec received_handler_spec() -> [amoc_xmpp_handlers:handler_spec()].
received_handler_spec() ->
    amoc_xmpp_muc:received_handler_spec() ++
    amoc_xmpp_presence:received_handler_spec() ++
    amoc_xmpp_ping:received_handler_spec().

%% Config helpers

cfg(Name) ->
    convert(Name, amoc_config:get(Name)).

convert(room_entry_interval, V) -> timer:seconds(V);
convert(room_message_interval, V) -> timer:seconds(V);
convert(delay_before_entering_rooms, V) -> timer:seconds(V);
convert(delay_before_sending_messages, V) -> timer:seconds(V);
convert(delay_after_sending_messages, V) -> timer:seconds(V);
convert(_Name, V) -> V.
