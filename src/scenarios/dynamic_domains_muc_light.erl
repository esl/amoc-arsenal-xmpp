%% @doc Each user performs the following steps:
%%   - Create the domain dynamically when needed (there is one creator per domain)
%%   - Log in to the dynamically created domain (see dynamic_domains.erl)
%%   - Send presence: available (see amoc_xmpp_presence.erl)
%%   - Create all rooms assigned to this user (there is one creator per room, see amoc_xmpp_muc.erl)
%%   - Send group chat messages to the created rooms periodically
%%   - Send presence: unavailable and disconnect

-module(dynamic_domains_muc_light).

-include_lib("kernel/include/logger.hrl").

-define(V(X), (fun amoc_config_validation:X/1)).

-required_variable(
   [#{name => messages_sent_per_room, default_value => 60, verification => ?V(nonnegative_integer),
      description => "Number of messages sent per room"},
    #{name => room_creation_interval, default_value => 20, verification => ?V(nonnegative_integer),
      description => "Delay between creating consecutive rooms (in seconds)"},
    #{name => room_message_interval, default_value => 10, verification => ?V(nonnegative_integer),
      description => "Interval (in seconds) between sending consecutive messages to each room"},
    #{name => delay_before_creating_rooms, default_value => 60, verification => ?V(nonnegative_integer),
      description => "Delay before creating the first room (in seconds)"},
    #{name => delay_before_sending_messages, default_value => 60, verification => ?V(nonnegative_integer),
      description => "Delay between creating the last room and sending messages (in seconds)"},
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
    MyRooms = amoc_xmpp_muc:rooms_to_create(MyId),
    escalus_connection:wait(Client, cfg(delay_before_creating_rooms)),
    create_rooms(Client, MyId, MyRooms),
    escalus_connection:wait(Client, cfg(delay_before_sending_messages)),
    send_messages(Client, MyId, MyRooms),
    escalus_connection:wait(Client, cfg(delay_after_sending_messages)).

-spec create_rooms(escalus:client(), amoc_scenario:user_id(), [pos_integer()]) -> [].
create_rooms(Client, MyId, MyRooms) ->
    TimeTable = timetable:new(create_room, length(MyRooms), cfg(room_creation_interval)),
    timetable:do(Client, fun(_, create_room, [RoomId | Rest]) ->
                                 create_room(Client, MyId, RoomId),
                                 Rest
                         end, TimeTable, MyRooms).

-spec send_messages(escalus:client(), amoc_scenario:user_id(), [pos_integer()]) -> ok.
send_messages(Client, MyId, MyRooms) ->
    TimeTable = message_timetable(MyId, MyRooms),
    timetable:do(Client, fun send_stanza/2, TimeTable).

-spec message_timetable(amoc_scenario:user_id(), [pos_integer()]) ->
          timetable:timetable(groupchat_event()).
message_timetable(MyId, RoomIdsToSend) ->
    timetable:merge([room_message_timetable(MyId, RoomId) || RoomId <- RoomIdsToSend]).

-spec room_message_timetable(amoc_scenario:user_id(), pos_integer()) ->
          timetable:timetable(groupchat_event()).
room_message_timetable(MyId, RoomId) ->
    RoomJid = room_jid(RoomId, MyId),
    timetable:new({groupchat, RoomJid},
                  cfg(messages_sent_per_room), cfg(room_message_interval)).

%% Room creation

-spec create_room(escalus:client(), amoc_scenario:user_id(), pos_integer()) -> ok.
create_room(Client, MyId, RoomId) ->
    RoomJid = room_jid(RoomId, MyId),
    MemberIds = amoc_xmpp_muc:room_members(MyId),
    MemberJids = [make_jid(MemberId) || MemberId <- MemberIds],
    amoc_xmpp_muc:create_muc_light_room(Client, room_name(RoomId), RoomJid, MemberJids).

%% Groupchat messages

-spec send_stanza(escalus:client(), groupchat_event()) -> ok.
send_stanza(Client, {groupchat, RoomJid}) ->
    amoc_xmpp_muc:send_message_to_room(Client, RoomJid).

room_jid(RoomId, MyId) ->
    amoc_xmpp_muc:muc_light_room_jid(RoomId, dynamic_domains:domain_name(MyId)).

room_name(RoomId) ->
    <<"room_", (integer_to_binary(RoomId))/binary>>.

%% User helpers

-spec make_jid(amoc_scenario:user_id()) -> binary().
make_jid(Id) ->
    amoc_xmpp_users:make_jid(Id, dynamic_domains:domain_name(Id)).

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

convert(room_creation_interval, V) -> timer:seconds(V);
convert(room_message_interval, V) -> timer:seconds(V);
convert(delay_before_creating_rooms, V) -> timer:seconds(V);
convert(delay_before_sending_messages, V) -> timer:seconds(V);
convert(delay_after_sending_messages, V) -> timer:seconds(V);
convert(_Name, V) -> V.
