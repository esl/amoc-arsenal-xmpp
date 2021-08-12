-module(dynamic_domains_muc_light).

-include_lib("kernel/include/logger.hrl").
-include_lib("exml/include/exml.hrl").

-define(V(X), (fun amoc_config_validation:X/1)).

-required_variable(
   [#{name => messages_sent_per_room, default_value => 60, verification => ?V(nonnegative_integer),
      description => "Number of messages sent per room"},
    #{name => room_creation_interval, default_value => 20, verification => ?V(nonnegative_integer),
      description => "Delay between creating consecutive rooms (in seconds)"},
    #{name => room_message_interval, default_value => 10, verification => ?V(nonnegative_integer),
      description => "Interval (in seconds) between sending consecutive messages to each room"},
    #{name => delay_before_sending_messages, default_value => 60, verification => ?V(nonnegative_integer),
      description => "Delay between creating the last room and sending messages (in seconds)"}
   ]).

-behaviour(amoc_scenario).

-export([init/0, start/1]).

-spec init() -> ok.
init() ->
    ?LOG_INFO("init metrics"),
    dynamic_domains:init(),
    amoc_xmpp_presence:init(),
    amoc_metrics:init(counters, muc_rooms_created),
    amoc_metrics:init(counters, muc_occupants),
    amoc_metrics:init(counters, muc_messages_sent),
    amoc_metrics:init(counters, muc_messages_received),
    amoc_metrics:init(counters, muc_notifications_received),
    amoc_metrics:init(times, room_creation_response_time),
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
    MyRooms = amoc_xmpp_muc:rooms_to_create(MyId),
    create_rooms(Client, MyId, MyRooms),
    escalus_connection:wait(Client, cfg(delay_before_sending_messages)),
    send_messages(Client, MyId, MyRooms).

create_rooms(Client, MyId, MyRooms) ->
    TimeTable = timetable:new(create_room, length(MyRooms), cfg(room_creation_interval)),
    timetable:do(Client, fun(_, create_room, [RoomId | Rest]) ->
                                 create_room(Client, MyId, RoomId),
                                 Rest
                         end, TimeTable, MyRooms).

send_messages(Client, MyId, MyRooms) ->
    TimeTable = message_timetable(MyId, MyRooms),
    timetable:do(Client, fun send_stanza/2, TimeTable).

message_timetable(MyId, RoomIdsToSend) ->
    timetable:merge([room_message_timetable(MyId, RoomId) || RoomId <- RoomIdsToSend]).

room_message_timetable(MyId, RoomId) ->
    RoomJid = room_jid(RoomId, dynamic_domains:domain_name(MyId)),
    timetable:new({groupchat, RoomJid},
                  cfg(messages_sent_per_room), cfg(room_message_interval)).

%% Room creation

create_room(Client, MyId, RoomId) ->
    MemberIds = amoc_xmpp_muc:room_members(MyId),
    MemberJids = [make_jid(MemberId) || MemberId <- MemberIds],
    RoomJid = room_jid(RoomId, dynamic_domains:domain_name(MyId)),
    Req = stanza_create_room(room_name(RoomId), RoomJid, MemberJids),
    Pred = fun(Stanza) -> escalus_pred:is_iq_result(Req, Stanza) end,
    TimeMetric = room_creation_response_time,
    amoc_xmpp:send_request_and_get_response(Client, Req, Pred, TimeMetric, 10000),
    %% TODO check response
    amoc_metrics:update_counter(muc_rooms_created).

stanza_create_room(RoomName, RoomJid, MemberJids) ->
    ConfigFields = [kv_el(<<"roomname">>, RoomName)],
    ConfigElement = #xmlel{name = <<"configuration">>, children = ConfigFields},
    UserFields = [user_element(Jid, <<"member">>) || Jid <- MemberJids],
    OccupantsElement = #xmlel{name = <<"occupants">>, children = UserFields},
    escalus_stanza:to(escalus_stanza:iq_set(ns(muc_light_create), [ConfigElement, OccupantsElement]),
                      RoomJid).

user_element(Jid, Aff) ->
    #xmlel{name = <<"user">>,
           attrs = [{<<"affiliation">>, Aff}],
           children = [#xmlcdata{content = Jid}]}.

kv_el(K, V) ->
    #xmlel{name = K,
           children = [#xmlcdata{content = V}]}.

%% Groupchat messages

send_stanza(Client, {groupchat, RoomJid}) ->
    escalus_connection:send(Client, message_to_room(RoomJid)).

message_to_room(RoomJid) ->
    Timestamp = integer_to_binary(os:system_time(microsecond)),
    escalus_stanza:groupchat_to(RoomJid, Timestamp).

room_jid(RoomId, Domain) ->
    <<(room_name(RoomId))/binary, $@, (muc_host(Domain))/binary>>.

room_name(RoomId) ->
    <<"room_", (integer_to_binary(RoomId))/binary>>.

muc_host(Domain) ->
    <<"muclight.", Domain/binary>>.

ns(muc_light_create) -> <<"urn:xmpp:muclight:0#create">>;
ns(muc_light_affiliations) -> <<"urn:xmpp:muclight:0#affiliations">>;
ns(muc_light_configuration) -> <<"urn:xmpp:muclight:0#configuration">>.

ttd(Stanza, #{recv_timestamp := Recv}) ->
    SentBin = exml_query:path(Stanza, [{element, <<"body">>}, cdata]),
    Recv - binary_to_integer(SentBin).

%% User helpers

-spec make_jid(amoc_scenario:user_id()) -> binary().
make_jid(Id) ->
    amoc_xmpp_users:make_jid(Id, dynamic_domains:domain_name(Id)).

%% Stanza handlers

sent_handler_spec() ->
    [{fun is_muc_message/1,
      fun() -> amoc_metrics:update_counter(muc_messages_sent) end} |
     amoc_xmpp_presence:sent_handler_spec()].

received_handler_spec() ->
    [{fun is_muc_notification/1,
      fun() -> amoc_metrics:update_counter(muc_notifications_received) end},
     {fun is_muc_message/1,
      fun(_, Stanza, Metadata) ->
              amoc_metrics:update_counter(muc_messages_received),
              amoc_metrics:update_time(message_ttd, ttd(Stanza, Metadata))
      end} |
     amoc_xmpp_presence:received_handler_spec()].

is_muc_notification(Message = #xmlel{name = <<"message">>}) ->
    exml_query:attr(Message, <<"type">>) =:= <<"groupchat">>
        andalso lists:member(exml_query:path(Message, [{element, <<"x">>}, {attr, <<"xmlns">>}]),
                             [ns(muc_light_affiliations),
                              ns(muc_light_configuration)]);
is_muc_notification(_) -> false.

is_muc_message(Stanza = #xmlel{name = <<"message">>}) ->
    exml_query:attr(Stanza, <<"type">>) =:= <<"groupchat">>;
is_muc_message(_) -> false.

%% Config helpers

cfg(Name) ->
    convert(Name, amoc_config:get(Name)).

convert(room_creation_interval, V) -> timer:seconds(V);
convert(room_message_interval, V) -> timer:seconds(V);
convert(delay_before_sending_messages, V) -> timer:seconds(V);
convert(_Name, V) -> V.
