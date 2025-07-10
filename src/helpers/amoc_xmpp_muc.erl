%%% @doc Building blocks for groupchat scenarios (MUC or MUC Light).
-module(amoc_xmpp_muc).

-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("kernel/include/logger.hrl").

%%% Common API
-export([init/0,
         sent_handler_spec/0,
         received_handler_spec/0,
         send_message_to_room/2]).

%%% MUC Light API
-export([create_muc_light_room/4,
         rooms_to_create/1, rooms_to_create/3,
         room_members/1, room_members/2,
         muc_light_room_jid/2]).

%%% MUC API
-export([enter_muc_room/2,
         rooms_to_join/1, rooms_to_join/3,
         muc_room_jid/2]).

%%% Debug API
-export([print_rooms_to_join/3,
         print_rooms_to_create/3]).

-define(V(X), (fun amoc_config_validation:X/1)).

-type room_id() :: pos_integer().

-required_variable(
   [#{name => rooms_per_user, default_value => 10, verification => ?V(nonnegative_integer),
      description => "Number of rooms each user belongs to"},
    #{name => users_per_room, default_value => 5, verification => ?V(nonnegative_integer),
      description => "Number of users each room has"},
    #{name => muc_light_prefix, default_value => <<"muclight">>, verification => ?V(binary),
      description => "Subdomain prefix for MUC Light"},
    #{name => muc_prefix, default_value => <<"conference">>, verification => ?V(binary),
      description => "Subdomain prefix for MUC"}
   ]).

-spec init() -> ok.
init() ->
    amoc_metrics:init(counters, muc_rooms_created),
    amoc_metrics:init(counters, muc_occupants),
    amoc_metrics:init(counters, muc_messages_sent),
    amoc_metrics:init(counters, muc_messages_received),
    amoc_metrics:init(counters, muc_presences_received),
    amoc_metrics:init(counters, muc_notifications_received),
    amoc_metrics:init(times, response),
    ok.

-spec sent_handler_spec() -> [amoc_xmpp_handlers:handler_spec()].
sent_handler_spec() ->
    [{fun is_muc_message/1,
      fun(Client, Stanza) ->
              ?LOG_DEBUG("~s sent message ~p", [escalus_client:username(Client), Stanza]),
              amoc_metrics:update_counter(muc_messages_sent)
      end}].

-spec received_handler_spec() -> [amoc_xmpp_handlers:handler_spec()].
received_handler_spec() ->
    [{fun is_muc_notification/1,
      fun(Client, Stanza) ->
              ?LOG_DEBUG("~s received MUC notification ~p", [escalus_client:username(Client), Stanza]),
              amoc_metrics:update_counter(muc_notifications_received)
      end},
     {fun is_muc_message/1,
      fun(Client, Stanza, Metadata) ->
              ?LOG_DEBUG("~s received MUC message ~p", [escalus_client:username(Client), Stanza]),
              amoc_metrics:update_counter(muc_messages_received),
              amoc_xmpp_handlers:measure_ttd(Client, Stanza, Metadata)
      end}].

is_muc_notification(Stanza) ->
    is_muc_message(Stanza)
        andalso lists:member(exml_query:path(Stanza, [{element, <<"body">>}, cdata]), [<<>>, undefined])
        andalso (has_muc_subject(Stanza) orelse has_muc_light_notification(Stanza)).

has_muc_subject(Stanza) ->
    exml_query:subelement(Stanza, <<"subject">>) =/= undefined.

has_muc_light_notification(Stanza) ->
    lists:member(exml_query:path(Stanza, [{element, <<"x">>}, {attr, <<"xmlns">>}]),
                [ns(muc_light_affiliations), ns(muc_light_configuration)]).

-spec is_muc_message(exml:element()) -> boolean().
is_muc_message(Stanza = #xmlel{name = <<"message">>}) ->
    exml_query:attr(Stanza, <<"type">>) =:= <<"groupchat">>;
is_muc_message(_) -> false.

-spec send_message_to_room(escalus:client(), jid:jid()) -> ok.
send_message_to_room(Client, RoomJid) ->
    Timestamp = integer_to_binary(os:system_time(microsecond)),
    Message = #xmlel{attrs = Attrs} = escalus_stanza:groupchat_to(RoomJid, <<"Hello">>),
    escalus:send(Client, Message#xmlel{attrs = Attrs#{<<"timestamp">> => Timestamp}}).

%%% MUC Light: Room creation

-spec create_muc_light_room(escalus:client(), binary(), binary(), [binary()]) -> ok.
create_muc_light_room(Client, RoomName, RoomJid, MemberJids) ->
    Req = muc_light_stanza_create_room(RoomName, RoomJid, MemberJids),
    Pred = fun(Stanza) -> escalus_pred:is_iq_result(Req, Stanza) end,
    amoc_xmpp:send_request_and_get_response(Client, Req, Pred, response, 10000),
    %% TODO check response
    amoc_metrics:update_counter(muc_occupants, length(MemberJids) + 1),
    amoc_metrics:update_counter(muc_rooms_created).

-spec muc_light_stanza_create_room(binary(), binary(), [binary()]) -> exml:element().
muc_light_stanza_create_room(RoomName, RoomJid, MemberJids) ->
    ConfigFields = [kv_el(<<"roomname">>, RoomName)],
    ConfigElement = #xmlel{name = <<"configuration">>, children = ConfigFields},
    UserFields = [user_element(Jid, <<"member">>) || Jid <- MemberJids],
    OccupantsElement = #xmlel{name = <<"occupants">>, children = UserFields},
    escalus_stanza:to(escalus_stanza:iq_set(ns(muc_light_create), [ConfigElement, OccupantsElement]),
                      RoomJid).

user_element(Jid, Aff) ->
    #xmlel{name = <<"user">>,
           attrs = #{<<"affiliation">> => Aff},
           children = [#xmlcdata{content = Jid}]}.

kv_el(K, V) ->
    #xmlel{name = K,
           children = [#xmlcdata{content = V}]}.

ns(muc_light_create) -> <<"urn:xmpp:muclight:0#create">>.

%%% MUC: Room entry and creation

-spec enter_muc_room(escalus:client(), jid:jid()) -> ok.
enter_muc_room(Client, RoomJid) ->
    Nick = escalus_client:username(Client),
    RoomFullJid = muc_room_full_jid(RoomJid, Nick),
    Req = muc_stanza_enter_room(RoomFullJid),
    Resp = amoc_xmpp:send_request_and_get_response(
             Client, Req, fun(Stanza) -> is_muc_presence_resp(Req, Stanza) end, response, 10000),
    amoc_metrics:update_counter(muc_presences_received),
    case room_entry_response_type(Resp) of
        created ->
            amoc_metrics:update_counter(muc_rooms_created),
            amoc_metrics:update_counter(muc_occupants),
            ?LOG_INFO("~s created room ~s", [Nick, RoomJid]),
            configure_instant_room(Client, Nick, RoomJid);
        joined ->
            amoc_metrics:update_counter(muc_occupants),
            ?LOG_INFO("~s joined room ~s", [Nick, RoomJid])
    end.

muc_room_full_jid(RoomJid, Nick) ->
    <<RoomJid/binary, $/, Nick/binary>>.

-spec muc_stanza_enter_room(jid:jid()) -> exml:element().
muc_stanza_enter_room(RoomJid) ->
    X = #xmlel{name = <<"x">>, attrs = #{<<"xmlns">> => ?NS_MUC}},
    Presence = escalus_stanza:to(escalus_stanza:presence(<<"available">>), RoomJid),
    Presence#xmlel{children = [X]}.

-spec is_muc_presence_resp(exml:element(), exml:element()) -> boolean().
is_muc_presence_resp(Req, Resp = #xmlel{name = <<"presence">>}) ->
    is_muc_presence(Resp) andalso
        escalus_utils:jid_to_lower(exml_query:attr(Req, <<"to">>)) =:=
        escalus_utils:jid_to_lower(exml_query:attr(Resp, <<"from">>));
is_muc_presence_resp(_, _) -> false.

-spec is_muc_presence(exml:element()) -> boolean().
is_muc_presence(Stanza) ->
    exml_query:path(Stanza, [{element, <<"x">>}, {attr, <<"xmlns">>}]) =:= ?NS_MUC_USER.

-spec room_entry_response_type(exml:element()) -> created | joined | locked.
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

configure_instant_room(Client, Nick, RoomJid) ->
    Req = stanza_configure_instant_room(RoomJid),
    Resp = amoc_xmpp:send_request_and_get_response(
             Client, Req, fun(Stanza) -> escalus_pred:is_iq_result(Req, Stanza) end,
             response, 10000),
    ?LOG_DEBUG("~s got room creation response ~p", [Nick, Resp]),
    ?LOG_INFO("~s configured instant room ~s", [Nick, RoomJid]).

stanza_configure_instant_room(RoomJid) ->
    EmptyForm = escalus_stanza:x_data_form(<<"submit">>, []),
    Stanza = escalus_stanza:iq_set(?NS_MUC_OWNER, [EmptyForm]),
    escalus_stanza:to(Stanza, RoomJid).

%%% Room distribution by buckets

-spec rooms_to_join(amoc_scenario:user_id()) -> [room_id()].
rooms_to_join(UserId) ->
    rooms_to_join(UserId, cfg(rooms_per_user), cfg(users_per_room)).

%% @doc Returns the IDs of rooms joined by the specified user, intended for MUC.
%% The order is important for equal distribution of created rooms
-spec rooms_to_join(amoc_scenario:user_id(), pos_integer(), pos_integer()) -> [room_id()].
rooms_to_join(UserId, RoomsPerUser, UsersPerRoom) ->
    BasicRoom = round_id((UserId - 1) * RoomsPerUser / UsersPerRoom, RoomsPerUser > UsersPerRoom),
    RoomBucketPos = BasicRoom rem RoomsPerUser,
    RoomBucketStartId = BasicRoom - RoomBucketPos + 1,
    lists:seq(BasicRoom + 1, RoomBucketStartId + RoomsPerUser - 1)
        ++ lists:seq(RoomBucketStartId, BasicRoom).

-spec rooms_to_create(amoc_scenario:user_id()) -> [room_id()].
rooms_to_create(UserId) ->
    rooms_to_create(UserId, cfg(rooms_per_user), cfg(users_per_room)).

%% @doc Returns the IDs of rooms created by the specified user, intended for MUC Light.
-spec rooms_to_create(amoc_scenario:user_id(), pos_integer(), pos_integer()) -> [room_id()].
rooms_to_create(UserId, RoomsPerUser, UsersPerRoom) ->
    MyRooms = rooms_to_join(UserId, RoomsPerUser, UsersPerRoom),
    lists:filter(fun(R) -> creator(R, RoomsPerUser, UsersPerRoom) =:= UserId end, MyRooms).

-spec room_members(amoc_scenario:user_id()) -> [amoc_scenario:user_id()].
room_members(CreatorId) ->
    room_members(CreatorId, cfg(users_per_room)).

%% @doc Returns the list of member IDs for any room created by the specified user,
%% intended for MUC Light.
%% The list does not contain the creator ID
-spec room_members(amoc_scenario:user_id(), pos_integer()) -> [amoc_scenario:user_id()].
room_members(CreatorId, UsersPerRoom) ->
    Occupants = bucket_ids(CreatorId, UsersPerRoom),
    lists:delete(CreatorId, Occupants).

creator(RoomId, RoomsPerUser, UsersPerRoom) ->
    round_id((RoomId - 1) * UsersPerRoom / RoomsPerUser + 1, RoomsPerUser =< UsersPerRoom).

round_id(FloatId, RoundUp) when FloatId >= 0 ->
    IntId = trunc(FloatId),
    case FloatId > IntId andalso RoundUp of
        true -> IntId + 1;
        false -> IntId
    end.

bucket_ids(Id, BucketSize) ->
    Position = (Id - 1) rem BucketSize,
    BucketStartId = Id - Position,
    lists:seq(BucketStartId, BucketStartId + BucketSize - 1).

muc_light_room_jid(RoomId, Domain) ->
    <<(room_name(RoomId))/binary, $@, (cfg(muc_light_prefix))/binary, $., Domain/binary>>.

muc_room_jid(RoomId, Domain) ->
    <<(room_name(RoomId))/binary, $@, (cfg(muc_prefix))/binary, $., Domain/binary>>.

room_name(RoomId) ->
    <<"room_", (integer_to_binary(RoomId))/binary>>.

%%% Debug - print user distribution in rooms

%% @doc Print a matrix specifying the relation between users and rooms.
%%   Each row means one user, each column means one room.
%%   For each user: the first room to join is shown as 'x'
%%                  other rooms to join are shown as '.'
%%
%% Example: amoc_xmpp_muc:print_rooms_to_join(6, 3, 2).
%%```
%% x..         <-- User 1 joins rooms 1, 2, 3
%% ..x         <-- User 2 joins rooms 3, 1, 2
%%    x..      <-- User 3 joins rooms 4, 5, 6
%%    ..x          (...)
%%       x..
%%       ..x
%% '''
-spec print_rooms_to_join(pos_integer(), pos_integer(), pos_integer()) -> ok.
print_rooms_to_join(UserCount, RoomsPerUser, UsersPerRoom) ->
    [print_my_rooms_to_join(UserId, RoomsPerUser, UsersPerRoom)
     || UserId <- lists:seq(1, UserCount)],
    ok.

print_my_rooms_to_join(UserId, RoomsPerUser, UsersPerRoom) ->
    RoomsToJoin = rooms_to_join(UserId, RoomsPerUser, UsersPerRoom),
    Tagged = lists:zip(RoomsToJoin, lists:seq(1, RoomsPerUser)),
    [{Lowest, _} | _] = Sorted = lists:sort(Tagged),
    io:put_chars(lists:duplicate(Lowest-1, $ )),
    io:put_chars([room_char(R) || {_, R} <- Sorted]),
    io:nl().

room_char(1) -> $x;
room_char(R) when R > 1 -> $..

%% @doc Print a matrix specifying the relation between users and rooms.
%%   Each row means one user, each column means one room.
%%   For each room: the creator is shown as 'x'
%%                  other members are shown as '.'
%%
%% Example: amoc_xmpp_muc:print_rooms_to_create(6, 3, 2).
%% ```
%% xx.       <-- User 1 creates rooms 1, 2
%% ..x       <-- User 2 creates room 3
%%    xx.    <-- User 3 creates rooms 4, 5
%%    ..x        (...)
%%       xx.
%%       ..x
%% '''
-spec print_rooms_to_create(pos_integer(), pos_integer(), pos_integer()) -> ok.
print_rooms_to_create(UserCount, RoomsPerUser, UsersPerRoom) ->
    AllRoomUsers = all_room_users(UserCount, RoomsPerUser, UsersPerRoom),
    [print_my_rooms_to_create(UserId, AllRoomUsers) || UserId <- lists:seq(1, UserCount)],
    ok.

all_room_users(UserCount, RoomsPerUser, UsersPerRoom) ->
    [room_column(RoomId, RoomsPerUser, UsersPerRoom)
     || RoomId <- all_rooms(UserCount, RoomsPerUser, UsersPerRoom)].

all_rooms(UserCount, RoomsPerUser, UsersPerRoom) ->
    lists:flatmap(fun(UserId) -> rooms_to_create(UserId, RoomsPerUser, UsersPerRoom) end,
                  lists:seq(1, UserCount)).

room_column(RoomId, RoomsPerUser, UsersPerRoom) ->
    CreatorId = creator(RoomId, RoomsPerUser, UsersPerRoom),
    Members = room_members(CreatorId, UsersPerRoom),
    {CreatorId, Members}.

print_my_rooms_to_create(UserId, AllRoomUsers) ->
    io:put_chars([room_to_create_char(UserId, CreatorId, Members)
                  || {CreatorId, Members} <- AllRoomUsers]),
    io:nl().

room_to_create_char(UserId, UserId, _) -> $x;
room_to_create_char(UserId, _, Members) ->
    case lists:member(UserId, Members) of
        true -> $.;
        false -> ($ )
    end.

cfg(Name) -> amoc_config:get(Name).

