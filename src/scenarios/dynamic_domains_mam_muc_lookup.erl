-module(dynamic_domains_mam_muc_lookup).

-include_lib("kernel/include/logger.hrl").

-define(V(X), (fun amoc_config_validation:X/1)).

-required_variable(
   [#{name => lookups_per_room, default_value => 60, verification => ?V(nonnegative_integer),
      description => "Number of MAM lookups per room"},
    #{name => room_lookup_interval, default_value => 10, verification => ?V(nonnegative_integer),
      description => "Interval (in seconds) between lookups for each room"}
   ]).

-behaviour(amoc_scenario).

-export([init/0, start/1]).

-spec init() -> ok.
init() ->
    ?LOG_INFO("init metrics"),
    amoc_xmpp_presence:init(),
    amoc_xmpp_mam:init(),
    ok.

-record(state, {last_mam_ids = #{}}).

-spec start(amoc_scenario:user_id()) -> any().
start(MyId) ->
    Spec = amoc_xmpp_handlers:make_props(received_handler_spec(), sent_handler_spec()),
    {ok, Client, _} = dynamic_domains:connect_or_exit(MyId, Spec),
    amoc_xmpp_presence:start(Client),
    do(MyId, Client),
    amoc_xmpp_presence:stop(Client).

-spec do(amoc_scenario:user_id(), escalus:client()) -> any().
do(MyId, Client) ->
    RoomIds = amoc_xmpp_muc:rooms_to_create(MyId, cfg(rooms_per_user), cfg(users_per_room)),
    TimeTable = lookup_timetable(MyId, RoomIds),
    timetable:do(Client, fun send_stanza/3, TimeTable, #state{}).

lookup_timetable(MyId, RoomIds) ->
    timetable:merge([room_lookup_timetable(MyId, RoomId) || RoomId <- RoomIds]).

room_lookup_timetable(MyId, RoomId) ->
    RoomJid = room_jid(RoomId, dynamic_domains:domain_name(MyId)),
    timetable:new({mam_lookup, RoomJid},
                  cfg(lookups_per_room), cfg(room_lookup_interval)).

send_stanza(Client, {mam_lookup, RoomJid}, State = #state{last_mam_ids = Ids}) ->
    Id = maps:get(RoomJid, Ids, none),
    NewId = amoc_xmpp_mam:get_mam_messages(Client, #{last_id => Id, jid => RoomJid}),
    State#state{last_mam_ids = Ids#{RoomJid => NewId}}.

%% Room helpers

room_jid(RoomId, Domain) ->
    <<(room_name(RoomId))/binary, $@, (muc_host(Domain))/binary>>.

room_name(RoomId) ->
    <<"room_", (integer_to_binary(RoomId))/binary>>.

muc_host(Domain) ->
    <<"muclight.", Domain/binary>>.

%% Stanza handlers

sent_handler_spec() ->
    amoc_xmpp_presence:sent_handler_spec().

received_handler_spec() ->
    amoc_xmpp_mam:received_handler_spec(groupchat) ++ amoc_xmpp_presence:received_handler_spec().

%% Config helpers

cfg(Name) ->
    convert(Name, amoc_config:get(Name)).

convert(room_lookup_interval, V) -> timer:seconds(V);
convert(_Name, V) -> V.
