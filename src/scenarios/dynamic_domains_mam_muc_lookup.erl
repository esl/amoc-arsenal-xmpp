%% @doc Each user performs the following steps:
%%   - Log in to a dynamically created domain (see dynamic_domains.erl)
%%   - Send presence: available (see amoc_xmpp_presence.erl)
%%   - For each room assigned to this user (there is one sender per room),
%%     send requests for MAM archive periodically, paging through results.
%%     When the next page would be incomplete, start again from the beginning.
%%     (see the config variables for this module, amoc_xmpp_mam.erl and amoc_xmpp_muc.erl)
%%   - Send presence: unavailable and disconnect
%%
%% Prerequisites:
%%   - Dynamic domains already created
%%   - MUC Light rooms already created
%%   - For non-empty MAM results: messages in the MAM MUC archive
%% You can generate both by running the 'dynamic_domains_muc_light' scenario before this one
%% for the same room configuration and at least as many users.

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

-type lookup_event() :: {mam_lookup, RoomJid :: binary()}.

-spec init() -> ok.
init() ->
    ?LOG_INFO("init metrics"),
    dynamic_domains:init(),
    amoc_xmpp_presence:init(),
    amoc_xmpp_ping:init(),
    amoc_xmpp_mam:init(),
    ok.

-record(state, {last_mam_ids = #{} :: map()}).

-spec start(amoc_scenario:user_id()) -> any().
start(MyId) ->
    Spec = amoc_xmpp_handlers:make_props(received_handler_spec(), sent_handler_spec()),
    {ok, Client, _} = dynamic_domains:connect_or_exit(MyId, Spec, #{create_domain => false}),
    amoc_xmpp_presence:start(Client),
    do(MyId, Client),
    amoc_xmpp_presence:stop(Client).

-spec do(amoc_scenario:user_id(), escalus:client()) -> any().
do(MyId, Client) ->
    RoomIds = amoc_xmpp_muc:rooms_to_create(MyId, cfg(rooms_per_user), cfg(users_per_room)),
    TimeTable = lookup_timetable(MyId, RoomIds),
    timetable:do(Client, fun send_stanza/3, TimeTable, #state{}).

-spec lookup_timetable(amoc_scenario:user_id(), [pos_integer()]) ->
          timetable:timetable(lookup_event()).
lookup_timetable(MyId, RoomIds) ->
    timetable:merge([room_lookup_timetable(MyId, RoomId) || RoomId <- RoomIds]).

-spec room_lookup_timetable(amoc_scenario:user_id(), pos_integer()) ->
          timetable:timetable(lookup_event()).
room_lookup_timetable(MyId, RoomId) ->
    RoomJid = room_jid(RoomId, dynamic_domains:domain_name(MyId)),
    timetable:new({mam_lookup, RoomJid},
                  cfg(lookups_per_room), cfg(room_lookup_interval)).

-spec send_stanza(escalus:client(), lookup_event(), #state{}) -> #state{}.
send_stanza(Client, {mam_lookup, RoomJid}, State = #state{last_mam_ids = Ids}) ->
    Id = maps:get(RoomJid, Ids, none),
    NewId = amoc_xmpp_mam:lookup(Client, #{last_id => Id, jid => RoomJid}),
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
    amoc_xmpp_presence:sent_handler_spec() ++ amoc_xmpp_ping:sent_handler_spec().

received_handler_spec() ->
    amoc_xmpp_mam:received_handler_spec(groupchat) ++ amoc_xmpp_presence:received_handler_spec() ++
        amoc_xmpp_ping:received_handler_spec().

%% Config helpers

cfg(Name) ->
    convert(Name, amoc_config:get(Name)).

convert(room_lookup_interval, V) -> timer:seconds(V);
convert(_Name, V) -> V.
