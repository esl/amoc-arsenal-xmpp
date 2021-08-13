-module(dynamic_domains_mam_lookup).

-include_lib("kernel/include/logger.hrl").

-define(V(X), (fun amoc_config_validation:X/1)).

-required_variable(
   [#{name => lookup_count, default_value => 60, verification => ?V(positive_integer),
      description => "Number of lookups performed by each user"},
    #{name => lookup_interval, default_value => 10, verification => ?V(nonnegative_integer),
      description => "Interval (in seconds) between lookups"}
   ]).

-behaviour(amoc_scenario).

-export([init/0, start/1]).

-spec init() -> ok.
init() ->
    ?LOG_INFO("init metrics"),
    dynamic_domains:init(),
    amoc_xmpp_presence:init(),
    amoc_xmpp_mam:init(),
    ok.

%% User helpers

-spec start(amoc_scenario:user_id()) -> any().
start(MyId) ->
    Spec = amoc_xmpp_handlers:make_props(received_handler_spec(), sent_handler_spec()),
    {ok, Client, _} = dynamic_domains:connect_or_exit(MyId, Spec, #{create_domain => false}),
    amoc_xmpp_presence:start(Client),
    do(MyId, Client),
    amoc_xmpp_presence:stop(Client).

-record(state, {last_mam_id = none :: amoc_xmpp_mam:last_id()}).

-spec do(amoc_scenario:user_id(), escalus:client()) -> any().
do(_MyId, Client) ->
    TimeTable = timetable:new(mam_lookup, cfg(lookup_count), cfg(lookup_interval)),
    timetable:do(Client, fun send_stanza/3, TimeTable, #state{}).

-spec send_stanza(escalus:client(), mam_lookup, #state{}) -> #state{}.
send_stanza(Client, mam_lookup, State = #state{last_mam_id = Id}) ->
    NewId = amoc_xmpp_mam:lookup(Client, #{last_id => Id}),
    State#state{last_mam_id = NewId}.

%% Stanza handlers

sent_handler_spec() ->
    amoc_xmpp_presence:sent_handler_spec().

received_handler_spec() ->
    amoc_xmpp_mam:received_handler_spec(chat) ++ amoc_xmpp_presence:received_handler_spec().

%% Config helpers

cfg(Name) ->
    convert(Name, amoc_config:get(Name)).

convert(lookup_interval, V) -> timer:seconds(V);
convert(_Name, V) -> V.
