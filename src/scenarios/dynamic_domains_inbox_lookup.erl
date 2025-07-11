%% @doc Each user performs the following steps:
%%   - Log in to a dynamically created domain (see dynamic_domains.erl)
%%   - Send presence: available (see amoc_xmpp_presence.erl)
%%   - Send requests for Inbox periodically
%%     (see the config variables for this module and for 'amoc_xmpp_inbox')
%%   - Send presence: unavailable and disconnect
%%
%% Prerequisites:
%%   - Dynamic domains already created
%%   - For non-empty inbox results: messages in Inbox
%% You can generate both by running either the 'dynamic_domains_pm'
%% or the 'dynamic_domains_mu_light' scenario before this one for at least as many users.

-module(dynamic_domains_inbox_lookup).

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
    amoc_xmpp_ping:init(),
    amoc_xmpp_inbox:init(),
    ok.

%% User helpers

-spec start(amoc_scenario:user_id()) -> any().
start(MyId) ->
    Spec = amoc_xmpp_handlers:make_props(received_handler_spec(), sent_handler_spec()),
    {ok, Client, _} = dynamic_domains:connect_or_exit(MyId, Spec, #{create_domain => false}),
    amoc_xmpp_presence:start(Client),
    do(MyId, Client),
    amoc_xmpp_presence:stop(Client).

-spec do(amoc_scenario:user_id(), escalus:client()) -> any().
do(_MyId, Client) ->
    TimeTable = timetable:new(inbox_lookup, cfg(lookup_count), cfg(lookup_interval)),
    timetable:do(Client, fun lookup/2, TimeTable).

-spec lookup(escalus:client(), inbox_lookup) -> any().
lookup(Client, inbox_lookup) ->
    amoc_xmpp_inbox:lookup(Client).

%% Stanza handlers

sent_handler_spec() ->
    amoc_xmpp_presence:sent_handler_spec() ++ amoc_xmpp_ping:sent_handler_spec().

received_handler_spec() ->
    amoc_xmpp_inbox:received_handler_spec() ++ amoc_xmpp_presence:received_handler_spec() ++
        amoc_xmpp_ping:received_handler_spec().

%% Config helpers

cfg(Name) ->
    convert(Name, amoc_config:get(Name)).

convert(lookup_interval, V) -> timer:seconds(V);
convert(_Name, V) -> V.
