-module(dynamic_domains_pep).

-moduledoc """
Each user performs the following steps:
- Create the domain dynamically when needed (there is one creator per domain)
- Log in to the static/dynamic domain (see dynamic_domains.erl)
- Send presence: available with entity capabilities (see amoc_xmpp_presence.erl)
- Subscribe to next users' presence in order to receive notifications from their nodes
  (see amoc_xmpp_presence.erl for details and variables)
- For each owned node, periodically publish items
- Send presence: unavailable and disconnect

See the configuration variables for intervals/delays/item counts.
During the test, incoming PEP item notifications are received, counted, and measured
(see amoc_xmpp_pep.erl)
""".

-include_lib("kernel/include/logger.hrl").

-define(V(X), (fun amoc_config_validation:X/1)).

-required_variable(
   [#{name => items_published_per_node, default_value => 60, verification => ?V(nonnegative_integer),
      description => "Number of items published for each node"},
    #{name => publish_interval, default_value => 10, verification => ?V(nonnegative_integer),
      description => "Interval (in seconds) between publishing consecutive items to each node"},
    #{name => delay_before_publishing, default_value => 60, verification => ?V(nonnegative_integer),
      description => "Delay before publishing the first item (in seconds)"},
    #{name => delay_after_publishing, default_value => 60, verification => ?V(nonnegative_integer),
      description => "Delay after publishing the last item (in seconds)"}
   ]).

-behaviour(amoc_scenario).

-export([init/0, start/1]).

-type pep_event() :: {publish_item, binary()}.

-spec init() -> ok.
init() ->
    ?LOG_INFO("init metrics"),
    dynamic_domains:init(),
    amoc_xmpp_presence:init(),
    amoc_xmpp_pep:init(),
    ok.

-spec start(amoc_scenario:user_id()) -> any().
start(MyId) ->
    Spec = amoc_xmpp_handlers:make_props(received_handler_spec(), sent_handler_spec()),
    {ok, Client, _} = dynamic_domains:connect_or_exit(MyId, Spec),
    Nodes = amoc_xmpp_pep:nodes_to_publish(),
    start_presence(MyId, Client, Nodes),
    do(MyId, Client, Nodes),
    amoc_xmpp_presence:stop(Client).

-spec start_presence(amoc_scenario:user_id(), escalus:client(), [binary()]) -> ok.
start_presence(MyId, Client, Nodes) ->
    amoc_xmpp_presence:send_presence_available_with_caps(Client, Nodes),
    amoc_xmpp_presence:subscribe_to_next_users(Client, MyId).

-spec do(amoc_scenario:user_id(), escalus:client(), [binary()]) -> ok.
do(_MyId, Client, Nodes) ->
    escalus_connection:wait(Client, cfg(delay_before_publishing)),
    publish_items(Client, Nodes),
    escalus_connection:wait(Client, cfg(delay_after_publishing)).

-spec publish_items(escalus:client(), [binary()]) -> ok.
publish_items(Client, Nodes) ->
    timetable:do(Client, fun publish_stanza/2, publish_timetable(Nodes)).

-spec publish_timetable([binary()]) -> timetable:timetable(pep_event()).
publish_timetable(Nodes) ->
    timetable:merge([node_publish_timetable(Node) || Node <- Nodes]).

-spec node_publish_timetable(binary()) -> timetable:timetable(pep_event()).
node_publish_timetable(Node) ->
    timetable:new({publish_item, Node}, cfg(items_published_per_node), cfg(publish_interval)).

-spec publish_stanza(escalus:client(), pep_event()) -> ok.
publish_stanza(Client, {publish_item, Node}) ->
    amoc_xmpp_pep:publish_item(Client, Node).

%% Stanza handlers

-spec sent_handler_spec() -> [amoc_xmpp_handlers:handler_spec()].
sent_handler_spec() ->
    amoc_xmpp_presence:sent_handler_spec() ++
        amoc_xmpp_ping:sent_handler_spec().

-spec received_handler_spec() -> [amoc_xmpp_handlers:handler_spec()].
received_handler_spec() ->
    amoc_xmpp_pep:received_handler_spec() ++
        amoc_xmpp_presence:received_handler_spec() ++
        amoc_xmpp_ping:received_handler_spec().

%% Config helpers

cfg(Name) ->
    convert(Name, amoc_config:get(Name)).

convert(delay_before_publishing, V) -> timer:seconds(V);
convert(delay_after_publishing, V) -> timer:seconds(V);
convert(publish_interval, V) -> timer:seconds(V);
convert(_Name, V) -> V.
