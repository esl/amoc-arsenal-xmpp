-module(dynamic_domains_inbox_lookup).

-include_lib("kernel/include/logger.hrl").
-include_lib("exml/include/exml.hrl").

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
    amoc_metrics:init(counters, inbox_messages_received),
    amoc_metrics:init(counters, inbox_lookups),
    amoc_metrics:init(counters, timeouts),
    amoc_metrics:init(times, inbox_lookup_response_time),
    ok.

%% User helpers

-spec start(amoc_scenario:user_id()) -> any().
start(MyId) ->
    Spec = amoc_xmpp_handlers:make_props(received_handler_spec(), sent_handler_spec()),
    {ok, Client, _} = dynamic_domains:connect_or_exit(MyId, Spec),
    amoc_xmpp_presence:start(Client),
    do(MyId, Client),
    amoc_xmpp_presence:stop(Client).

-spec do(amoc_scenario:user_id(), escalus:client()) -> any().
do(_MyId, Client) ->
    TimeTable = timetable:new(inbox_lookup, cfg(lookup_count), cfg(lookup_interval)),
    timetable:do(Client, fun lookup/2, TimeTable).

lookup(Client, inbox_lookup) ->
    get_inbox(Client).

%% Inbox requests

get_inbox(Client) ->
    Req = escalus_stanza:iq(<<"set">>,
                            [#xmlel{name = <<"inbox">>,
                                    attrs = [{<<"xmlns">>, ns(inbox)}],
                                    children = [rsm_max(10)]}]),
    Pred = fun(Stanza) -> escalus_pred:is_iq_result(Req, Stanza) end,
    Metric = inbox_lookup_response_time,
    Timeout = timer:seconds(10),
    amoc_metrics:update_counter(inbox_lookups),
    amoc_xmpp:send_request_and_get_response(Client, Req, Pred, Metric, Timeout).

rsm_max(Value) ->
    #xmlel{name = <<"set">>,
           attrs = [{<<"xmlns">>, ns(rsm)}],
           children = [#xmlel{name = <<"max">>,
                              children = [#xmlcdata{content = integer_to_binary(Value)}]}
                      ]}.

%% Stanza handlers

sent_handler_spec() ->
    amoc_xmpp_presence:sent_handler_spec().

received_handler_spec() ->
    [{fun(Stanza) -> is_inbox_result(Stanza) end,
      fun() -> amoc_metrics:update_counter(inbox_messages_received) end} |
     amoc_xmpp_presence:received_handler_spec()].

is_inbox_result(Stanza) ->
    NS = exml_query:path(Stanza, [{element, <<"result">>},
                                  {attr, <<"xmlns">>}]),
    M = exml_query:path(Stanza, [{element, <<"result">>},
                                 {element, <<"forwarded">>},
                                 {element, <<"message">>}]),
    ns(inbox) =:= NS andalso (escalus_pred:has_type(<<"chat">>, M) orelse
                              escalus_pred:has_type(<<"groupchat">>, M)).

ns(rsm) -> <<"http://jabber.org/protocol/rsm">>;
ns(inbox) -> <<"erlang-solutions.com:xmpp:inbox:0">>.

%% Config helpers

cfg(Name) ->
    convert(Name, amoc_config:get(Name)).

convert(lookup_interval, V) -> timer:seconds(V);
convert(_Name, V) -> V.
