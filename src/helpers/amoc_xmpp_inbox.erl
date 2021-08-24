-module(amoc_xmpp_inbox).

-include_lib("exml/include/exml.hrl").

-define(V(X), (fun amoc_config_validation:X/1)).

-required_variable(
   [#{name => max_items_per_inbox_lookup, default_value => 10, verification => ?V(nonnegative_integer),
      description => "Maximum number of messages returned per inbox lookup"},
    #{name => inbox_lookup_timeout, default_value => 10, verification => ?V(positive_integer),
      description => "Timeout for each inbox lookup request (in seconds)"}
   ]).

-export([init/0, lookup/1, received_handler_spec/0]).

-spec init() -> ok.
init() ->
    amoc_xmpp_presence:init(),
    amoc_metrics:init(counters, inbox_lookups),
    amoc_metrics:init(counters, inbox_messages_received),
    amoc_metrics:init(counters, timeouts),
    amoc_metrics:init(times, inbox_lookup_response_time),
    ok.

-spec lookup(escalus:client()) -> exml:element().
lookup(Client) ->
    Req = escalus_stanza:iq(<<"set">>,
                            [#xmlel{name = <<"inbox">>,
                                    attrs = [{<<"xmlns">>, ns(inbox)}],
                                    children = [rsm_max()]}]),
    Pred = fun(Stanza) -> escalus_pred:is_iq_result(Req, Stanza) end,
    Metric = inbox_lookup_response_time,
    Timeout = cfg(inbox_lookup_timeout),
    amoc_metrics:update_counter(inbox_lookups),
    amoc_xmpp:send_request_and_get_response(Client, Req, Pred, Metric, Timeout).

rsm_max() ->
    Max = cfg(max_items_per_inbox_lookup),
    #xmlel{name = <<"set">>,
           attrs = [{<<"xmlns">>, ns(rsm)}],
           children = [#xmlel{name = <<"max">>,
                              children = [#xmlcdata{content = integer_to_binary(Max)}]}
                      ]}.

-spec received_handler_spec() -> [amoc_xmpp_handlers:handler_spec()].
received_handler_spec() ->
    [{fun(Stanza) -> is_inbox_result(Stanza) end,
      fun() -> amoc_metrics:update_counter(inbox_messages_received) end}].

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

convert(inbox_lookup_timeout, V) -> timer:seconds(V);
convert(_Name, V) -> V.
