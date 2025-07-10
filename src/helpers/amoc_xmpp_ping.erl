%%% @doc This module handles server ping requests (XEP-0199, section 4.1)
-module(amoc_xmpp_ping).

-define(V(X), (fun amoc_config_validation:X/1)).

-include_lib("escalus/include/escalus_xmlns.hrl").

-required_variable(
   [#{name => ping_enabled,
      default_value => true,
      verification => ?V(boolean),
      description => "Enables responding to ping requests from the server"}
   ]).

-export([init/0,
         sent_handler_spec/0,
         received_handler_spec/0]).

%%% API

-spec init() -> ok.
init() ->
    amoc_metrics:init(counters, iq_ping_sent),
    amoc_metrics:init(counters, iq_ping_received),
    ok.

-spec sent_handler_spec() -> [amoc_xmpp_handlers:handler_spec()].
sent_handler_spec() ->
    [{fun(Stanza) -> escalus_pred:is_iq_result(Stanza) andalso is_ping(Stanza) end,
      fun() -> amoc_metrics:update_counter(iq_ping_sent) end}].

-spec received_handler_spec() -> [amoc_xmpp_handlers:handler_spec()].
received_handler_spec() ->
    [{fun(Stanza) -> escalus_pred:is_iq_get(Stanza) andalso is_ping(Stanza) end,
      fun respond_to_ping/2}].

%%% Helpers

-spec is_ping(exml:element()) -> boolean().
is_ping(Stanza) ->
    ?NS_PING =:= exml_query:path(Stanza, [{element, <<"ping">>}, {attr, <<"xmlns">>}]).

-spec respond_to_ping(escalus:client(), exml:element()) -> ok.
respond_to_ping(Client, Request) ->
    amoc_metrics:update_counter(iq_ping_received),
    escalus:send(Client, escalus_stanza:iq_result(Request)).
