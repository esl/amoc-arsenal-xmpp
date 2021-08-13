-module(amoc_xmpp_presence).

-define(V(X), (fun amoc_config_validation:X/1)).

-required_variable(
   [#{name => wait_time_after_presence_available,
      default_value => 60,
      verification => ?V(nonnegative_integer),
      description => "Wait time after sending presence: available (in seconds)"},
    #{name => wait_time_before_presence_unavailable,
      default_value => 60,
      verification => ?V(nonnegative_integer),
      description => "Wait time before sending presence: unavailable (in seconds)"}
   ]).

-export([init/0,
         start/1,
         stop/1,
         sent_handler_spec/0,
         received_handler_spec/0]).

-spec init() -> any().
init() ->
    amoc_metrics:init(counters, presences_sent),
    amoc_metrics:init(counters, presences_received).

-spec start(escalus:client()) -> any().
start(Client) ->
    send_presence_available(Client),
    escalus_connection:wait(Client, cfg(wait_time_after_presence_available)).

-spec stop(escalus:client()) -> any().
stop(Client) ->
    escalus_connection:wait(Client, cfg(wait_time_before_presence_unavailable)),
    send_presence_unavailable(Client),
    escalus_connection:stop(Client).

%% Stanza

-spec send_presence_available(escalus:client()) -> ok.
send_presence_available(Client) ->
    Pres = escalus_stanza:presence(<<"available">>),
    escalus_connection:send(Client, Pres).

-spec send_presence_unavailable(escalus:client()) -> ok.
send_presence_unavailable(Client) ->
    Pres = escalus_stanza:presence(<<"unavailable">>),
    escalus_connection:send(Client, Pres).

%% Stanza handlers

-spec sent_handler_spec() -> [amoc_xmpp_handlers:handler_spec()].
sent_handler_spec() ->
    [{fun escalus_pred:is_presence/1,
      fun() -> amoc_metrics:update_counter(presences_sent) end}].

-spec received_handler_spec() -> [amoc_xmpp_handlers:handler_spec()].
received_handler_spec() ->
    [{fun escalus_pred:is_presence/1,
      fun() -> amoc_metrics:update_counter(presences_received) end}].

%% Config helpers

cfg(Name) ->
    timer:seconds(amoc_config:get(Name)).
