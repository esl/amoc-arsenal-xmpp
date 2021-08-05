-module(amoc_xmpp_presence).

-define(V(X), (fun amoc_config_validation:X/1)).

-required_variable(
   [#{name => wait_time_before_scenario, default_value => 60, verification => ?V(nonnegative_integer),
      description => "Wait time before scenario after connecting (in seconds)"},
    #{name => wait_time_after_scenario, default_value => 60, verification => ?V(nonnegative_integer),
      description => "Wait time after scenario before disconnecting (in seconds)"}
   ]).

-export([init/0,
         start/1,
         stop/1,
         sent_handler_spec/0,
         received_handler_spec/0]).

init() ->
    amoc_metrics:init(counters, presences_sent),
    amoc_metrics:init(counters, presences_received).

start(Client) ->
    send_presence_available(Client),
    escalus_connection:wait(Client, cfg(wait_time_before_scenario)).

stop(Client) ->
    escalus_connection:wait(Client, cfg(wait_time_after_scenario)),
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

sent_handler_spec() ->
    [{fun escalus_pred:is_presence/1,
      fun() -> amoc_metrics:update_counter(presences_sent) end}].

received_handler_spec() ->
    [{fun escalus_pred:is_presence/1,
      fun() -> amoc_metrics:update_counter(presences_received) end}].

%% Config helpers

cfg(Name) ->
    timer:seconds(amoc_config:get(Name)).
