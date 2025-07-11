-module(amoc_xmpp_presence).

-define(V(X), (fun amoc_config_validation:X/1)).

-required_variable(
   [#{name => presence_enabled,
      default_value => true,
      verification => ?V(boolean),
      description => "Enable or disable presences"},
    #{name => wait_time_after_presence_available,
      default_value => 0,
      verification => ?V(nonnegative_integer),
      description => "Wait time after sending presence: available (in seconds)"},
    #{name => wait_time_before_presence_unavailable,
      default_value => 0,
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
    case cfg(presence_enabled) of
        true ->
            send_presence_available(Client),
            escalus_connection:wait(Client, cfg(wait_time_after_presence_available));
        false ->
            ok
    end.

-spec stop(escalus:client()) -> any().
stop(Client) ->
    case cfg(presence_enabled) of
        true ->
            escalus_connection:wait(Client, cfg(wait_time_before_presence_unavailable)),
            send_presence_unavailable(Client);
        false ->
            ok
    end,
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
    [{fun is_presence_without_error/1,
      fun() -> amoc_metrics:update_counter(presences_sent) end}].

-spec received_handler_spec() -> [amoc_xmpp_handlers:handler_spec()].
received_handler_spec() ->
    [{fun is_presence_without_error/1,
      fun() -> amoc_metrics:update_counter(presences_received) end}].

%% Predicates

-spec is_presence_without_error(exmle:element()) -> boolean().
is_presence_without_error(Stanza) ->
    escalus_pred:is_presence(Stanza) andalso exml_query:attr(Stanza, <<"type">>) =/= <<"error">>.

%% Config helpers

cfg(Name) ->
    convert(Name, amoc_config:get(Name)).

convert(wait_time_after_presence_available, Value) -> timer:seconds(Value);
convert(wait_time_before_presence_unavailable, Value) -> timer:seconds(Value);
convert(_Name, Value) -> Value.
