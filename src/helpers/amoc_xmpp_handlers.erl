-module(amoc_xmpp_handlers).

-include_lib("kernel/include/logger.hrl").
-include_lib("exml/include/exml.hrl").

%% Handler construction
-export([make_props/2]).
-export([stanza_handlers/1]).
-export([stanza_handler/2]).

%% Actions
-export([measure_ttd/3]).
-export([measure_sent_messages/0]).

%% Helpers
-export([ttd/2]).

%% Types
-type handler_spec() :: {escalus_connection:stanza_pred(), action()}.

-type action() :: fun((escalus_connection:client(),
                       exml_stream:element(),
                       escalus_connection:metadata()) -> any())
                | fun((escalus_connection:client(),
                       exml_stream:element()) -> any())
                | fun(() -> any()).

-export_type([handler_spec/0, action/0]).

%% @doc Make a proplist with the received and sent stanza handlers.
%% A catch-all handler is added to the received stanza handlers
%% to avoid silently skipping unexpected stanzas, e.g. errors.
-spec make_props([handler_spec()], [handler_spec()]) -> escalus_users:user_spec().
make_props(RecvSpec, SentSpec) ->
    RecvSpecWithGuard = RecvSpec ++ [{fun match_all/1, fun warn_about_skipped_stanza/2}],
    [{received_stanza_handlers, stanza_handlers(RecvSpecWithGuard)},
     {sent_stanza_handlers, stanza_handlers(SentSpec)}].

%% Handler construction

-spec stanza_handlers([handler_spec()]) -> [escalus_connection:stanza_handler()].
stanza_handlers(Spec) ->
    lists:map(fun stanza_handler/1, Spec).

stanza_handler({Pred, Action}) ->
    stanza_handler(Pred, Action).

-spec stanza_handler(escalus_connection:stanza_pred(), action()) -> escalus_connection:stanza_handler().
stanza_handler(Pred, Action) ->
    fun(Client, Stanza, Metadata) ->
            case Pred(Stanza) of
                true ->
                    perform_action(Action, Client, Stanza, Metadata),
                    true;
                false ->
                    false
            end
    end.

perform_action(Action, Client, Stanza, Metadata) when is_function(Action, 3) ->
    Action(Client, Stanza, Metadata);
perform_action(Action, Client, Stanza, _Metadata) when is_function(Action, 2) ->
    Action(Client, Stanza);
perform_action(Action, _Client, _Stanza, _Metadata) when is_function(Action, 0) ->
    Action().

%% Predicates

match_all(_) ->
    true.

%% Actions

-spec measure_ttd(escalus_connection:client(),
                  exml_stream:element(),
                  escalus_connection:metadata()) -> any().
measure_ttd(_Client, Stanza, Metadata) ->
    case exml_query:subelement(Stanza, <<"delay">>) of
        undefined -> amoc_metrics:update_time(message_ttd, ttd(Stanza, Metadata));
        _ -> ok
    end.

-spec warn_about_skipped_stanza(escalus_connection:client(), exml_stream:element()) -> any().
warn_about_skipped_stanza(_Client, Stanza) ->
    ?LOG_WARNING("Skipping received stanza ~p", [Stanza]).

-spec measure_sent_messages() -> any().
measure_sent_messages() ->
    amoc_metrics:update_counter(messages_sent).

%% Helpers

-spec ttd(exml_stream:element(), escalus_connection:metadata()) -> integer().
ttd(#xmlel{attrs = Attrs}, #{recv_timestamp := RecvTimestamp}) ->
    {_, SentBin} = lists:keyfind(<<"timestamp">>, 1, Attrs),
    RecvTimestamp - binary_to_integer(SentBin).
