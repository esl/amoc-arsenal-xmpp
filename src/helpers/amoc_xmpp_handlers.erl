-module(amoc_xmpp_handlers).

-include_lib("kernel/include/logger.hrl").
-include_lib("exml/include/exml.hrl").

%% Handler construction
-export([make_props/2]).
-export([make_stanza_handlers/1]).
-export([make_stanza_handler/2]).

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
                | fun((exml_stream:element()) -> any())
                | fun(() -> any()).

-export_type([handler_spec/0, action/0]).

%% @doc Make a proplist with the received and sent stanza handlers.
%% A catch-all handler is added to the received stanza handlers
%% to avoid silently skipping unexpected stanzas, e.g. errors.
-spec make_props([handler_spec()], [handler_spec()]) -> escalus_users:user_spec().
make_props(RecvSpec, SentSpec) ->
    RecvSpecWithGuard = RecvSpec ++ [{fun match_all/1, fun warn_about_skipped_stanza/2}],
    [{received_stanza_handlers, make_stanza_handlers(RecvSpecWithGuard)},
     {sent_stanza_handlers, make_stanza_handlers(SentSpec)}].

%% Handler construction

-spec make_stanza_handlers([handler_spec()]) -> [escalus_connection:stanza_handler()].
make_stanza_handlers(Spec) ->
    lists:map(fun make_stanza_handler/1, Spec).

make_stanza_handler({Pred, Action}) ->
    make_stanza_handler(Pred, Action).

make_stanza_handler(Pred, Action) ->
    check_pred(Pred),
    check_action(Action),
    make_stanza_handler_fn(Pred, Action).

check_pred(Pred) when is_function(Pred, 1) ->
    ok;
check_pred(InvalidPred) ->
    error({invalid_stanza_predicate, InvalidPred}).

check_action(Action) when is_function(Action) ->
    {arity, Arity} = erlang:fun_info(Action, arity),
    if
        Arity > 3 -> error({invalid_stanza_action, Action});
        true -> ok
    end;
check_action(InvalidAction) ->
    error({invalid_stanza_action, InvalidAction}).

-spec make_stanza_handler_fn(escalus_connection:stanza_pred(), action()) ->
    escalus_connection:stanza_handler().
make_stanza_handler_fn(Pred, Action) ->
    fun(Client, Stanza, Metadata) ->
        case check_predicate(Pred, Stanza) of
            true ->
                perform_action(Action, Client, Stanza, Metadata),
                %% even if action crashes mark the message as processed one
                true;
            false ->
                false
        end
    end.

check_predicate(Pred, Stanza) when is_function(Pred, 1) ->
    case apply_safely(Pred, [Stanza]) of
        {ok, BoolRet} when is_boolean(BoolRet) ->
            BoolRet;
        {ok, InvalidRet} ->
            ?LOG_ERROR("predicate function returns invalid value~n\tPredFN = ~p,~n\t"
                       "Ret = ~p,~n\tStanza = ~p", [Pred, InvalidRet, Stanza]),
            false;
        {error, Error} ->
            ?LOG_ERROR("predicate function crashed~n\tPredFN = ~p,~n\t"
                       "Stanza = ~p,~n\tError = ~p", [Pred, Stanza, Error]),
            false
    end.

perform_action(Action, Client, Stanza, Metadata) ->
    Args = if
               is_function(Action, 3) -> [Client, Stanza, Metadata];
               is_function(Action, 2) -> [Client, Stanza];
               is_function(Action, 1) -> [Stanza];
               is_function(Action, 0) -> []
           end,
    Ret = apply_safely(Action, Args),
    case Ret of
        {ok, _} ->
            ok;
        {error, Error} ->
            ?LOG_ERROR("action function crashed~n\tActionFN = ~p,~n\t"
                       "Args = ~p,~n\tError = ~p", [Action, Args, Error])
    end,
    Ret.

-spec apply_safely(function(), [term()]) -> {ok | error, term()}.
apply_safely(F, A) ->
    try erlang:apply(F, A) of
        Result -> {ok, Result}
    catch
        Class:Exception:Stacktrace ->
            {error, {Class, Exception, Stacktrace}}
    end.

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
