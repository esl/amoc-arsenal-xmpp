-module(amoc_xmpp_mam).

-include_lib("exml/include/exml.hrl").

-define(V(X), (fun amoc_config_validation:X/1)).

-required_variable(
   [#{name => max_items_per_lookup, default_value => 5, verification => ?V(nonnegative_integer),
      description => "Maximum number of messages returned per MAM lookup"},
    #{name => mam_lookup_timeout, default_value => 10, verification => ?V(positive_integer),
      description => "Timeout for each MAM lookup request (in seconds)"}
   ]).

-export([init/0, lookup/2, received_handler_spec/1]).

-type last_id() :: none | binary().
-type lookup_opts() :: #{last_id => last_id(), jid => binary()}.

-export_type([last_id/0, lookup_opts/0]).

-spec init() -> ok.
init() ->
    amoc_xmpp_presence:init(),
    amoc_metrics:init(counters, mam_lookups),
    amoc_metrics:init(counters, mam_messages_received),
    amoc_metrics:init(counters, timeouts),
    amoc_metrics:init(times, mam_lookup_response_time),
    ok.

-spec lookup(escalus:client(), lookup_opts()) -> last_id().
lookup(Client, Opts) ->
    Req = mam_request(Opts),
    Pred = fun(Stanza) -> escalus_pred:is_iq_result(Req, Stanza) end,
    Metric = mam_lookup_response_time,
    Timeout = cfg(mam_lookup_timeout),
    amoc_metrics:update_counter(mam_lookups),
    Response = amoc_xmpp:send_request_and_get_response(Client, Req, Pred, Metric, Timeout),
    get_last_id(Response).

-spec mam_request(lookup_opts()) -> exml:element().
mam_request(Opts) ->
    Max = cfg(max_items_per_lookup),
    SetChild = #xmlel{name = <<"set">>,
                      attrs = [{<<"xmlns">>, <<"http://jabber.org/protocol/rsm">>}],
                      children = [#xmlel{name = <<"max">>,
                                         children = [#xmlcdata{content = integer_to_binary(Max)}]}
                                 | after_elements(Opts)]},
    Req = escalus_stanza:iq(<<"set">>,
                            [#xmlel{name = <<"query">>,
                                    attrs = [{<<"xmlns">>,  <<"urn:xmpp:mam:2">>}],
                                    children = [SetChild]}]),
    case Opts of
        #{jid := Jid} -> escalus_stanza:to(Req, Jid);
        #{} -> Req
    end.

-spec after_elements(lookup_opts()) -> [exml:element()].
after_elements(#{last_id := LastId}) when LastId =/= none ->
    [#xmlel{name = <<"after">>, children = [#xmlcdata{content = LastId}]}];
after_elements(#{}) -> [].

%% Wrap around if the next result set would be incomplete
-spec get_last_id(exml:element()) -> last_id().
get_last_id(Stanza) ->
    [SetEl] = exml_query:paths(Stanza, [{element, <<"fin">>}, {element, <<"set">>}]),
    [FirstIndex] = exml_query:paths(SetEl, [{element, <<"first">>}, {attr, <<"index">>}]),
    [CountBin] = exml_query:paths(SetEl, [{element, <<"count">>}, cdata]),
    Count = binary_to_integer(CountBin),
    case binary_to_integer(FirstIndex) + 2*cfg(max_items_per_lookup) =< Count of
        true -> exml_query:path(SetEl, [{element, <<"last">>}, cdata], none);
        false -> none
    end.

-spec received_handler_spec(chat | groupchat) -> [amoc_xmpp_handlers:handler_spec()].
received_handler_spec(Type) ->
    [{fun(M) -> is_mam_archived_message(M, Type) end,
      fun() -> amoc_metrics:update_counter(mam_messages_received) end}].

%% TODO rework escalus_pred to include these checks
-spec is_mam_archived_message(exml:element(), chat | groupchat) -> boolean().
is_mam_archived_message(#xmlel{name = <<"message">>} = Stanza, Type) ->
    M = exml_query:path(Stanza, [{element, <<"result">>},
                                 {element, <<"forwarded">>},
                                 {element, <<"message">>}]),
    BinType = atom_to_binary(Type),
    escalus_pred:is_message(M) andalso escalus_pred:has_type(BinType, M);
is_mam_archived_message(_Stanza, _Type) ->
    false.

%% Config helpers

cfg(Name) ->
    convert(Name, amoc_config:get(Name)).

convert(mam_lookup_timeout, V) -> timer:seconds(V);
convert(_Name, V) -> V.
