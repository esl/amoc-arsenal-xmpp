-module(amoc_xmpp_presence).

-moduledoc """
Presence handling utilities for XMPP scenarios:
  - Sending available/unavailable presence
  - Sending available presence with entity capabilities (XEP-0115),
    advertising notification interest for PEP nodes
  - Responding to server's disco#info requests for capabilities
  - Subscribing to next users' presence
  - Handling and counting incoming presence
  - Acknowledging and counting incoming roster pushes
  - Auto-accepting incoming presence subscription requests
""".

-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("kernel/include/logger.hrl").

-define(V(X), (fun amoc_config_validation:X/1)).
-define(CAPS_NODE, ~"https://github.com/esl/amoc-arsenal-xmpp").
-define(CAPS_HASH_KEY(Nodes), {?MODULE, caps_hash, Nodes}).
-define(CAPS_NODES_KEY(Hash), {?MODULE, caps_nodes, Hash}).
-define(SUBSCRIPTION_BUCKET_SIZE, 1000).

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
      description => "Wait time before sending presence: unavailable (in seconds)"},
    #{name => wait_time_before_subscription,
      default_value => 60,
      verification => ?V(nonnegative_integer),
      description => "Wait time before sending presence subscription requests (in seconds)"},
    #{name => presence_subscriptions_per_user,
      default_value => 0,
      verification => ?V(nonnegative_integer),
      description => "Number of presence subscription requests each user sends to next users"},
    #{name => subscribe_interval,
      default_value => 10,
      verification => ?V(nonnegative_integer),
      description => "Interval (in seconds) between sending presence subscription requests"}
   ]).

-export([init/0,
         start/1,
         stop/1,
         subscribe/2,
         subscribe_to_next_users/2,
         send_presence_available_with_caps/2,
         sent_handler_spec/0,
         received_handler_spec/0]).

-spec init() -> any().
init() ->
    amoc_metrics:init(counters, presences_sent),
    amoc_metrics:init(counters, presences_received),
    amoc_metrics:init(counters, presence_subscriptions),
    amoc_metrics:init(counters, roster_pushes_acknowledged).

-spec start(escalus:client()) -> any().
start(Client) ->
    case cfg(presence_enabled) of
        true -> send_presence_available(Client);
        false -> ok
    end.

-spec stop(escalus:client()) -> any().
stop(Client) ->
    case cfg(presence_enabled) of
        true -> send_presence_unavailable(Client);
        false -> ok
    end,
    escalus_connection:stop(Client).

%% Stanza

-spec send_presence_available(escalus:client()) -> ok.
send_presence_available(Client) ->
    Pres = escalus_stanza:presence(<<"available">>),
    escalus_connection:send(Client, Pres),
    escalus_connection:wait(Client, cfg(wait_time_after_presence_available)).

-spec send_presence_available_with_caps(escalus:client(), [binary()]) -> ok.
send_presence_available_with_caps(Client, Nodes) ->
    Caps = caps(Nodes),
    Pres = escalus_stanza:presence(~"available", [Caps]),
    escalus_connection:send(Client, Pres),
    escalus_connection:wait(Client, cfg(wait_time_after_presence_available)).

-spec send_presence_unavailable(escalus:client()) -> ok.
send_presence_unavailable(Client) ->
    Pres = escalus_stanza:presence(<<"unavailable">>),
    escalus_connection:wait(Client, cfg(wait_time_before_presence_unavailable)),
    escalus_connection:send(Client, Pres).

-spec subscribe(escalus:client(), binary()) -> ok.
subscribe(Client, Jid) ->
    send_direct_presence(Client, Jid, ~"subscribe").

-spec subscribe_to_next_users(escalus:client(), amoc_scenario:user_id()) -> ok.
subscribe_to_next_users(Client, MyId) ->
    case next_user_jids(MyId) of
        [] ->
            ok;
        Jids ->
            escalus_connection:wait(Client, cfg(wait_time_before_subscription)),
            TimeTable = timetable:new(subscribe, length(Jids), cfg(subscribe_interval)),
            timetable:do(Client, fun subscribe_to_next/3, TimeTable, Jids),
            ok
    end.

-spec caps([binary()]) -> exml:element().
caps(Nodes) ->
    Caps = escalus_stanza:caps(~"sha-1", store_caps_mapping(Nodes), v1),
    escalus_stanza:setattr(Caps, ~"node", ?CAPS_NODE).

-spec store_caps_mapping([binary()]) -> binary().
store_caps_mapping(Nodes0) ->
    Nodes = lists:usort(Nodes0),
    maybe
        undefined ?= persistent_term:get(?CAPS_HASH_KEY(Nodes), undefined),
        Hash = caps_hash(Nodes),
        persistent_term:put(?CAPS_HASH_KEY(Nodes), Hash),
        persistent_term:put(?CAPS_NODES_KEY(Hash), Nodes),
        Hash
    end.

-spec caps_hash([binary()]) -> binary().
caps_hash(Nodes) ->
    base64:encode(crypto:hash(sha, caps_verification_string(Nodes))).

-spec caps_verification_string([binary()]) -> binary().
caps_verification_string(Nodes) ->
    Features = lists:usort([?NS_CAPS, ?NS_DISCO_INFO | notify_features(Nodes)]),
    iolist_to_binary([~"client/pc//<", [[Feature, ~"<"] || Feature <- Features]]).

-spec notify_features([binary()]) -> [binary()].
notify_features(Nodes) ->
    [<<Node/binary, "+notify">> || Node <- Nodes].

-spec next_user_jids(amoc_scenario:user_id()) -> [binary()].
next_user_jids(MyId) ->
    NextUserIds = next_user_ids(MyId, cfg(presence_subscriptions_per_user)),
    [dynamic_domains:make_jid(NextUserId) || NextUserId <- NextUserIds].

-doc """
Return next users from a large bucket to keep the subscriptions unidirectional,
avoiding race conditions. Total user count must be divisible by ?SUBSCRIPTION_BUCKET_SIZE.
""".
-spec next_user_ids(amoc_scenario:user_id(), non_neg_integer()) -> [amoc_scenario:user_id()].
next_user_ids(MyId, NextUserCount) when NextUserCount < ?SUBSCRIPTION_BUCKET_SIZE ->
    PositionInBucket = (MyId - 1) rem ?SUBSCRIPTION_BUCKET_SIZE,
    BucketStartId = MyId - PositionInBucket,
    [BucketStartId + ((PositionInBucket + I) rem ?SUBSCRIPTION_BUCKET_SIZE)
     || I <- lists:seq(1, NextUserCount)].

-spec subscribe_to_next(escalus:client(), subscribe, [binary()]) -> [binary()].
subscribe_to_next(Client, subscribe, [Jid | Rest]) ->
    subscribe(Client, Jid),
    Rest.

%% Stanza handlers

-spec sent_handler_spec() -> [amoc_xmpp_handlers:handler_spec()].
sent_handler_spec() ->
    [{fun is_presence_without_error/1,
      fun() -> amoc_metrics:update_counter(presences_sent) end}].

-spec received_handler_spec() -> [amoc_xmpp_handlers:handler_spec()].
received_handler_spec() ->
    [{fun is_caps_disco_info_request/1,
      fun respond_to_caps_disco_info/2},
     {fun escalus_pred:is_roster_set/1,
      fun acknowledge_roster_push/2},
     {fun is_subscribe_presence/1,
      fun accept_subscription/2},
     {fun is_subscribed_presence/1,
      fun record_subscription/0},
     {fun is_presence_without_error/1,
      fun() -> amoc_metrics:update_counter(presences_received) end}].

%% Predicates

-spec is_caps_disco_info_request(exml:element()) -> boolean().
is_caps_disco_info_request(Stanza) ->
    escalus_pred:is_iq_get(Stanza) andalso
        case caps_hash_from_request(Stanza) of
            undefined -> false;
            Hash -> caps_nodes(Hash) =/= undefined
        end.

-spec is_presence_without_error(exml:element()) -> boolean().
is_presence_without_error(Stanza) ->
    escalus_pred:is_presence(Stanza) andalso exml_query:attr(Stanza, <<"type">>) =/= <<"error">>.

-spec is_subscribe_presence(exml:element()) -> boolean().
is_subscribe_presence(Stanza) ->
    is_presence_with_type(~"subscribe", Stanza).

-spec is_subscribed_presence(exml:element()) -> boolean().
is_subscribed_presence(Stanza) ->
    is_presence_with_type(~"subscribed", Stanza).

-spec is_presence_with_type(binary(), exml:element()) -> boolean().
is_presence_with_type(Type, Stanza) ->
    escalus_pred:is_presence(Stanza) andalso exml_query:attr(Stanza, ~"type") =:= Type.

%% Actions

-spec acknowledge_roster_push(escalus:client(), exml:element()) -> ok.
acknowledge_roster_push(Client, Request) ->
    ?LOG_DEBUG("~s acknowledging roster push ~p", [escalus_client:username(Client), Request]),
    amoc_metrics:update_counter(roster_pushes_acknowledged),
    escalus:send(Client, escalus_stanza:iq_result(Request)).

-spec accept_subscription(escalus:client(), exml:element()) -> ok.
accept_subscription(Client, Stanza) ->
    From = exml_query:attr(Stanza, ~"from"),
    ?LOG_DEBUG("~s accepting presence subscription from ~s",
               [escalus_client:username(Client), From]),
    amoc_metrics:update_counter(presences_received),
    send_direct_presence(Client, From, ~"subscribed").

-spec record_subscription() -> ok.
record_subscription() ->
    amoc_metrics:update_counter(presences_received),
    amoc_metrics:update_counter(presence_subscriptions).

-spec respond_to_caps_disco_info(escalus:client(), exml:element()) -> ok.
respond_to_caps_disco_info(Client, Request) ->
    Hash = caps_hash_from_request(Request),
    Nodes = caps_nodes(Hash),
    Query = escalus_stanza:query_el(?NS_DISCO_INFO,
                                    #{~"node" => caps_node(Hash)},
                                    disco_info_children(Nodes)),
    escalus:send(Client, escalus_stanza:iq_result(Request, [Query])).

%% Caps helpers

-spec caps_hash_from_request(exml:element()) -> binary() | undefined.
caps_hash_from_request(Stanza) ->
    case exml_query:path(Stanza, [{element, ~"query"}, {attr, ~"xmlns"}]) of
        ?NS_DISCO_INFO ->
            Node = exml_query:path(Stanza, [{element, ~"query"}, {attr, ~"node"}]),
            caps_hash_from_node(Node);
        _ ->
            undefined
    end.

-spec caps_hash_from_node(binary() | undefined) -> binary() | undefined.
caps_hash_from_node(undefined) ->
    undefined;
caps_hash_from_node(Node) ->
    Prefix = caps_node_prefix(),
    case binary:match(Node, Prefix) of
        {0, PrefixSize} ->
            binary:part(Node, PrefixSize, byte_size(Node) - PrefixSize);
        _ ->
            undefined
    end.

-spec caps_nodes(binary()) -> [binary()] | undefined.
caps_nodes(Hash) ->
    persistent_term:get(?CAPS_NODES_KEY(Hash), undefined).

-spec caps_node(binary()) -> binary().
caps_node(Hash) ->
    <<(?CAPS_NODE)/binary, $#, Hash/binary>>.

-spec caps_node_prefix() -> binary().
caps_node_prefix() ->
    <<(?CAPS_NODE)/binary, $#>>.

-spec disco_info_children([binary()]) -> [exml:element()].
disco_info_children(Nodes) ->
    Features = lists:usort([?NS_CAPS, ?NS_DISCO_INFO | notify_features(Nodes)]),
    [escalus_stanza:identity(~"client", ~"pc", ~"")
     | [escalus_stanza:feature(Feature) || Feature <- Features]].

-spec send_direct_presence(escalus:client(), binary(), binary()) -> ok.
send_direct_presence(Client, Jid, Type) ->
    Pres = escalus_stanza:presence_direct(Jid, Type),
    escalus_connection:send(Client, Pres).

%% Config helpers

cfg(Name) ->
    convert(Name, amoc_config:get(Name)).

convert(wait_time_after_presence_available, Value) -> timer:seconds(Value);
convert(wait_time_before_presence_unavailable, Value) -> timer:seconds(Value);
convert(wait_time_before_subscription, Value) -> timer:seconds(Value);
convert(subscribe_interval, Value) -> timer:seconds(Value);
convert(_Name, Value) -> Value.
