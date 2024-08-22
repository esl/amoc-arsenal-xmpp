%==============================================================================
%% @copyright 2019-2020 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%% @end
%%
%% @doc
%% In this scenario, users are creating PEP nodes and publishing items.
%% Users are publishing items to their PEP nodes and receiving items from other
%% users' nodes. Each node has a number of subscribers limited by the
%% `n_of_subscribers' variable. Publishing can start depending on the
%% `node_activation_policy' variable, either after `all_nodes' or after `n_nodes'
%% are subscribed to. Interactions between users and pubsub PEP nodes are managed
%% by the `amoc_coordinator'. Additional subscription and publication delay can
%% be introduced with use of the `coordinator_delay' variable. This can help to
%% moderate the load when users are being added.
%%
%% == User steps: ==
%%
%% 1. Connect to the XMPP host given by the `mim_host' variable.
%%
%% 2. Create a PEP node and send presence `available', in order to receive
%%    messages from the PEP nodes. The rate of node creation is limited by the
%%    `node_creation_rate' per minute. Node creation results in a timeout when
%%    `iq_timeout' is exceeded.
%%
%% 3. Add user to the `amoc_coordinator' and pass client data.
%%
%% 4. Wait for the following messages in a loop:
%%
%% - {stanza, MessageStanza} - process message stanza, check if it contains the
%%   user's own jid. If it does, schedule a `publish_item' message. The rate of
%%   these messages is handled by `amoc_throttle' and depends on the
%%   `publication_rate' variable.
%%
%% - {stanza, IqStanza} - process an `iq' stanza and update corresponding metrics.
%%
%% - {stanza, PresenceStanza} - respond to the `subscribe' presence stanzas.
%%
%% - publish_item - message from `amoc_throttle' that was scheduled after a
%%   message stanza was received. Send a message, which size is defined by the
%%   `publication_size' variable, to the PEP node. The rate of these `publish_item_node'
%%   messages is handled by `amoc_throttle' and depends on the `node_publication_rate'
%%   variable.
%%
%% 5. Continue execution of the `user_loop'.
%%
%% == Metrics exposed by this scenario: ==
%%
%%   === Counters: ===
%%     ==== node_creation ====
%%       - node_creation has following counter metrics exposed: `request', `response',
%%         `timeout', `response result', `response error', `timeout result',
%%         `timeout error'
%%     ==== publication ====
%%       - publication has following counter metrics exposed: `request', `response',
%%         `timeout', `response result', `response error', `timeout result',
%%         `timeout error'
%%     ==== message ====
%%       - message - incremented with every received message stanza.
%%
%%  === Times: ===
%%   - node_creation - time for the pubsub node to be created
%%
%%   - publication - time to publish pubsub item
%%
%%   - message_tdd - message time to delivery
%%
%% @end
%%==============================================================================
-module(pubsub_pep).

-behaviour(amoc_scenario).

-include_lib("exml/include/exml.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("kernel/include/logger.hrl").

-define(V(X), fun amoc_config_validation:X/1).

-required_variable([
    #{name => iq_timeout, default_value => 10000, verification => ?V(positive_integer),
      description => "IQ timeout (milliseconds, def: 10000ms)"},
    #{name => coordinator_delay, default_value => 0, verification => ?V(nonnegative_integer),
      description => "Delay after N subscriptions (milliseconds, def: 0ms)"},
    #{name => node_creation_rate, default_value => 600, verification => ?V(positive_integer),
      description => "Rate of node creations (per minute, def:600)"},
    #{name => publication_size, default_value => 300, verification => ?V(nonnegative_integer),
      description => "Size of additional payload (bytes, def:300)"},
    #{name => publication_rate, default_value => 1500, verification => ?V(positive_integer),
      description => "Rate of publications (per minute, def:1500)"},
    #{name => n_of_subscribers, default_value => 50, verification => ?V(nonnegative_integer),
      description => "Number of subscriptions for each node (def: 50)"},
    #{name => activation_policy, default_value => all_nodes, verification => [all_nodes, n_nodes],
      description => "Publish after subscribtion of (def: all_nodes | n_nodes)"},
    #{name => mim_host, default_value => <<"localhost">>, verification => ?V(binary),
      description => "The virtual host served by the server (def: <<\"localhost\">>)"}
]).

-define(PEP_NODE_NS, <<"just_some_random_namespace">>).
-define(CAPS_HASH, <<"erNmVoMSwRBR4brUU/inYQ5NFr0=">>). %% mod_caps:make_disco_hash(feature_elems(), sha1).
-define(NODE, {pep, ?PEP_NODE_NS}).

-define(GROUP_NAME, <<"pubsub_simple_coordinator">>).
-define(NODE_CREATION_THROTTLING, node_creation).
-define(PUBLICATION_THROTTLING, publication).

-define(COORDINATOR_TIMEOUT, 100).

-export([init/0, start/1]).

-spec init() -> ok.
init() ->
    init_metrics(),
    {ok, PublicationRate} = amoc_config:get(publication_rate),
    {ok, NodeCreationRate} = amoc_config:get(node_creation_rate),

    amoc_throttle:start(?NODE_CREATION_THROTTLING, NodeCreationRate),
    amoc_throttle:start(?PUBLICATION_THROTTLING, PublicationRate),
    start_coordinator(),
    ok.

-spec start(amoc_scenario:user_id()) -> any().
start(Id) ->
    Client = connect_amoc_user(Id),
    start_user(Client).

init_metrics() ->
    iq_metrics:start([node_creation, publication]),
    Counters = [message],
    Times = [message_ttd],
    [amoc_metrics:init(counters, Metric) || Metric <- Counters],
    [amoc_metrics:init(times, Metric) || Metric <- Times].


%%------------------------------------------------------------------------------------------------
%% Coordinator
%%------------------------------------------------------------------------------------------------
start_coordinator() ->
    amoc_coordinator:start(?MODULE, get_coordination_plan(), ?COORDINATOR_TIMEOUT).

get_coordination_plan() ->
    N = get_no_of_node_subscribers(),

    [{N, [fun make_clients_friends/3,
          users_activation(n_nodes),
          coordination_delay()]},
     {all, users_activation(all_nodes)}].

coordination_delay() ->
    Delay = amoc_config:get(coordinator_delay),
    fun({coordinate, _}) -> timer:sleep(Delay);
        (_) -> ok
    end.

make_clients_friends(_, _, undefined) -> ok;
make_clients_friends(_, {_, C1}, {_, C2}) ->
    send_presence(C1, <<"subscribe">>, C2),
    send_presence(C2, <<"subscribe">>, C1).

users_activation(ActivationPolicy) ->
    case amoc_config:get(activation_policy) of
        ActivationPolicy ->
            fun(_, CoordinationData) ->
                [schedule_publishing(Pid) || {Pid, _} <- CoordinationData]
            end;
        _ -> fun(_) -> ok end
    end.
%%------------------------------------------------------------------------------------------------
%% User
%%------------------------------------------------------------------------------------------------
start_user(Client) ->
    ?LOG_DEBUG("user process ~p", [self()]),
    create_new_node(Client),
    erlang:monitor(process, Client#client.rcv_pid),
    escalus_tcp:set_active(Client#client.rcv_pid, true),
    send_presence_with_caps(Client),
    user_loop(Client).

create_new_node(Client) ->
    amoc_throttle:wait(?NODE_CREATION_THROTTLING),
    create_pubsub_node(Client),
    amoc_coordinator:add(?MODULE, Client).

user_loop(Client) ->
    receive
        {stanza, _, #xmlel{name = <<"message">>} = Stanza, #{recv_timestamp := TimeStamp}} ->
            process_msg(Stanza, TimeStamp),
            user_loop(Client);
        {stanza, _, #xmlel{name = <<"iq">>} = Stanza, _} ->
            process_iq(Client, Stanza),
            user_loop(Client);
        {stanza, _, #xmlel{name = <<"presence">>} = Stanza, _} ->
            process_presence(Client, Stanza),
            user_loop(Client);
        publish_item ->
            IqTimeout = amoc_config:get(iq_timeout),
            Id = publish_pubsub_item(Client),
            iq_metrics:request(publication, Id, IqTimeout),
            user_loop(Client);
        {'DOWN', _, process, Pid, Info} when Pid =:= Client#client.rcv_pid ->
            ?LOG_ERROR("TCP connection process ~p down: ~p", [Pid, Info]);
        Msg ->
            ?LOG_ERROR("unexpected message ~p", [Msg])
    end.

schedule_publishing(Pid) ->
    amoc_throttle:send(?PUBLICATION_THROTTLING, Pid, publish_item).

%%------------------------------------------------------------------------------------------------
%% User connection
%%------------------------------------------------------------------------------------------------
connect_amoc_user(Id) ->
    ExtraProps = amoc_xmpp:pick_server([[{host, "127.0.0.1"}]]) ++
                 [{server, amoc_config:get(mim_host)},
                  {socket_opts, socket_opts()}],

    {ok, Client, _} = amoc_xmpp:connect_or_exit(Id, ExtraProps),
    erlang:put(jid, Client#client.jid),
    Client.

socket_opts() ->
    [binary,
     {reuseaddr, false},
     {nodelay, true}].

%%------------------------------------------------------------------------------------------------
%% Node creation
%%------------------------------------------------------------------------------------------------
create_pubsub_node(Client) ->
    ReqId = iq_id(create, Client),
    iq_metrics:request(node_creation, ReqId),
    Request = publish_pubsub_stanza(Client, ReqId, #xmlel{name = <<"nothing">>}),
    %Request = escalus_pubsub_stanza:create_node(Client, ReqId, ?NODE),
    escalus:send(Client, Request),

    try
        CreateNodeResult = escalus:wait_for_stanza(Client, amoc_config:get(iq_timeout)),
        case escalus_pred:is_iq_result(Request, CreateNodeResult) of
            true ->
                ?LOG_DEBUG("node creation ~p (~p)", [?NODE, self()]),
                iq_metrics:response(ReqId, result);
            false ->
                iq_metrics:response(ReqId, error),
                ?LOG_ERROR("Error creating node: ~p", [CreateNodeResult]),
                exit(node_creation_failed)
        end

    catch
        exit:{timeout_when_waiting_for_stanza, _} = Exit ->
            iq_metrics:timeout(ReqId, delete),
            ?LOG_ERROR("Timeout creating node: ~p", [Exit]),
            exit(node_creation_timeout);
        Error:Reason ->
            amoc_metrics:update_counter(node_creation_failure),
            ?LOG_ERROR("Error creating node: ~p", [{Error, Reason}]),
            exit(node_creation_failed)
    end.

%%------------------------------------------------------------------------------------------------
%% User presence & caps
%%------------------------------------------------------------------------------------------------
send_presence(From, Type, To = #client{}) ->
    ToJid = escalus_client:short_jid(To),
    send_presence(From, Type, ToJid);
send_presence(From, Type, To) ->
    Presence = escalus_stanza:presence_direct(To, Type),
    escalus_client:send(From, Presence).

send_presence_with_caps(Client) ->
    Presence = escalus_stanza:presence(<<"available">>, [caps()]),
    escalus:send(Client, Presence).

caps() ->
    #xmlel{name = <<"c">>,
           attrs = [{<<"xmlns">>, <<"http://jabber.org/protocol/caps">>},
                    {<<"hash">>, <<"sha-1">>},
                    {<<"node">>, <<"http://www.chatopus.com">>},
                    {<<"ver">>, ?CAPS_HASH}]}.

%%------------------------------------------------------------------------------------------------
%% Item publishing
%%------------------------------------------------------------------------------------------------
publish_pubsub_item(Client) ->
    Id = iq_id(publish, Client),
    PayloadSize = amoc_config:get(publication_size),
    Content = item_content(PayloadSize),
    Request = publish_pubsub_stanza(Client, Id, Content),
    escalus:send(Client, Request),
    Id.

publish_pubsub_stanza(Client, Id, Content) ->
    ItemId = <<"current">>,
    escalus_pubsub_stanza:publish(Client, ItemId, Content, Id, ?NODE).

item_content(PayloadSize) ->
    Payload = #xmlcdata{content = <<<<"A">> || _ <- lists:seq(1, PayloadSize)>>},
    #xmlel{
        name = <<"entry">>,
        attrs = [{<<"timestamp">>, integer_to_binary(os:system_time(microsecond))},
                 {<<"jid">>, erlang:get(jid)}],
        children = [Payload]}.

%%------------------------------------------------------------------------------------------------
%% Item processing
%%------------------------------------------------------------------------------------------------
process_msg(#xmlel{name = <<"message">>} = Stanza, TS) ->
    escalus:assert(is_message, Stanza),
    Entry = exml_query:path(Stanza, [{element, <<"event">>}, {element, <<"items">>},
                                     {element, <<"item">>}, {element, <<"entry">>}]),
    case Entry of
        undefined -> ok;
        _ ->
            case {exml_query:attr(Entry, <<"jid">>), erlang:get(jid)} of
                {JID, JID} -> schedule_publishing(self());
                _ -> ok
            end,
            TimeStampBin = exml_query:attr(Entry, <<"timestamp">>),
            TimeStamp = binary_to_integer(TimeStampBin),
            TTD = TS - TimeStamp,
%%            ?LOG_DEBUG("time to delivery ~p", [TTD]),
            amoc_metrics:update_counter(message),
            amoc_metrics:update_time(message_ttd, TTD)
    end.

process_presence(Client, Stanza) ->
    case exml_query:attr(Stanza, <<"type">>) of
        <<"subscribe">> ->
            From = exml_query:attr(Stanza, <<"from">>),
            send_presence(Client, <<"subscribed">>, From);
        _ ->
            ok %%it's ok to just ignore other presence notifications
    end.

process_iq(Client, #xmlel{name = <<"iq">>} = Stanza) ->
    Id = exml_query:attr(Stanza, <<"id">>),
    Type = exml_query:attr(Stanza, <<"type">>),
    NS = exml_query:path(Stanza, [{element, <<"query">>}, {attr, <<"xmlns">>}]),
    case {Type, NS, Id} of
        {<<"get">>, ?NS_DISCO_INFO, _} ->
            handle_disco_query(Client, Stanza);
        {<<"set">>, ?NS_ROSTER, _} ->
            ok; %%it's ok to just ignore roster pushes
        {_, undefined, <<"publish", _/binary>>} ->
            handle_publish_resp(Stanza,  Id);
        _ ->
            ?LOG_WARNING("unexpected iq ~p", [Stanza])
    end.

handle_publish_resp(PublishResult, Id) ->
    case escalus_pred:is_iq_result(PublishResult) of
        true ->
%%            ?LOG_DEBUG("publish time ~p", [PublishTime]),
            iq_metrics:response(Id, result);
        _ ->
            iq_metrics:response(Id, error),
            ?LOG_ERROR("Error publishing failed: ~p", [PublishResult]),
            exit(publication_failed)
    end.

handle_disco_query(Client, DiscoRequest) ->
    ?LOG_DEBUG("handle_disco_query ~p", [self()]),
    QueryEl = escalus_stanza:query_el(<<"http://jabber.org/protocol/disco#info">>,
                                      feature_elems()),
    DiscoResult = escalus_stanza:iq_result(DiscoRequest, [QueryEl]),
    escalus:send(Client, DiscoResult).

feature_elems() ->
    NodeNs = ?PEP_NODE_NS,
    [#xmlel{name = <<"identity">>,
            attrs = [{<<"category">>, <<"client">>},
                     {<<"name">>, <<"Psi">>},
                     {<<"type">>, <<"pc">>}]},
     #xmlel{name = <<"feature">>,
            attrs = [{<<"var">>, <<"http://jabber.org/protocol/disco#info">>}]},
     #xmlel{name = <<"feature">>,
            attrs = [{<<"var">>, NodeNs}]},
     #xmlel{name = <<"feature">>,
            attrs = [{<<"var">>, <<NodeNs/bitstring, "+notify">>}]}].
%%------------------------------------------------------------------------------------------------
%% Stanza helpers
%%------------------------------------------------------------------------------------------------
iq_id(Type, Client) ->
    UserName = escalus_utils:get_username(Client),
    Suffix = random_suffix(),
    list_to_binary(io_lib:format("~s-~s-~p",
                                 [Type, UserName, Suffix])).

random_suffix() ->
    Suffix = base64:encode(crypto:strong_rand_bytes(5)),
    re:replace(Suffix, "/", "_", [global, {return, binary}]).

%%------------------------------------------------------------------------------------------------
%% Config helpers
%%------------------------------------------------------------------------------------------------
get_no_of_node_subscribers() ->
    %instead of constant No of subscriptions we can use min/max values.
    amoc_config:get(n_of_subscribers).
