%%==============================================================================
%% @copyright 2019-2020 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%% @end
%%
%% @doc
%% In this scenario, users are creating pubsub nodes and publishing items.
%% Users are publishing items to the nodes that they created and receiving items from
%% other nodes they subscribed to. Each node has a number of subscribers limited by
%% the `n_of_subscribers' variable. Publishing can start depending on the `activation_policy'
%% variable, either after `all_nodes' or after `n_nodes' are subscribed to.
%% Interactions between users and pubsub nodes are managed by the `amoc_coordinator'.
%% Additional subscription and publication delay can be introduced with use of the
%% `coordinator_delay' variable. This can help to moderate the load when users are
%% being added.
%%
%%
%% == User steps: ==
%%
%% 1. Connect to the XMPP host given by the `mim_host' variable.
%%
%% 2. Create pubsub node. The rate of node creation is limited by the
%%    `node_creation_rate' per minute. The pubsub service address is defined by the
%%    `pubsub_addr' variable. Node creation results in a timeout when `iq_timeout'
%%    is exceeded.
%%
%% 3. Add user to the `amoc_coordinator' and pass pubsub node and client data.
%%
%% 4. Wait for the following messages in a loop:
%%
%% - {subscribe_to, N} - message from `amoc_coordinator' for the client to subscribe
%%   to the given Node N.
%%
%% - {stanza, MessageStanza} - process message stanza, check if it contains the user's
%%   own jid. If it does, schedule a `publish_item' message. The rate of these messages
%%   is handled by `amoc_throttle' and depends on the `publication_rate' variable.
%%
%% - {stanza, IqStanza} - process an `iq' stanza. Whether response to publish or
%%   subscribe is received, update the metrics accordingly.
%%
%% - publish_item - message from `amoc_throttle' that was scheduled after a
%%   message stanza was received. An item is prepared with payload of `publication_size'.
%%   User sends all messages to the same pubsub node that he created. All messages contain
%%   the user's jid that helps to recognise the message.
%%
%% 5. Continue execution of the `user_loop'. If no message is received for `iq_timeout',
%%    timeouts are calculated for every user request.
%%
%% == Metrics exposed by this scenario: ==
%%
%%   === Counters: ===
%%     ==== Message ====
%%       - message - incremented with every received message stanza.
%%     ==== Node ====
%%       - node_creation_failure - incremented when node creation failed.
%%
%%       - node_creation_success - incremented when node creation succeeded.
%%
%%       - node_creation_timeout - incremented when node creation timed out.
%%     ==== Publication ====
%%       - publication_query - incremented for every pubsub publication query that was sent.
%%
%%       - publication_result - incremented for every correct response to publication query.
%%
%%       - publication_error - incremented for every incorrect response to publication query.
%%
%%       - publication_success - incremented for every correct response to publication query
%%         which didn't timeout.
%%
%%       - publication_timeout - incremented for every correct response to publication query
%%         that timeout.
%%     ==== Subscription ====
%%       - subscription_query - incremented for every subscription query that was sent.
%%
%%       - subscription_result - incremented for every correct response to subscription query.
%%
%%       - subscription_error - incremented for every incorrect response to subscription query.
%%
%%       - subscription_success - incremented for every correct response to subscription query
%%         which didn't timeout.
%%
%%       - subscription_timeout - incremented for every correct response to subscription query
%%         that timeout.
%%
%%  === Times: ===
%%   - node_creation - time for the pubsub node to be created
%%
%%   - subscription - time to subscribe to pubsub node
%%
%%   - publication - time to publish pubsub item
%%
%%   - message_tdd - message time to delivery
%%
%% @end
%%==============================================================================
-module(pubsub_simple).

-behaviour(amoc_scenario).

-include_lib("exml/include/exml.hrl").
-include_lib("escalus/include/escalus.hrl").
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
      description => "The virtual host served by the server (def: <<\"localhost\">>)"},
    #{name => pubsub_addr, default_value => <<"pubsub.localhost">>, verification => ?V(binary),
      description => "Pubsub service address (def: <<\"pubsub.localhost\">>"}
]).

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
    Counters = [message,
                %% node counters
                node_creation_failure, node_creation_success, node_creation_timeout,
                %% publication counters
                publication_query, publication_result, publication_error,
                publication_success, publication_timeout,
                %% subscription counters
                subscription_query, subscription_result, subscription_error,
                subscription_success, subscription_timeout],
    Times = [node_creation, subscription, publication, message_ttd],
    [amoc_metrics:init(counters, Metric) || Metric <- Counters],
    [amoc_metrics:init(times, Metric) || Metric <- Times].

%%------------------------------------------------------------------------------------------------
%% Coordinator
%%------------------------------------------------------------------------------------------------
start_coordinator() ->
    amoc_coordinator:start(?MODULE, get_coordination_plan(), ?COORDINATOR_TIMEOUT).

get_coordination_plan() ->
    N = get_no_of_node_subscribers(),

    [{N, [fun subscribe_users/2,
          users_activation(n_nodes),
          coordination_delay()]},
     {all, users_activation(all_nodes)}].

subscribe_users(_, CoordinationData) ->
    PidsAndNodes = [{Pid, Node} || {Pid, {_Client, Node}} <- CoordinationData],
    [subscribe_msg(P, N) || {P, _} <- PidsAndNodes, {_, N} <- PidsAndNodes].

users_activation(ActivationPolicy) ->
    case amoc_config:get(activation_policy) of
        ActivationPolicy ->
            fun(_, CoordinationData) ->
                [schedule_publishing(Pid) || {Pid, _} <- CoordinationData]
            end;
        _ -> fun(_) -> ok end
    end.

coordination_delay() ->
    Delay = amoc_config:get(coordinator_delay),
    fun({coordinate, _}) -> timer:sleep(Delay);
        (_) -> ok
    end.

subscribe_msg(Pid, Node) ->
    Pid ! {subscribe_to, Node}.

%%------------------------------------------------------------------------------------------------
%% User
%%------------------------------------------------------------------------------------------------
start_user(Client) ->
    ?LOG_DEBUG("user process ~p", [self()]),
    Node = create_new_node(Client),
    erlang:monitor(process, Client#client.rcv_pid),
    escalus_tcp:set_active(Client#client.rcv_pid, true),
    user_loop(Client, Node, #{}).

create_new_node(Client) ->
    amoc_throttle:wait(?NODE_CREATION_THROTTLING),
    Node = create_pubsub_node(Client),
    amoc_coordinator:add(?MODULE, {Client, Node}),
    Node.

user_loop(Client, Node, Requests) ->
    IqTimeout = amoc_config:get(iq_timeout),
    receive
        {subscribe_to, N} ->
            {TS, Id} = subscribe(Client, N),
            amoc_metrics:update_counter(subscription_query, 1),
            user_loop(Client, Node, Requests#{Id=>{new, TS}});
        {stanza, _, #xmlel{name = <<"message">>} = Stanza, #{recv_timestamp := TimeStamp}} ->
            process_msg(Stanza, TimeStamp),
            user_loop(Client, Node, Requests);
        {stanza, _, #xmlel{name = <<"iq">>} = Stanza, #{recv_timestamp := TimeStamp}} ->
            NewRequests = process_iq(Stanza, TimeStamp, Requests),
            user_loop(Client, Node, NewRequests);
        publish_item ->
            {TS, Id} = publish_pubsub_item(Client, Node),
            amoc_metrics:update_counter(publication_query, 1),
            user_loop(Client, Node, Requests#{Id=>{new, TS}});
        {'DOWN', _, process, Pid, Info} when Pid =:= Client#client.rcv_pid ->
            amoc_metrics:update_gauge(amoc_users_size, amoc_users_sup:count_no_of_users()),
            ?LOG_ERROR("TCP connection process ~p down: ~p", [Pid, Info]);
        Msg ->
            amoc_metrics:update_gauge(amoc_users_size, amoc_users_sup:count_no_of_users()),
            ?LOG_ERROR("unexpected message ~p", [Msg])
    after IqTimeout ->
        user_loop(Client, Node, verify_request(Requests))
    end.

verify_request(Requests) ->
    IqTimeout = amoc_config:get(iq_timeout),
    Now = os:system_time(microsecond),
    VerifyFN =
    fun(Key, Value) ->
        case Value of
            {new, TS} when Now > TS + IqTimeout * 1000 ->
                update_timeout_metrics(Key),
                {timeout, TS};
            _ -> Value
        end
    end,
    maps:map(VerifyFN, Requests).

update_timeout_metrics(<<"publish", _/binary>>) ->
    amoc_metrics:update_counter(publication_timeout, 1);
update_timeout_metrics(<<"subscribe", _/binary>>) ->
    amoc_metrics:update_counter(subscription_timeout, 1);
update_timeout_metrics(Id) ->
    ?LOG_ERROR("unknown iq id ~p", Id).

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
    Node = pubsub_node(),
    ReqId = iq_id(create, Client, Node),
    NodeConfig = [{<<"pubsub#subscribe">>, <<"1">>},
                  {<<"pubsub#access_model">>, <<"open">>},
                  {<<"pubsub#publish_model">>, <<"open">>}],
    Request = escalus_pubsub_stanza:create_node(Client, ReqId,
                                                Node, NodeConfig),
    escalus:send(Client, Request),
    try
        Fun = fun() -> escalus:wait_for_stanza(Client, amoc_config:get(iq_timeout)) end,
        {CreateNodeTime, CreateNodeResult} = timer:tc(Fun),
        case escalus_pred:is_iq_result(Request, CreateNodeResult) of
            true ->
                ?LOG_DEBUG("node creation ~p (~p)", [Node, self()]),
                amoc_metrics:update_counter(node_creation_success, 1),
                amoc_metrics:update_time(node_creation, CreateNodeTime);
            false ->
                amoc_metrics:update_counter(node_creation_failure, 1),
                ?LOG_ERROR("Error creating node: ~p", [CreateNodeResult]),
                exit(node_creation_failed)
        end
    catch
        exit:{timeout_when_waiting_for_stanza, _} = Exit ->
            amoc_metrics:update_counter(node_creation_timeout, 1),
            ?LOG_ERROR("Timeout creating node: ~p", [Exit]),
            exit(node_creation_timeout);
        Error:Reason ->
            amoc_metrics:update_counter(node_creation_failure),
            ?LOG_ERROR("Error creating node: ~p", [{Error, Reason}]),
            exit(node_creation_failed)
    end,
    Node.

pubsub_node() ->
    Prefix = <<"princely_musings">>,
    Suffix = random_suffix(),
    Name = <<Prefix/binary, "_", Suffix/binary>>,
    {amoc_config:get(pubsub_addr), Name}.

%%------------------------------------------------------------------------------------------------
%% Node subscription
%%------------------------------------------------------------------------------------------------
subscribe(Client, Node) ->
    Id = iq_id(subscribe, Client, Node),
    Request = escalus_pubsub_stanza:subscribe(Client, Id, Node),
    escalus:send(Client, Request),
    {os:system_time(microsecond), Id}.

%%------------------------------------------------------------------------------------------------
%% Item publishing
%%------------------------------------------------------------------------------------------------
publish_pubsub_item(Client, Node) ->
    Id = iq_id(publish, Client, Node),
    PayloadSize = amoc_config:get(publication_size),
    Content = item_content(PayloadSize),
    Request = escalus_pubsub_stanza:publish(Client, Content, Id, Node),
    escalus:send(Client, Request),
    {os:system_time(microsecond), Id}.


item_content(PayloadSize) ->
    Payload = #xmlcdata{content = <<<<"A">> || _ <- lists:seq(1, PayloadSize)>>},
    #xmlel{
        name = <<"entry">>,
        attrs = #{<<"timestamp">> => integer_to_binary(os:system_time(microsecond)),
                  <<"jid">> => erlang:get(jid)},
        children = [Payload]}.

%%------------------------------------------------------------------------------------------------
%% Item processing
%%------------------------------------------------------------------------------------------------
process_msg(#xmlel{name = <<"message">>} = Stanza, TS) ->
    escalus:assert(is_message, Stanza),
    Entry = exml_query:path(Stanza, [{element, <<"event">>}, {element, <<"items">>},
                                     {element, <<"item">>}, {element, <<"entry">>}]),
    case {exml_query:attr(Entry, <<"jid">>), erlang:get(jid)} of
        {JID, JID} -> schedule_publishing(self());
        _ -> ok
    end,
    TimeStampBin = exml_query:attr(Entry, <<"timestamp">>),
    TimeStamp = binary_to_integer(TimeStampBin),
    TTD = TS - TimeStamp,
    ?LOG_DEBUG("time to delivery ~p", [TTD]),
    amoc_metrics:update_counter(message),
    amoc_metrics:update_time(message_ttd, TTD).

process_iq(#xmlel{name = <<"iq">>} = Stanza, TS, Requests) ->
    RespId = exml_query:attr(Stanza, <<"id">>),
    case {RespId, maps:get(RespId, Requests, undefined)} of
        {_, undefined} ->
            ?LOG_WARNING("unknown iq ~p ~p", [Stanza]);
        {<<"publish", _/binary>>, {Tag, ReqTS}} ->
            handle_publish_resp(Stanza, {Tag, TS - ReqTS});
        {<<"subscribe", _/binary>>, {Tag, ReqTS}} ->
            handle_subscribe_resp(Stanza, {Tag, TS - ReqTS})
    end,
    maps:remove(RespId, Requests).

handle_publish_resp(PublishResult, {Tag, PublishTime}) ->
    IqTimeout = amoc_config:get(iq_timeout),
    case escalus_pred:is_iq_result(PublishResult) of
        true ->
            ?LOG_DEBUG("publish time ~p", [PublishTime]),
            amoc_metrics:update_counter(publication_result, 1),
            amoc_metrics:update_time(publication, PublishTime),
            case Tag of
                new when IqTimeout * 1000 > PublishTime ->
                    amoc_metrics:update_counter(publication_success, 1);
                new ->
                    amoc_metrics:update_counter(publication_timeout, 1);
                timeout -> ok %% do nothing, it's already reported as timeout
            end;
        _ ->
            amoc_metrics:update_counter(publication_error, 1),
            ?LOG_ERROR("Error publishing failed: ~p", [PublishResult]),
            exit(publication_failed)
    end.

handle_subscribe_resp(SubscribeResult, {Tag, SubscribeTime}) ->
    IqTimeout = amoc_config:get(iq_timeout),
    case escalus_pred:is_iq_result(SubscribeResult) of
        true ->
            ?LOG_DEBUG("subscribe time ~p", [SubscribeTime]),
            amoc_metrics:update_counter(subscription_result, 1),
            amoc_metrics:update_time(subscription, SubscribeTime),
            case Tag of
                new when IqTimeout > SubscribeTime ->
                    amoc_metrics:update_counter(subscription_success, 1);
                new ->
                    amoc_metrics:update_counter(subscription_timeout, 1);
                timeout -> ok %% do nothing, it's already reported as timeout
            end;
        _ ->
            amoc_metrics:update_counter(subscription_error, 1),
            ?LOG_ERROR("Error subscribing failed: ~p", [SubscribeResult]),
            exit(subscription_failed)
    end.

%%------------------------------------------------------------------------------------------------
%% Stanza helpers
%%------------------------------------------------------------------------------------------------
iq_id(Type, Client, {NodeAddr, NodeName}) ->
    UserName = escalus_utils:get_username(Client),
    Suffix = random_suffix(),
    list_to_binary(io_lib:format("~s-~s-~s-~s-~p",
                                 [Type, UserName, NodeAddr, NodeName, Suffix])).

random_suffix() ->
    Suffix = base64:encode(crypto:strong_rand_bytes(5)),
    re:replace(Suffix, "/", "_", [global, {return, binary}]).

%%------------------------------------------------------------------------------------------------
%% Config helpers
%%------------------------------------------------------------------------------------------------
get_no_of_node_subscribers() ->
    %instead of constant No of subscriptions we can use min/max values.
    amoc_config:get(n_of_subscribers).
