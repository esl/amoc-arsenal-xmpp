%%==============================================================================
%% @copyright 2019-2020 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%% @end
%%
%% @doc
%% In this scenario, users are communicating using PEP and MUC Light, while GDPR
%% removal requests are performed.
%%
%% Users are publishing items to their PEP nodes and receiving items from other
%% users' nodes. Each node has a number of subscribers limited by the
%% `n_of_subscribers' variable. Publishing can start depending on the
%% `node_activation_policy' variable, either after `all_nodes' or after
%% `n_nodes' are subscribed to. Similarly, users send and receive messages from
%% MUC Light rooms. Each room has `room_size' users, and sending messages to
%% rooms can start depending on the `room_activation_policy'. Interactions
%% between users, pubsub PEP nodes and MUC Light rooms are managed by the
%% `amoc_coordinator'.
%%
%% == User steps: ==
%%
%% 1. Connect to the XMPP host given by the `mim_host' variable.
%%
%% 2. Create a PEP node and send presence `available', in order to receive
%%    messages from the PEP nodes. The rate of nodes' creation is limited by the
%%    `node_creation_rate' per minute. Node creation results in a timeout when
%%    `iq_timeout' is exceeded.
%%
%% 3. Create a MUC Light room. The rate of rooms' creation is limited by the
%%    `room_creation_rate' per minute. The virtual MUC host is defined by the
%%    `muc_host' variable. Room creation counts as a timeout when `iq_timeout'
%%    is exceeded.
%%
%% 4. Wait for the following messages in a loop:
%%
%% - {publish_item_room, RoomJid} - message from `amoc_coordinator', sent after
%%   users have created their rooms, or from `amoc_throttle', scheduled after
%%   receiving a groupchat message from self.
%%   Send a groupchat message to the Room. Size of this message is defined by
%%   the `publication_size' variable. The rate of `publish_item_room' messages
%%   is handled by `amoc_throttle' and depends on the `room_publication_rate'
%%   variable.
%%
%% - publish_item_node - a message analogous to the `publish_item_room' message.
%%   Send a message, which size is defined by the `publication_size' variable,
%%   to the PEP node. The rate of these `publish_item_node' messages is handled
%%   by `amoc_throttle' and depends on the `node_publication_rate' variable.
%%
%% - {add_users, MemberJids} - message from `amoc_coordinator', sent after it
%%   had grouped `room_size' users together. Add users with given jids to own
%%   MUC Light room as members.
%%
%% - remove_user - send a GDPR removal request and quit scenario execution for
%%   this user. The rate of these messages is handled by `amoc_throttle' and
%%   depends on the `gdpr_removal_rate' variable. They should start being sent
%%   after all users have logged in.
%%
%% - {stanza, _, MsgStanza, TimeStamp} - process either a pubsub or a groupchat
%%   message and update corresponding metrics. If it's the first MUC Light
%%   affiliation message, remember the randomly generated RoomJid. Discard all
%%   other affiliation change messages. In the case of a pubsub message, check
%%   if it contains user's own jid. If it does, schedule a `publish_item_node'
%%   message. Similarly for a MUC Light message: check if it contains user's own
%%   jid and if it does, schedule a `publish_item_room' message.
%%
%% - {stanza, _, IqStanza, TimeStamp} - process an `iq' stanza and update
%%   corresponding metrics. In the case of an iq result for the room creation,
%%   send client data to the coordinator, which informs that the user is ready
%%   to send groupchat messages.
%%
%% - {stanza, _, PresenceStanza, TimeStamp} - respond to the `subscribe'
%%   presence stanzas.
%%
%% 5. Continue execution of the `user_loop'. If no message is received for
%%    `iq_timeout', timeouts are calculated for every user request.
%%
%% == Metrics exposed by this scenario: ==
%%
%%   === Counters: ===
%%     ==== Message ====
%%       - pubsub_message - incremented with every message stanza received from
%%         pubsub PEP.
%%
%%       - muc_light_message - incremented with every message stanza received
%%         from MUC Light.
%%
%%       - muc_light_message_sent - incremented with every message sent to the
%%         room.
%%
%%       - muc_light_affiliation_change_messages - incremented with every
%%         affiliation change message from MUC Light.
%%     ==== Node ====
%%       - node_creation_success - incremented when node creation succeeded.
%%
%%       - node_creation_failure - incremented when node creation failed.
%%
%%       - node_creation_timeout - incremented when node creation timed out.
%%     ==== Room ====
%%       - room_creation_success - incremented when room creation succeeded.
%%
%%       - room_creation_failure - incremented when room creation failed.
%%
%%       - room_creation_timeout - incremented when room creation timed out.
%%
%%       - room_affiliation_change_success - incremented when room creation
%%         failed.
%%
%%       - room_affiliation_change_failure - incremented when room creation
%%         succeeded.
%%
%%       - room_affiliation_change_timeout - incremented when room creation
%%         timed out.
%%     ==== Publication ====
%%       - publication_query - incremented for every pubsub publication query
%%         that was sent.
%%
%%       - publication_result - incremented for every correct response to
%%         publication query.
%%
%%       - publication_error - incremented for every incorrect response to
%%         publication query.
%%
%%       - publication_success - incremented for every correct response to
%%         publication query which didn't timeout.
%%
%%       - publication_timeout - incremented for every correct response to
%%         publication query that timeout.
%%     ==== GDPR ====
%%       - gdpr_removal - incremented when a user is removed.
%%  === Times: ===
%%   - room_creation - time for the MUC Light room to be created.
%%
%%   - node_creation - time for the pubsub PEP node to be created.
%%
%%   - pubsub_publication - time to publish a pubsub item.
%%
%%   - pubsub_message_ttd - message time to delivery for pubsub.
%%
%%   - muc_light_ttd - message time to delivery for MUC Light.
%%
%%   - room_affiliation_change - time to change room affiliation.
%%
%%   - gdpr_removal - time to perform the GDPR removal request.
%%
%% @end
%%==============================================================================
-module(gdpr_removal).

-behaviour(amoc_scenario).

-include_lib("exml/include/exml.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("kernel/include/logger.hrl").

-define(V(X), fun amoc_config_validation:X/1).

-required_variable([
    #{name => iq_timeout, default_value => 10000, verification => ?V(positive_integer),
      description => "IQ timeout (milliseconds, def: 10000ms)"},
    #{name => room_creation_rate, default_value => 600, verification => ?V(positive_integer),
      description => "Rate of room creations (per minute, def:600)"},
    #{name => node_creation_rate, default_value => 600, verification => ?V(positive_integer),
      description => "Rate of node creations (per minute, def:600)"},
    #{name => room_publication_rate, default_value => 1500, verification => ?V(positive_integer),
      description => "Rate of publications to room (per minute, def:1500)"},
    #{name => node_publication_rate, default_value => 1500, verification => ?V(positive_integer),
      description => "Rate of publications to PEP node (per minute, def:1500)"},
    #{name => room_size, default_value => 10, verification => ?V(positive_integer),
      description => "Number of users in a room."},
    #{name => n_of_subscribers, default_value => 50, verification => ?V(nonnegative_integer),
      description => "Number of subscriptions for each node (def: 50)"},
    #{name => room_activation_policy, default_value => all_rooms, verification => [all_rooms, n_rooms],
      description => "Publish after setup of (def: all_rooms | n_sers)"},
    #{name => node_activation_policy, default_value => all_nodes, verification => [all_nodes, n_nodes],
      description => "Publish after setup of (def: all_nodes | n_nodes)"},
    #{name => gdpr_removal_rate, default_value => 2, verification => ?V(positive_integer),
      description => "Rate of user removals (per minute, def:1)"},
    #{name => publication_size, default_value => 300, verification => ?V(nonnegative_integer),
      description => "Size of additional payload (bytes, def:300)"},
    #{name => mim_host, default_value => <<"localhost">>, verification => ?V(binary),
      description => "The virtual host served by the server (def: <<\"localhost\">>)"},
    #{name => muc_host, default_value => <<"muclight.localhost">>, verification => ?V(binary),
      description => "The virtual MUC host served by the server (def: <<\"muclight.localhost\">>)"}
]).

-define(ROOM_CREATION_THROTTLING, room_creation).
-define(NODE_CREATION_THROTTLING, node_creation).
-define(ROOM_PUBLICATION_THROTTLING, room_publication).
-define(NODE_PUBLICATION_THROTTLING, node_publication).
-define(REMOVAL_THROTTLING, user_removal).

-define(ROOM_CREATION_ID, <<"room_creation_id">>).
-define(NODE_CREATION_ID, <<"node_creation_id">>).

-define(PEP_NODE_NS, <<"just_some_random_namespace">>).
-define(CAPS_HASH, <<"erNmVoMSwRBR4brUU/inYQ5NFr0=">>). %% mod_caps:make_disco_hash(feature_elems(), sha1).
-define(NODE, {pep, ?PEP_NODE_NS}).

-define(NS_MUC_LIGHT_AFFILIATIONS, <<"urn:xmpp:muclight:0#affiliations">>).
-define(NS_MUC_LIGHT_CREATION, <<"urn:xmpp:muclight:0#create">>).

-export([init/0, start/1]).

-spec init() -> ok.
init() ->
    init_metrics(),
    http_req:start(),

    [RoomPublicationRate, NodePublicationRate, RoomCreationRate, NodeCreationRate, GDPRRemovalRate] =
    [amoc_config:get(Key) ||
     Key <- [room_publication_rate, node_publication_rate,
             room_creation_rate, node_creation_rate,
             gdpr_removal_rate]],
    amoc_throttle:start(?ROOM_CREATION_THROTTLING, RoomCreationRate),
    amoc_throttle:start(?ROOM_PUBLICATION_THROTTLING, RoomPublicationRate),
    amoc_throttle:start(?NODE_CREATION_THROTTLING, NodeCreationRate),
    amoc_throttle:start(?NODE_PUBLICATION_THROTTLING, NodePublicationRate),
    amoc_throttle:start(?REMOVAL_THROTTLING, GDPRRemovalRate),

    start_coordinator(),
    ok.

-spec start(amoc_scenario:user_id()) -> any().
start(Id) ->
    Client = connect_amoc_user(Id),
    start_user(Client).

init_metrics() ->
    Counters = [pubsub_message, muc_light_message,
        room_creation_success, room_creation_timeout, room_creation_failure,
        node_creation_success, node_creation_timeout, node_creation_failure,
        publication_query, publication_result, publication_error,
        publication_success, publication_timeout,
        muc_light_message_sent,

        muc_light_affiliation_change_messages,

        room_affiliation_change_success, room_affiliation_change_timeout,
        room_affiliation_change_failure,

        gdpr_removal],
    Times = [room_creation, node_creation,
        pubsub_publication,
        pubsub_message_ttd, muc_light_ttd,
        room_affiliation_change,
        gdpr_removal],
    [amoc_metrics:init(counters, Metric) || Metric <- Counters],
    [amoc_metrics:init(times, Metric) || Metric <- Times].

%%------------------------------------------------------------------------------------------------
%% Coordinator
%%------------------------------------------------------------------------------------------------
start_coordinator() ->
    Plan = get_plan(),
    amoc_coordinator:start(?MODULE, Plan).

get_plan() ->
    [{amoc_config:get(room_size),
      fun(_, PidsAndClients) ->
          make_full_rooms(PidsAndClients),
          room_activate_users(pids(PidsAndClients), n_rooms)
      end},
     {amoc_config:get(n_of_subscribers),
      fun(_, PidsAndClients) ->
          make_all_clients_friends(clients(PidsAndClients)),
          node_activate_users(pids(PidsAndClients), n_nodes)
      end},
     {all,
      fun(_, PidsAndClients) ->
          room_activate_users(pids(PidsAndClients), all_rooms),
          node_activate_users(pids(PidsAndClients), all_nodes),
          activate_removal(pids(PidsAndClients))
      end}].

clients(PidsAndClients) ->
    {_Pids, Clients} = lists:unzip(PidsAndClients),
    Clients.

pids(PidsAndClients) ->
    {Pids, _Clients} = lists:unzip(PidsAndClients),
    Pids.

node_activate_users(Pids, ActivationPolicy) ->
    case amoc_config:get(node_activation_policy) of
        ActivationPolicy ->
            ?LOG_DEBUG("Node activate users running. Policy ~p. Pids: ~p", [ActivationPolicy, Pids]),
            [schedule_node_publishing(Pid) || Pid <- Pids];
        _ -> ok
    end.

room_activate_users(Pids, ActivationPolicy) ->
    case amoc_config:get(room_activation_policy) of
        ActivationPolicy ->
            ?LOG_DEBUG("Room activate users running. Policy ~p. Pids: ~p", [ActivationPolicy, Pids]),
            [schedule_room_publishing(Pid) || Pid <- Pids];
        _ -> ok
    end.

activate_removal(Pids) ->
    [schedule_removal(Pid) || Pid <- Pids].

make_all_clients_friends(Clients) ->
    ?LOG_DEBUG("Make all clients friends."),
    escalus_utils:distinct_pairs(
        fun(C1, C2) ->
            send_presence(C1, <<"subscribe">>, C2),
            send_presence(C2, <<"subscribe">>, C1)
        end, Clients).

make_full_rooms(PidsAndClients) ->
    PidsAndJids = [{Pid, Client#client.jid} || {Pid, Client} <- PidsAndClients],
    [begin
         MemberJids = [Jid || {_, Jid} <- PidsAndJids, Jid =/= OwnerJid],
         OwnerPid ! {add_users, MemberJids}
     end || {OwnerPid, OwnerJid} <- PidsAndJids].

schedule_removal(Pid) ->
    amoc_throttle:send(?REMOVAL_THROTTLING, Pid, remove_user).

%%------------------------------------------------------------------------------------------------
%% User
%%------------------------------------------------------------------------------------------------
start_user(Client) ->
    ?LOG_DEBUG("User process ~p", [self()]),
    erlang:monitor(process, Client#client.rcv_pid),
    create_new_node(Client),
    ?LOG_DEBUG("Node created User process ~p", [self()]),
    send_presence_with_caps(Client),
    escalus_tcp:set_active(Client#client.rcv_pid, true),
    {TS, Id} = request_muc_light_room(Client),
    user_loop(Client, #{Id=>{new, TS}}).

create_new_node(Client) ->
    amoc_throttle:wait(?NODE_CREATION_THROTTLING),
    create_pubsub_node(Client).

user_loop(Client, Requests) ->
    IqTimeout = amoc_config:get(iq_timeout),
    receive
        {publish_item_room, RoomJid} ->
            amoc_metrics:update_counter(muc_light_message_sent),
            send_message_to_room(Client, RoomJid),
            user_loop(Client, Requests);
        publish_item_node ->
            {TS, Id} = publish_pubsub_item(Client),
            amoc_metrics:update_counter(publication_query),
            user_loop(Client, Requests#{Id=>{new, TS}});
        {add_users, MemberJids} ->
            {TS, Id} = add_users_to_room(Client, MemberJids),
            user_loop(Client, Requests#{Id=>{new, TS}});
        remove_user ->
            ?LOG_DEBUG("GDPR: Removing myself ~p (~p)", [escalus_client:short_jid(Client), self()]),
            remove_self(Client);
        {stanza, _, #xmlel{name = <<"message">>} = Stanza, #{recv_timestamp := RecvTimeStamp}} ->
            process_message(Stanza, RecvTimeStamp),
            user_loop(Client, Requests);
        {stanza, _, #xmlel{name = <<"iq">>} = Stanza, #{recv_timestamp := RecvTimeStamp}} ->
            NewRequests = process_iq(Client, Stanza, RecvTimeStamp, Requests),
            user_loop(Client, NewRequests);
        {stanza, _, #xmlel{name = <<"presence">>} = Stanza, _} ->
            process_presence(Client, Stanza),
            user_loop(Client, Requests);
        {'DOWN', _, process, Pid, Info} when Pid =:= Client#client.rcv_pid ->
            ?LOG_ERROR("TCP connection process ~p down: ~p", [Pid, Info]);
        Msg ->
            ?LOG_ERROR("unexpected message ~p", [Msg])
    after IqTimeout ->
        user_loop(Client, verify_request(Requests))
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
    amoc_metrics:update_counter(publication_timeout);
update_timeout_metrics(<<"affiliation", _/binary>>) ->
    amoc_metrics:update_counter(room_affiliation_change_timeout);
update_timeout_metrics(?ROOM_CREATION_ID) ->
    amoc_metrics:update_counter(room_creation_timeout);
update_timeout_metrics(Id) ->
    ?LOG_ERROR("unknown iq id ~p", Id).

schedule_room_publishing(Pid) ->
    amoc_throttle:send(?ROOM_PUBLICATION_THROTTLING, Pid, {publish_item_room, undefined}).
schedule_room_publishing(Pid, RoomJid) ->
    amoc_throttle:send(?ROOM_PUBLICATION_THROTTLING, Pid, {publish_item_room, RoomJid}).

schedule_node_publishing(Pid) ->
    amoc_throttle:send(?NODE_PUBLICATION_THROTTLING, Pid, publish_item_node).

remove_self(Client) ->
    %TODO when running with clt-swarm make sure to use correct cfg, change ports here etc.
    Path = list_to_binary(["/api/users/", amoc_config:get(mim_host), "/", escalus_client:username(Client)]),

    {RemovalTime, {ok, _}} = timer:tc(fun() -> http_req:request("http://localhost:8088", Path, <<"DELETE">>, []) end),

    amoc_metrics:update_counter(gdpr_removal),
    amoc_metrics:update_time(gdpr_removal, RemovalTime),
    % Suppresses errors from escalus, unlike just jumping out of loop
    throw(stop).

%%------------------------------------------------------------------------------------------------
%% User connection
%%------------------------------------------------------------------------------------------------
connect_amoc_user(Id) ->
    Cfg = make_user_cfg(Id),
    {ok, Client, _} = escalus_connection:start(Cfg),
    erlang:put(jid, Client#client.jid),
    Client.

make_user_cfg(Id) ->
    BinId = integer_to_binary(Id),
    Username = <<"user_", BinId/binary>>,
    Password = <<"password_", BinId/binary>>,
    Resource = <<"res1">>,
    ConnectionDetails = amoc_xmpp:pick_server([[{host, "127.0.0.1"}]]),
    [{username, Username},
        {server, amoc_config:get(mim_host)},
        {resource, Resource},
        {password, Password},
        {carbons, false},
        {stream_management, false},
        {socket_opts, socket_opts()} |
        ConnectionDetails].

socket_opts() ->
    [binary,
        {reuseaddr, false},
        {nodelay, true}].

%%------------------------------------------------------------------------------------------------
%% Node creation
%%------------------------------------------------------------------------------------------------
create_pubsub_node(Client) ->
    ReqId = ?NODE_CREATION_ID,
    Request = publish_pubsub_stanza(Client, ReqId, #xmlel{name = <<"nothing">>}),
    escalus:send(Client, Request),

    try
        Fun = fun() -> escalus:wait_for_stanza(Client, amoc_config:get(iq_timeout)) end,
        {CreateNodeTime, CreateNodeResult} = timer:tc(Fun),
        case escalus_pred:is_iq_result(Request, CreateNodeResult) of
            true ->
                ?LOG_DEBUG("node creation ~p (~p)", [?NODE, self()]),
                amoc_metrics:update_counter(node_creation_success),
                amoc_metrics:update_time(node_creation, CreateNodeTime);
            false ->
                amoc_metrics:update_counter(node_creation_failure),
                ?LOG_ERROR("Error creating node: ~p", [CreateNodeResult]),
                exit(node_creation_failed)
        end
    catch
        exit:{timeout_when_waiting_for_stanza, _} = Exit ->
            amoc_metrics:update_counter(node_creation_timeout),
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
    ?LOG_DEBUG("Send presence with caps ~p, (~p).", [escalus_client:short_jid(Client), self()]),
    Presence = escalus_stanza:presence(<<"available">>, [caps()]),
    escalus:send(Client, Presence).

caps() ->
    #xmlel{name = <<"c">>,
        attrs = [{<<"xmlns">>, <<"http://jabber.org/protocol/caps">>},
            {<<"hash">>, <<"sha-1">>},
            {<<"node">>, <<"http://www.chatopus.com">>},
            {<<"ver">>, ?CAPS_HASH}]}.

%%------------------------------------------------------------------------------------------------
%% Room creation
%%------------------------------------------------------------------------------------------------
request_muc_light_room(Client) ->
    amoc_throttle:wait(?ROOM_CREATION_THROTTLING),
    Id = ?ROOM_CREATION_ID,
    MucHost = amoc_config:get(muc_host),
    CreateRoomStanza = escalus_stanza:iq_set(?NS_MUC_LIGHT_CREATION, []),
    CreateRoomStanzaWithTo = escalus_stanza:to(CreateRoomStanza, MucHost),
    CreateRoomStanzaWithId = escalus_stanza:set_id(CreateRoomStanzaWithTo, Id),

    escalus:send(Client, CreateRoomStanzaWithId),
    {os:system_time(microsecond), Id}.

%%------------------------------------------------------------------------------------------------
%% Room affiliation change
%%------------------------------------------------------------------------------------------------
add_users_to_room(Client, Jids) ->
    Id = iq_id(affiliation, Client),
    RoomJid = erlang:get(my_room),
    AffList = [#xmlel{name = <<"user">>,
        attrs = [{<<"affiliation">>, <<"member">>}],
        children = [#xmlcdata{content = Jid}]} || Jid <- Jids],
    AffChangeStanza = escalus_stanza:iq_set(?NS_MUC_LIGHT_AFFILIATIONS, AffList),
    AffChangeStanzaWithId = escalus_stanza:set_id(AffChangeStanza, Id),
    ?LOG_DEBUG("Adding users to room: ~p", [Jids]),
    escalus:send(Client, escalus_stanza:to(AffChangeStanzaWithId, RoomJid)),
    {os:system_time(microsecond), Id}.

%%------------------------------------------------------------------------------------------------
%% Sending muc_light messages
%%------------------------------------------------------------------------------------------------
send_message_to_room(Client, undefined) ->
    RoomJid = erlang:get(my_room),
    send_message_to_room(Client, RoomJid);
send_message_to_room(Client, RoomJid) ->
    PayloadSize = amoc_config:get(publication_size),
    MessageBody = item_content(PayloadSize),
    Message = #xmlel{name = <<"message">>,
        attrs = [{<<"to">>, RoomJid},
            {<<"type">>, <<"groupchat">>}],
        children = [MessageBody]},
    escalus:send(Client, Message).

%%------------------------------------------------------------------------------------------------
%% Item publishing
%%------------------------------------------------------------------------------------------------

publish_pubsub_item(Client) ->
    Id = iq_id(publish, Client),
    PayloadSize = amoc_config:get(publication_size),
    Content = item_content(PayloadSize),
    Request = publish_pubsub_stanza(Client, Id, Content),
    escalus:send(Client, Request),
    {os:system_time(microsecond), Id}.

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

process_message(Stanza, RecvTimeStamp) ->
    Type = exml_query:attr(Stanza, <<"type">>),
    case Type of
        <<"groupchat">> -> process_muc_light_message(Stanza, RecvTimeStamp);
        _ -> process_pubsub_msg(Stanza, RecvTimeStamp)
    end.

process_pubsub_msg(#xmlel{name = <<"message">>} = Stanza, TS) ->
    Entry = exml_query:path(Stanza, [{element, <<"event">>}, {element, <<"items">>},
        {element, <<"item">>}, {element, <<"entry">>}]),
    case Entry of
        undefined -> ok;
        _ ->
            case {exml_query:attr(Entry, <<"jid">>), erlang:get(jid)} of
                {JID, JID} -> schedule_node_publishing(self());
                _ -> ok
            end,
            TimeStampBin = exml_query:attr(Entry, <<"timestamp">>),
            TimeStamp = binary_to_integer(TimeStampBin),
            TTD = TS - TimeStamp,
%%            ?LOG_DEBUG("pubsub time to delivery ~p", [TTD]),
            amoc_metrics:update_counter(pubsub_message),
            amoc_metrics:update_time(pubsub_message_ttd, TTD)
    end.

process_muc_light_message(Stanza, RecvTimeStamp) ->
    case exml_query:subelement(Stanza, <<"x">>) of
        undefined ->
            handle_normal_muc_light_message(Stanza, RecvTimeStamp);
        #xmlel{name = <<"x">>, attrs = [{<<"xmlns">>, ?NS_MUC_LIGHT_AFFILIATIONS}], children = _} ->
            handle_muc_light_affiliation_message(Stanza);
        _ -> ?LOG_ERROR("Unknown message.")
    end.

handle_normal_muc_light_message(Stanza, RecvTimeStamp) ->
    ReqTimeStampBin = exml_query:path(Stanza, [{element, <<"entry">>}, {attr, <<"timestamp">>}]),
    ReqTimeStamp = binary_to_integer(ReqTimeStampBin),

    RoomBareJid = get_sender_bare_jid(Stanza),

    From = exml_query:path(Stanza, [{element, <<"entry">>}, {attr, <<"jid">>}]),
    case erlang:get(jid) of
        From -> schedule_room_publishing(self(), RoomBareJid);
        _ -> ok
    end,

    TTD = RecvTimeStamp - ReqTimeStamp,
%%    ?LOG_DEBUG("muc light time to delivery ~p", [TTD]),
    amoc_metrics:update_counter(muc_light_message),
    amoc_metrics:update_time(muc_light_ttd, TTD).

handle_muc_light_affiliation_message(Stanza) ->
    amoc_metrics:update_counter(muc_light_affiliation_change_messages),
    case exml_query:subelement(Stanza, <<"prev-version">>) of
        % actually XEP states only that prev-version SHOULD NOT be sent to new users - not sure if can rely on that
        undefined -> handle_first_affiliation_message(Stanza);
        _ -> ok % drop affiliation change stanzas
    end.

handle_first_affiliation_message(Stanza) ->
    RoomJid = exml_query:attr(Stanza, <<"from">>),
    case erlang:get(rooms) of
        undefined ->
            erlang:put(rooms, [RoomJid]),
            erlang:put(my_room, RoomJid);
        RoomList ->
            case lists:member(RoomJid, RoomList) of
                true -> ok;
                false -> erlang:put(rooms, [RoomJid | RoomList])
            end
    end.

process_presence(Client, Stanza) ->
    case exml_query:attr(Stanza, <<"type">>) of
        <<"subscribe">> ->
            From = exml_query:attr(Stanza, <<"from">>),
            send_presence(Client, <<"subscribed">>, From);
        _ ->
            ok %%it's ok to just ignore other presence notifications
    end.

process_iq(Client, #xmlel{name = <<"iq">>} = Stanza, TS, Requests) ->
    Id = exml_query:attr(Stanza, <<"id">>),
    Type = exml_query:attr(Stanza, <<"type">>),
    NS = exml_query:path(Stanza, [{element, <<"query">>}, {attr, <<"xmlns">>}]),
    case {Type, NS, Id, maps:get(Id, Requests, undefined)} of
        {<<"result">>, undefined, ?ROOM_CREATION_ID, {Tag, ReqTS}} ->
            handle_muc_light_room_iq_result(Stanza, {Tag, TS - ReqTS}),
            send_info_to_coordinator(Client);
        {<<"result">>, _, <<"affiliation", _/binary>>, {Tag, ReqTS}} ->
            handle_affiliation_change_iq(Stanza, {Tag, TS - ReqTS});
        {<<"get">>, ?NS_DISCO_INFO, _, undefined} ->
            handle_disco_query(Client, Stanza);
        {<<"set">>, ?NS_ROSTER, _, undefined} ->
            ok; %%it's ok to just ignore roster pushes
        {_, undefined, <<"publish", _/binary>>, undefined} ->
            ?LOG_WARNING("unknown publish iq ~p", [Stanza]);
        {_, undefined, <<"publish", _/binary>>, {Tag, ReqTS}} ->
            handle_publish_resp(Stanza, {Tag, TS - ReqTS});
        _ ->
            ?LOG_WARNING("unexpected iq ~p", [Stanza])
    end,
    maps:remove(Id, Requests).

handle_muc_light_room_iq_result(CreateRoomResult, {Tag, RoomCreationTime}) ->
    case {escalus_pred:is_iq_result(CreateRoomResult), CreateRoomResult} of
        {true, _} ->
            ?LOG_DEBUG("Room creation ~p took ~p", [self(), RoomCreationTime]),
            amoc_metrics:update_time(room_creation, RoomCreationTime),
            IqTimeout = amoc_config:get(iq_timeout),
            case Tag of
                new when IqTimeout * 1000 > RoomCreationTime ->
                    amoc_metrics:update_counter(room_creation_success);
                new ->
                    amoc_metrics:update_counter(room_creation_timeout);
                timeout -> ok %% do nothing, it's already reported as timeout
            end;
        {false, _} ->
            amoc_metrics:update_counter(room_creation_failure),
            ?LOG_ERROR("Error creating room: ~p", [CreateRoomResult]),
            exit(room_creation_failed)
    end.

send_info_to_coordinator(Client) ->
    ?LOG_DEBUG("Process ~p, sending info about myself to coordinator", [self()]),
    amoc_coordinator:add(?MODULE, Client).

handle_affiliation_change_iq(AffiliationChangeResult, {Tag, AffiliationChangeTime}) ->
    case {escalus_pred:is_iq_result(AffiliationChangeResult), AffiliationChangeResult} of
        {true, _} ->
            ?LOG_DEBUG("Adding users to room ~p took ~p", [self(), AffiliationChangeTime]),
            amoc_metrics:update_time(room_affiliation_change, AffiliationChangeTime),
            IqTimeout = amoc_config:get(iq_timeout),
            case Tag of
                new when IqTimeout * 1000 > AffiliationChangeTime ->
                    amoc_metrics:update_counter(room_affiliation_change_success);
                new ->
                    amoc_metrics:update_counter(room_affiliation_change_timeout);
                timeout -> ok %% do nothing, it's already reported as timeout
            end;
        {false, _} ->
            amoc_metrics:update_counter(room_affiliation_change_failure),
            ?LOG_ERROR("Error affiliation change: ~p", [AffiliationChangeTime]),
            exit(affiliation_change_timeout)
    end.

handle_publish_resp(PublishResult, {Tag, PublishTime}) ->
    IqTimeout = amoc_config:get(iq_timeout),
    case escalus_pred:is_iq_result(PublishResult) of
        true ->
            amoc_metrics:update_counter(publication_result),
            amoc_metrics:update_time(pubsub_publication, PublishTime),
            case Tag of
                new when IqTimeout * 1000 > PublishTime ->
                    amoc_metrics:update_counter(publication_success);
                new ->
                    amoc_metrics:update_counter(publication_timeout);
                timeout -> ok %% do nothing, it's already reported as timeout
            end;
        _ ->
            amoc_metrics:update_counter(publication_error),
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

get_sender_bare_jid(Stanza) ->
    From = exml_query:attr(Stanza, <<"from">>),
    [BareJid | _] = binary:split(From, <<"/">>),
    BareJid.
