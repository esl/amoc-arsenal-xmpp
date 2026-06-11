-module(amoc_xmpp_pep).

-moduledoc """
Building blocks for PEP (XEP-0163: Personal Eventing Protocol) scenarios:
  - Determining the names of nodes to publish (see nodes_per_user)
  - Item publishing (auto-create makes separate node creation unnecessary)
  - Counting incoming item notifications and measuring TTD
""".

-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("kernel/include/logger.hrl").

-export([init/0,
         publish_item/2,
         nodes_to_publish/0,
         received_handler_spec/0]).

-define(V(X), (fun amoc_config_validation:X/1)).

-required_variable(#{name => nodes_per_user, default_value => 1,
                     verification => ?V(nonnegative_integer),
                     description => "Number of PEP nodes each user publishes to"}).

-spec init() -> ok.
init() ->
    amoc_metrics:init(counters, pubsub_items_published),
    amoc_metrics:init(counters, pubsub_notifications_received),
    amoc_metrics:init(counters, timeouts),
    amoc_metrics:init(times, pubsub_item_ttd),
    amoc_metrics:init(times, response),
    ok.

-spec nodes_to_publish() -> [binary()].
nodes_to_publish() ->
    [node_name(NodeId) || NodeId <- lists:seq(1, cfg(nodes_per_user))].

-spec received_handler_spec() -> [amoc_xmpp_handlers:handler_spec()].
received_handler_spec() ->
    [{fun is_pep_notification/1,
      fun record_pep_notification/3}].

-spec publish_item(escalus:client(), escalus_pubsub_stanza:pubsub_node_name()) -> ok.
publish_item(Client, Node) ->
    Id = escalus_stanza:id(),
    Req = escalus_pubsub_stanza:publish(item_content(Client), Id, Node),
    Pred = fun(Stanza) -> escalus_pred:is_iq_result(Req, Stanza) end,
    Resp = amoc_xmpp:send_request_and_get_response(Client, Req, Pred, response, 10000),
    ?LOG_DEBUG("~s got PEP publish response ~p", [escalus_client:username(Client), Resp]),
    amoc_metrics:update_counter(pubsub_items_published).

-spec item_content(escalus:client()) -> exml:element().
item_content(Client) ->
    #xmlel{
       name = ~"entry",
       attrs = #{~"timestamp" => integer_to_binary(os:system_time(microsecond)),
                 ~"jid" => escalus_client:short_jid(Client)},
       children = [#xmlcdata{content = cfg(message_body)}]}.

-spec node_name(pos_integer()) -> binary().
node_name(NodeId) ->
    iolist_to_binary([~"test-node-", integer_to_binary(NodeId)]).

-spec is_pep_notification(exml:element()) -> boolean().
is_pep_notification(Stanza = #xmlel{name = ~"message"}) ->
    EventXmlns = exml_query:path(Stanza, [{element, ~"event"}, {attr, ~"xmlns"}]),
    Items = exml_query:path(Stanza, [{element, ~"event"}, {element, ~"items"}]),
    EventXmlns =:= ?NS_PUBSUB_EVENT andalso Items =/= undefined;
is_pep_notification(_) ->
    false.

-spec record_pep_notification(escalus:client(), exml:element(),
                              escalus_connection:metadata()) -> ok.
record_pep_notification(Client, Stanza, Metadata) ->
    ?LOG_DEBUG("~s received PEP notification ~p", [escalus_client:username(Client), Stanza]),
    amoc_metrics:update_counter(pubsub_notifications_received),
    update_pubsub_item_ttd(Stanza, Metadata).

-spec update_pubsub_item_ttd(exml:element(), escalus_connection:metadata()) -> ok.
update_pubsub_item_ttd(Stanza, #{recv_timestamp := RecvTimestamp}) ->
    Timestamp = exml_query:path(Stanza, [{element, ~"event"},
                                         {element, ~"items"},
                                         {element, ~"item"},
                                         {element, ~"entry"},
                                         {attr, ~"timestamp"}]),
    amoc_metrics:update_time(pubsub_item_ttd, RecvTimestamp - binary_to_integer(Timestamp)).

cfg(Name) -> amoc_config:get(Name).
