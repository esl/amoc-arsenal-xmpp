-module(dynamic_domains_mam_muc_lookup).

-include_lib("kernel/include/logger.hrl").
-include_lib("exml/include/exml.hrl").

-define(V(X), (fun amoc_config_validation:X/1)).

-required_variable(
   [#{name => max_items_per_lookup, default_value => 5, verification => ?V(nonnegative_integer),
      description => "Maximum number of messages returned per lookup"},
    #{name => lookups_per_room, default_value => 36, verification => ?V(nonnegative_integer),
      description => "Number of MAM lookups per room"},
    #{name => room_lookup_interval, default_value => 10, verification => ?V(nonnegative_integer),
      description => "Interval (in seconds) between lookups for each room"}
   ]).

-behaviour(amoc_scenario).

-export([init/0, start/1]).

-spec init() -> ok.
init() ->
    ?LOG_INFO("init metrics"),
    amoc_xmpp_presence:init(),
    amoc_metrics:init(counters, mam_messages_received),
    amoc_metrics:init(counters, mam_lookups),
    amoc_metrics:init(counters, timeouts),
    amoc_metrics:init(times, mam_lookup_response_time),
    ok.

-record(state, {last_mam_ids = #{}}).

-spec start(amoc_scenario:user_id()) -> any().
start(MyId) ->
    Spec = amoc_xmpp_handlers:make_props(received_handler_spec(), sent_handler_spec()),
    {ok, Client, _} = dynamic_domains:connect_or_exit(MyId, Spec),
    amoc_xmpp_presence:start(Client),
    do(MyId, Client),
    amoc_xmpp_presence:stop(Client).

-spec do(amoc_scenario:user_id(), escalus:client()) -> any().
do(MyId, Client) ->
    RoomIds = amoc_xmpp_muc:rooms_to_create(MyId, cfg(rooms_per_user), cfg(users_per_room)),
    TimeTable = lookup_timetable(MyId, RoomIds),
    timetable:do(Client, fun send_stanza/3, TimeTable, #state{}).

lookup_timetable(MyId, RoomIds) ->
    timetable:merge([room_lookup_timetable(MyId, RoomId) || RoomId <- RoomIds]).

room_lookup_timetable(MyId, RoomId) ->
    RoomJid = room_jid(RoomId, dynamic_domains:domain_name(MyId)),
    timetable:new({mam_lookup, RoomJid},
                  cfg(lookups_per_room), cfg(room_lookup_interval)).

send_stanza(Client, {mam_lookup, RoomJid}, State = #state{last_mam_ids = Ids}) ->
    Id = maps:get(RoomJid, Ids, undefined),
    NewId = get_mam_messages(Client, RoomJid, Id),
    State#state{last_mam_ids = Ids#{RoomJid => NewId}}.

%% MAM requests

get_mam_messages(Client, RoomJid, LastId) ->
    Max = cfg(max_items_per_lookup),
    SetChild = #xmlel{name = <<"set">>,
                      attrs = [{<<"xmlns">>, <<"http://jabber.org/protocol/rsm">>}],
                      children = [#xmlel{name = <<"max">>,
                                         children = [#xmlcdata{content = integer_to_binary(Max)}]}
                                 | after_elements(LastId)]},
    Req0 = escalus_stanza:iq(<<"set">>,
                            [#xmlel{name = <<"query">>,
                                    attrs = [{<<"xmlns">>,  <<"urn:xmpp:mam:2">>}],
                                    children = [SetChild]}]),
    Req = escalus_stanza:to(Req0, RoomJid),
    Pred = fun(Stanza) -> escalus_pred:is_iq_result(Req, Stanza) end,
    Metric = mam_lookup_response_time,
    Timeout = timer:seconds(10),
    Response = amoc_xmpp:send_request_and_get_response(Client, Req, Pred, Metric, Timeout),
    get_last_id(Response).

after_elements(undefined) -> [];
after_elements(Last) -> [#xmlel{name = <<"after">>, children = [#xmlcdata{content = Last}]}].

%% Wrap around if the next result set would be incomplete
get_last_id(Stanza) ->
    [SetEl] = exml_query:paths(Stanza, [{element, <<"fin">>}, {element, <<"set">>}]),
    [FirstIndex] = exml_query:paths(SetEl, [{element, <<"first">>}, {attr, <<"index">>}]),
    [CountBin] = exml_query:paths(SetEl, [{element, <<"count">>}, cdata]),
    Count = binary_to_integer(CountBin),
    case binary_to_integer(FirstIndex) + 2*cfg(max_items_per_lookup) =< Count of
        true -> exml_query:path(SetEl, [{element, <<"last">>}, cdata]);
        false -> undefined
    end.

%% Room helpers

room_jid(RoomId, Domain) ->
    <<(room_name(RoomId))/binary, $@, (muc_host(Domain))/binary>>.

room_name(RoomId) ->
    <<"room_", (integer_to_binary(RoomId))/binary>>.

muc_host(Domain) ->
    <<"muclight.", Domain/binary>>.

%% Stanza handlers

sent_handler_spec() ->
    amoc_xmpp_presence:sent_handler_spec().

received_handler_spec() ->
    [{fun is_mam_archived_message/1,
      fun() -> amoc_metrics:update_counter(mam_messages_received) end} |
     amoc_xmpp_presence:received_handler_spec()].

is_mam_archived_message(#xmlel{} = Stanza) ->
    M = exml_query:path(Stanza, [{element, <<"result">>},
                                 {element, <<"forwarded">>},
                                 {element, <<"message">>}]),
    escalus_pred:is_groupchat_message(M).

%% Config helpers

cfg(Name) ->
    convert(Name, amoc_config:get(Name)).

convert(room_lookup_interval, V) -> timer:seconds(V);
convert(_Name, V) -> V.
