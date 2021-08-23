%% @doc Each user performs the following steps:
%%   - Create the domain dynamically when needed (there is one creator for each domain)
%%   - Log in to the dynamically created domain (see dynamic_domains.erl)
%%   - Send presence: available (see amoc_xmpp_presence.erl)
%%   - Send chat messages to neighbours periodically (users are divided into buckets of neighbours)
%%   - Send presence: unavailable and disconnect

-module(dynamic_domains_pm).

-include_lib("kernel/include/logger.hrl").

-define(V(X), (fun amoc_config_validation:X/1)).

-required_variable(
   [#{name => message_count, default_value => 60, verification => ?V(nonnegative_integer),
      description => "Number of messages sent by each user"},
    #{name => message_interval, default_value => 10, verification => ?V(nonnegative_integer),
      description => "Interval (in seconds) between messages"}
   ]).

-behaviour(amoc_scenario).

-export([init/0, start/1]).

-spec init() -> ok.
init() ->
    ?LOG_INFO("init metrics"),
    dynamic_domains:init(),
    amoc_xmpp_presence:init(),
    amoc_metrics:init(counters, messages_sent),
    amoc_metrics:init(counters, messages_received),
    amoc_metrics:init(times, message_ttd),
    ok.

-spec start(amoc_scenario:user_id()) -> any().
start(MyId) ->
    Spec = amoc_xmpp_handlers:make_props(received_handler_spec(), sent_handler_spec()),
    {ok, Client, _} = dynamic_domains:connect_or_exit(MyId, Spec),
    amoc_xmpp_presence:start(Client),
    do(MyId, Client),
    amoc_xmpp_presence:stop(Client).

-record(state, {neighbours :: [amoc_scenario:user_id()]}).

-spec do(amoc_scenario:user_id(), escalus:client()) -> any().
do(MyId, Client) ->
    NeighbourIds = amoc_xmpp:bucket_neighbours(MyId, 10),
    TimeTable = timetable:new(chat, cfg(message_count), cfg(message_interval)),
    timetable:do(Client, fun send_stanza/3, TimeTable, #state{neighbours = NeighbourIds}).

%% One-to-one messages

-spec send_stanza(escalus:client(), chat, #state{}) -> #state{}.
send_stanza(Client, chat, State = #state{neighbours = [RecipientId | Rest]}) ->
    Msg = escalus_stanza:chat_to_with_id_and_timestamp(make_jid(RecipientId), <<"hello">>),
    escalus_connection:send(Client, Msg),
    State#state{neighbours = Rest ++ [RecipientId]}.

%% User helpers

-spec make_jid(amoc_scenario:user_id()) -> binary().
make_jid(Id) ->
    amoc_xmpp_users:make_jid(Id, dynamic_domains:domain_name(Id)).

%% Stanza handlers

sent_handler_spec() ->
    [{fun escalus_pred:is_chat_message/1, fun amoc_xmpp_handlers:measure_sent_messages/0} |
     amoc_xmpp_presence:sent_handler_spec()].

received_handler_spec() ->
    [{fun escalus_pred:is_chat_message/1, fun handle_chat_message/3} |
     amoc_xmpp_presence:received_handler_spec()].

handle_chat_message(Client, Stanza, Metadata) ->
    amoc_metrics:update_counter(messages_received),
    amoc_xmpp_handlers:measure_ttd(Client, Stanza, Metadata).

%% Config helpers

cfg(Name) ->
    convert(Name, amoc_config:get(Name)).

convert(message_interval, V) -> timer:seconds(V);
convert(_Name, V) -> V.
