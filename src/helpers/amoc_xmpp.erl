-module(amoc_xmpp).

-export([connect_or_exit/1]).
-export([connect_or_exit/2]).
-export([pick_server/1]).
-export([make_user/2]).
-export([send_request_and_get_response/5]).
-export([bucket_neighbours/2]).

-include_lib("kernel/include/logger.hrl").
-define(V(X), (fun amoc_config_validation:X/1)).

-required_variable([
    #{name => xmpp_servers, description => "the list of XMPP servers"},
    #{name => legacy_tls, description => "use legacy tls connection",
      default_value => false,  verification => ?V(boolean)}]).

%% @doc Connects and authenticates a user with the given id and additional properties.
%% If the passed proplist is empty, a default user spec created by
%% make_user function is passed.
%% If the proplist is not empty it will be merged with the default props.
%% When a property is defined both in default and passed props,
%% the one from passed props is used.
-spec connect_or_exit(amoc_scenario:user_id(), escalus_users:user_spec()) ->
    {ok, escalus_connection:client(), escalus_users:user_spec()}.
connect_or_exit(Id, ExtraSpec) ->
    Spec = make_user(Id, ExtraSpec),
    connect_or_exit(Spec).

%% @doc Connects and authenticates a user based on the given user spec.
%% This function exits if the connection or authentication was not successful.
-spec connect_or_exit(escalus_users:user_spec()) ->
    {ok, escalus_connection:client(), escalus_users:user_spec()}.
connect_or_exit(Spec) ->
    NewSpec = maybe_use_legacy_tls(Spec),
    {ConnectionTime, ConnectionResult} = timer:tc(escalus_connection, start, [NewSpec]),
    case ConnectionResult of
        {ok, _, _} = Result ->
            amoc_metrics:update_counter(connections),
            amoc_metrics:update_time(connection, ConnectionTime),
            Result;
        Error ->
            amoc_metrics:update_counter(connection_failures),
            ?LOG_ERROR("Could not connect user=~p, reason=~p", [Spec, Error]),
            exit(connection_failed)
    end.

maybe_use_legacy_tls(Spec) ->
    case amoc_config:get(legacy_tls) of
        false -> Spec;
        true -> lists:keystore(ssl, 1, Spec, {ssl, true})
    end.

%% @doc Picks a random server based on the config var `xmpp_servers'.
%% This function expects a list of proplists defining the endpoint
%% to which an XMPP client can connect, for instance:
%% [[{host, "127.0.0.1"}, {port, 5222}], [{host, "127.0.0.1"}, {port, 5223}]]
%% One of the above proplists is picked and can be added to user's spec.
%% It's required that the proplists contains at least the `host' property.
%% Since the `xmpp_servers' config option is read via `amoc_config' API,
%% it's possible to pass it as an ENV var when starting amoc:
%% ```> AMOC_xmpp_servers="[[{host,\"127.0.0.2\"}, {port, 5222}],[{host, \"127.0.0.1\"}, {port, 5223}]]" make console'''
-spec pick_server([[proplists:property()]]) -> [proplists:property()].
pick_server(DefaultServers) ->
    Servers = amoc_config:get(xmpp_servers, DefaultServers),
    verify(Servers),
    S = length(Servers),
    N = erlang:phash2(self(), S) + 1,
    lists:nth(N, Servers).

verify(Servers) ->
    lists:foreach(
      fun(Proplist) ->
              true = proplists:is_defined(host, Proplist)
      end,
      Servers
     ).

-spec make_user(amoc_scenario:user_id(), escalus_users:user_spec()) -> escalus_users:user_spec().
make_user(Id, Props) ->
    ProfileId = amoc_xmpp_users:username(Id),
    Password = amoc_xmpp_users:password(Id),
    DefaultSpec = maps:from_list(default_user_spec(ProfileId, Password)),
    ExtraSpec = maps:from_list(Props),
    Merged = maps:merge(DefaultSpec, ExtraSpec),
    maps:to_list(Merged).

-spec default_user_spec(binary(), binary()) -> escalus_users:user_spec().
default_user_spec(ProfileId, Password) ->
    [{username, ProfileId},
     {server, <<"localhost">>},
     {password, Password},
     {carbons, false},
     {stream_management, false},
     {resource, base64:encode(crypto:strong_rand_bytes(5))}] ++
        pick_server([[{host, "127.0.0.1"}]]).

-spec send_request_and_get_response(
        escalus:client(), exml:element(), escalus_connection:stanza_pred(), amoc_metrics:name(), timeout()
       ) -> exml_stream:element().
send_request_and_get_response(Client, Req, Pred, TimeMetric, Timeout) ->
    SendTimestamp = os:system_time(microsecond),
    Options = #{pred => Pred,
                timeout => Timeout,
                safe => true,
                with_metadata => true},
    case escalus_connection:send_and_receive(Client, Req, Options) of
        {error, timeout} ->
            amoc_metrics:update_counter(timeouts),
            error(timeout_when_waiting_for_response, [Client, Req, Pred]);
        {Stanza, #{recv_timestamp := RecvTimestamp}} ->
            amoc_metrics:update_time(TimeMetric, RecvTimestamp - SendTimestamp),
            Stanza
    end.

%% @doc Divide users into buckets of BucketSize and return all users from the bucket except Id.
%% When the total user number is divisible by BucketSize, it makes the total number of messages
%% easy to predict and check.
-spec bucket_neighbours(amoc_scenario:user_id(), pos_integer()) -> [amoc_scenario:user_id()].
bucket_neighbours(Id, BucketSize) ->
    PositionInBucket = (Id - 1) rem BucketSize,
    BucketStartId = Id - PositionInBucket,
    lists:delete(Id, lists:seq(BucketStartId, BucketStartId + BucketSize - 1)).
