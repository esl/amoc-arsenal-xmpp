-module(dynamic_domains).

-export([init/0,
         connect_or_exit/2,
         connect_or_exit/3,
         get_rest_api_host/0,
         domain_name/1,
         domain_id/1]).

-define(V(X), (fun amoc_config_validation:X/1)).

-required_variable(
   [#{name => domain_count, default_value => 10000, verification => ?V(positive_integer),
      description => "Number of dynamic domains"},
    #{name => domain_bucket_size, default_value => 1, verification => ?V(positive_integer),
      description => "Number of consecutive users belonging to the same domain"},
    #{name => domain_prefix, default_value => <<"test-domain-">>, verification => ?V(binary),
      description => "Prefix of the dynamic domain names"},
    #{name => rest_port, default_value => 8088, verification => ?V(positive_integer),
      description => "Port number of the REST API for dynamic domains"},
    #{name => rest_host, verification => ?V(binary_or_undefined),
      description => "Host name of the REST API for dynamic domains"},
    #{name => host_type, default_value => <<"localhost">>, verification => ?V(binary),
      description => "Host type for the created domains"},
    #{name => wait_time_for_domain_creation, default_value => 60,
      verification => ?V(nonnegative_integer),
      description => "Time to wait until the domain is created by the first user (in seconds)"}
   ]).

-type connect_opts() :: #{create_domain => boolean()}.

-spec init() -> any().
init() ->
    amoc_metrics:init(counters, domain_creation_requests),
    amoc_metrics:init(times, domain_creation_time).

-spec connect_or_exit(amoc_scenario:user_id(), escalus_users:user_spec()) ->
          {ok, escalus_connection:client(), escalus_users:user_spec()}.
connect_or_exit(Id, ExtraSpec) ->
    connect_or_exit(Id, ExtraSpec, #{}).

-spec connect_or_exit(amoc_scenario:user_id(), escalus_users:user_spec(), connect_opts()) ->
          {ok, escalus_connection:client(), escalus_users:user_spec()}.
connect_or_exit(Id, ExtraSpec, Opts) ->
    Spec = amoc_xmpp:make_user(Id, [{server, dynamic_domains:domain_name(Id)} | ExtraSpec]),
    maybe_create_domain(Id, Spec, Opts),
    amoc_xmpp:connect_or_exit(Spec).

-spec maybe_create_domain(amoc_scenario:user_id(), escalus_users:user_spec(), connect_opts()) -> ok.
maybe_create_domain(UserId, UserSpec, Opts) ->
    case should_create_domain(UserId, Opts) of
        true ->
            Host = get_rest_api_host(),
            Domain = proplists:get_value(server, UserSpec),
            amoc_metrics:update_counter(domain_creation_requests),
            {Time, _} = timer:tc(fun create_domain/2, [Host, Domain]),
            amoc_metrics:update_time(domain_creation_time, Time);
        false ->
            not_needed
    end,
    timer:sleep(cfg(wait_time_for_domain_creation)).

-spec should_create_domain(amoc_scenario:user_id(), connect_opts()) -> boolean().
should_create_domain(_UserId, #{create_domain := Value}) -> Value;
should_create_domain(UserId, #{}) -> is_first_user_in_domain(UserId).

-spec is_first_user_in_domain(amoc_scenario:user_id()) -> boolean().
is_first_user_in_domain(UserId) ->
    BucketSize = cfg(domain_bucket_size),
    PositionInBucket = (UserId - 1) rem BucketSize,
    BucketIdFromZero = (UserId - 1) div BucketSize,
    PositionInBucket == 0 andalso BucketIdFromZero < cfg(domain_count).

-spec create_domain(binary(), binary()) -> ok.
create_domain(Host, Domain) ->
    {ok, Conn} = gun:open(binary_to_list(Host), cfg(rest_port)),
    Path = <<"/api/domains/", Domain/binary>>,
    Body = <<"{\"host_type\": \"", (cfg(host_type))/binary, "\"}">>,
    Stream = gun:put(Conn, Path, [{<<"content-type">>, <<"application/json">>}], Body),
    {response, fin, 204, _} = gun:await(Conn, Stream),
    gun:close(Conn).

-spec domain_name(amoc_scenario:user_id()) -> binary().
domain_name(UserId) ->
    Prefix = cfg(domain_prefix),
    BinId = integer_to_binary(domain_id(UserId)),
    <<Prefix/binary, BinId/binary>>.

-spec domain_id(amoc_scenario:user_id()) -> pos_integer().
domain_id(UserId) ->
    BucketSize = cfg(domain_bucket_size),
    BucketIdFromZero = (UserId - 1) div BucketSize,
    BucketIdFromZero rem cfg(domain_count) + 1.

get_rest_api_host() ->
    case cfg(rest_host) of
        undefined ->
            proplists:get_value(host, amoc_xmpp:pick_server([]));
        Binary ->
            Binary
    end.

cfg(Name) ->
    convert(Name, amoc_config:get(Name)).

convert(wait_time_for_domain_creation, V) -> timer:seconds(V);
convert(_Name, V) -> V.
