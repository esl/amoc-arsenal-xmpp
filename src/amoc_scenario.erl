%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_scenario).

-export_type([user_id/0, state/0]).

-type user_id() :: non_neg_integer().
-type state() :: any().

-callback init() -> {ok, state()} | ok | {error, Reason :: term()}.
-callback start(user_id(), state()) -> any().
-callback start(user_id()) -> any().

%% either start/1 or start/2 must be exported from the behaviour module
-optional_callbacks([start/1, start/2]).

