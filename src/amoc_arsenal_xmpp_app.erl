-module(amoc_arsenal_xmpp_app).

-behaviour(application).

-export([start/2, stop/1]).

-spec start(application:start_type(), term()) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    Ret = amoc_arsenal_xmpp_sup:start_link(),
    amoc_metrics:start(),
    amoc_api:start(),
    Ret.

-spec stop(term()) -> ok.
stop(_State) ->
    ok.
