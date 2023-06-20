-module(amoc_arsenal_xmpp_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-spec start_link() -> {ok, Pid :: pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init(term()) ->
    {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
                       MaxR :: non_neg_integer(), MaxT :: pos_integer()},
          [ChildSpec :: supervisor:child_spec()]
    }}.
init([]) ->
    {ok, {{one_for_one, 5, 10}, []}}.
