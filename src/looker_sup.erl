-module(looker_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  snmpm:start(),

  Flags     = {one_for_one, 5, 10},
  LookerMon = {looker_mon, {looker_mon, start_link, []}, permanent, 10500, worker, [looker_mon]},
  {ok, { Flags , [LookerMon]} }.