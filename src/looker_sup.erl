-module(looker_sup).
-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->

  snmpm:start(),
  RegisterUser = snmpm:register_user("simple_user", snmpm_user_default, undefined),
  io:format("~nRegisterUser = ~p~n", [RegisterUser]),
  Options = [
              {address, [127,0,0,1]},
              {engine_id, "the_engine"},
              %{max_message_size, 484},
              {community, "public"},
              {version, v2}
            ],
  RegisterAgent = snmpm:register_agent("simple_user","simple_agent", Options),
  io:format("~nRegisterAgent = ~p~n", [RegisterAgent]),


  SyncGet = snmpm:sync_get("simple_user","simple_agent",[[1,3,6,1,2,1,1,5,0]]),
  io:format("~nSyncGet = ~p~n", [SyncGet]),

  Flags = {one_for_one, 5, 10},

  {ok, { Flags , []} }.

