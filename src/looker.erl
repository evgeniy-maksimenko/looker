-module(looker).

%% API
-export([
  start/0,
  stop/0
]).

start() ->
  ok = application:start(looker),
  ok.
stop() ->
  application:stop(?MODULE).
