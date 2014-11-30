-module(looker_mon).
-behaviour(gen_server).

-export([start_link/0, rx/0, tx/0, ifOperStatus/0, sysUpTime/0]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-export([sync_get/0]).

-include("../include/looker.hrl").
-define(tcpConnState, {tcpConnState, [1,3,6,1,2,1,6,13,1,1]}).
-define(ifInOctets, {ifInOctets, [1,3,6,1,2,1,2,2,1,10]}).
-define(ifOutOctets, {ifOutOctets, [1,3,6,1,2,1,2,2,1,16]}).
-define(ifOperStatus, {ifOperStatus, [1,3,6,1,2,1,2,2,1,8]}).
-define(sysUpTime, {sysUpTime, [1,3,6,1,2,1,1,3]}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% =====================================================================================================================
%% API
%% =====================================================================================================================
sync_get() -> gen_server:call(?MODULE, ?tcpConnState).
rx() -> gen_server:call(?MODULE, ?ifInOctets).
tx() -> gen_server:call(?MODULE, ?ifOutOctets).
ifOperStatus() -> gen_server:call(?MODULE, ?ifOperStatus).
sysUpTime() -> gen_server:call(?MODULE, ?sysUpTime).

%% =====================================================================================================================
%% Callbacks
%% =====================================================================================================================
init([]) ->
  {ok,Config} = application:get_env(?APP_NAME, ?APP_MANAGER_CONF),
  looker_service:initialize(looker_snmp:register_user(?USER_ID, snmpm_user_default, undefined), Config).

handle_call({FunOid, Oids}, _From, State) ->
  Reply = looker_service:parseResponse(FunOid, looker_service:reply_registering_agent(looker_service:registering_agent(State#state.manager_conf), FunOid, Oids)),
  {reply, lists:sort(Reply), State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

