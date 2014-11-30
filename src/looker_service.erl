-module(looker_service).
-export([initialize/2, registering_agent/1, reply_registering_agent/3, ifInOctets/1, ifOutOctets/1, ifOperStatus/1, sysUpTime/1]).
-export([tcpConnState/1, getDescrTcpConnStateValues/1, parseResponse/2]).

-define(SEPARATOR,".").
-include("../include/looker.hrl").

initialize(ok, Config)                -> {ok, #state{manager_conf = Config}};
initialize({error, Reason}, _Config)  -> {failed_register_user, Reason}.

registering_agent(Config) ->
  case is_agent_register(snmpm:which_agents(?USER_ID)) of
    ok -> looker_snmp:register_agent(?USER_ID, ?TARGET_NAME, proplists:get_value(options, Config));
    _ = Reason -> Reason
  end.

is_agent_register([]) -> ok;
is_agent_register(_)  -> looker_snmp:unregister_agent(?USER_ID, ?TARGET_NAME).

reply_registering_agent(ok, FunOid, Oids) -> ?MODULE:FunOid(Oids);
reply_registering_agent(Reason, _FunOid, _Oids) -> Reason.

%TODO нужно уберать дублирование кода
ifInOctets(Oid) -> ifInOctets(Oid,[]).
ifInOctets([1,3,6,1,2,1,2,2,1,11| _ ], Acc) -> delete_last_lists(Acc);
ifInOctets(Oid, Acc) ->
  {OID,Type,Value} = get_next_sync(Oid),
  ifInOctets(OID, [{OID, Type, Value}|Acc]).

ifOutOctets(Oid) -> ifOutOctets(Oid,[]).
ifOutOctets([1,3,6,1,2,1,2,2,1,17| _ ], Acc) -> delete_last_lists(Acc);
ifOutOctets(Oid, Acc) ->
  {OID,Type,Value} = get_next_sync(Oid),
  ifOutOctets(OID, [{OID, Type, Value}|Acc]).

tcpConnState(Oid) -> tcpConnState(Oid, []).
tcpConnState([1,3,6,1,2,1,6,13,1,2| _ ], Acc) -> delete_last_lists(Acc);
tcpConnState(Oid, Acc) ->
  {OID,Type,Value} = get_next_sync(Oid),
  tcpConnState(OID, [{OID, Type, Value}|Acc]).

ifOperStatus(Oid) -> ifOperStatus(Oid, []).
ifOperStatus([1,3,6,1,2,1,2,2,1,9| _ ], Acc) -> delete_last_lists(Acc);
ifOperStatus(Oid, Acc) ->
  {OID,Type,Value} = get_next_sync(Oid),
  ifOperStatus(OID, [{OID, Type, Value}|Acc]).

sysUpTime(Oid) -> sysUpTime(Oid, []).
sysUpTime([1,3,6,1,2,1,1,4| _ ], Acc) -> delete_last_lists(Acc);
sysUpTime(Oid, Acc) ->
  {OID,Type,Value} = get_next_sync(Oid),
  sysUpTime(OID, [{OID, Type, Value}|Acc]).

parseResponse(FunOid, Reply) ->
  [generate_struct_resp(FunOid, List, Value) || {List, _, Value} <- Reply].

generate_struct_resp(tcpConnState, List, Value) ->
  [M1,M2,M3,M4] = lists:sublist(List, 11, 4),
  ManagerPORT   = lists:nth(15, List),
  [A1,A2,A3,A4] = lists:sublist(List, 16, 4),
  AgentPORT     = lists:nth(20, List),
  [
    {<<"status">>, list_to_binary(atom_to_list(getDescrTcpConnStateValues(Value)))},
    {<<"manager_ip">>,list_to_binary([integer_to_list(M1),?SEPARATOR,integer_to_list(M2),?SEPARATOR,integer_to_list(M3),?SEPARATOR,integer_to_list(M4)])},
    {<<"manager_port">>, ManagerPORT},
    {<<"agent_ip">>,list_to_binary([integer_to_list(A1),?SEPARATOR,integer_to_list(A2),?SEPARATOR,integer_to_list(A3),?SEPARATOR,integer_to_list(A4)])},
    {<<"agent_port">>, AgentPORT}
  ];
generate_struct_resp(ifInOctets, List, Value) ->
  [
    {<<"rx">>, lists:last(List)},
    {<<"value">>, Value}
  ];
generate_struct_resp(ifOutOctets, List, Value) ->
  [
    {<<"tx">>, lists:last(List)},
    {<<"value">>, Value}
  ];
generate_struct_resp(ifOperStatus, List, Value) ->
  [
    {<<"eth">>, lists:last(List)},
    {<<"value">>, list_to_binary(atom_to_list(getDescrIfOperStatus(Value)))}
  ];
generate_struct_resp(sysUpTime, _List, Value) ->
  [
    {"value", Value}
  ].

getDescrIfOperStatus(Value) ->
  List = [
    {1 , up},
    {2 , down},
    {3 , testing},
    {4 , unknown},
    {5 , dormant},
    {6 , notPresent},
    {7 , lowerLayerDown}
  ],
  proplists:get_value(Value, List).

getDescrTcpConnStateValues(Value) ->
  List = [
    {1 , closed},
    {2 , listen},
    {3 , synSent},
    {4 , synReceived},
    {5 , established},
    {6 , finWait1},
    {7 , finWait2},
    {8 , closeWait},
    {9 , lastAck},
    {10 , closing},
    {11 , timeWait},
    {12 , deleteTCB}
  ],
  proplists:get_value(Value, List).

delete_last_lists(Acc) ->
  lists:delete(lists:last(lists:reverse(Acc)),lists:reverse(Acc)).

get_next_sync(Oid) ->
  {ok,{_,_,[{_,OID,Type,Value,_}]},_} = looker_snmp:sync_get_next(?USER_ID, ?TARGET_NAME,[Oid]),
  {OID,Type,Value}.