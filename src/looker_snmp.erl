-module(looker_snmp).
-export([register_user/3, register_agent/3, unregister_agent/2, sync_get_next/3]).

-type id()                  :: term().
-type snmp_user_module()    :: snmpm_user().
-type data()                :: term().
-type snmpm_user()          :: atom().
-type reason()              :: ok | reasonTp().
-type reasonTp()            :: {error, tuple()}.
-type userId()              :: term().
-type targetName()          :: term().
-type config()              :: [agent_config()].
-type agent_config()        :: {item(), val()}.
-type val()                 :: term().
-type oids()                :: [oid()].
-type oid()                 :: [byte()].
-type reasonGetNext()       :: {ok, snmpReply(), remaining()} | reasonGetNextTp().
-type remaining()           :: integer().
-type snmpReply()           :: snmp_reply().
-type snmp_reply()          :: {error_status(), error_index(), varbinds()}.
-type varbinds()            :: list().
-type error_status()        :: noError | atom().
-type error_index()         :: integer().
-type reasonGetNextTp()     :: {error, reasonErrorGetNext()}.
-type reasonErrorGetNext()  :: {send_failed, reqId(), r()} | {invalid_sec_info, secInfo(), snmpInfo()} | term().
-type r()                   :: term().
-type reqId()               :: integer().
-type secInfo()             :: [sec_info()].
-type sec_info()            :: {sec_tag(), expectedValue(), receivedValue()}.
-type sec_tag()             :: atom().
-type expectedValue()       :: term().
-type receivedValue()       :: term().
-type snmpInfo()            :: term().
-type item()                :: engine_id
| address
| port
| community
| timeout
| max_message_size
| version
| sec_model
| sec_name
| sec_level
| tdomain.

-spec register_user(id(), snmp_user_module(), data()) -> reason().
register_user(Id, Module, Data) ->
  snmpm:register_user(Id, Module, Data).

-spec register_agent(userId(), targetName(), config()) -> reason().
register_agent(UserId, TargetName, Config) ->
  snmpm:register_agent(UserId, TargetName, Config).

-spec unregister_agent(userId(), targetName()) -> reason().
unregister_agent(UserId, TargetName) ->
  snmpm:unregister_agent(UserId, TargetName).

-spec sync_get_next(userId(), targetName(), oids()) -> reasonGetNext().
sync_get_next(UserId, TargetName, Oids) ->
  snmpm:sync_get_next(UserId, TargetName, Oids).
