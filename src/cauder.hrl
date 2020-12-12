-define(APP_NAME, "CauDEr").
-define(APP_URL, "https://github.com/mistupv/cauder-v2").
-define(APP_DB, 'cauder/database').

-define(ID_GAMMA, 0).

-ifdef(debug).
-define(LOG(X), io:format("{~p,~p}: ~p~n", [?MODULE, ?LINE, X])).
-define(TO_STRING(X), lists:flatten(io_lib:format("~p",[X]))).
-else.
-define(LOG(X), ok).
-define(TO_STRING(X), "").
-endif.

%% Name of the module that defines the forwards semantics.
-define(FWD_SEM, cauder_semantics_forwards).
%% Name of the module that defines the backwards semantics.
-define(BWD_SEM, cauder_semantics_backwards).

-define(RULE_SEQ,      seq).
-define(RULE_SELF,     self).
-define(RULE_SPAWN,    spawn).
-define(RULE_SEND,     send).
-define(RULE_RECEIVE, 'receive').
-define(RULE_END, 'end').
-define(RULE_REGISTER, register).
-define(RULE_UNREGISTER, unregister).



-define(REPLAY_DATA,  200).


-define(NOT_EXP,    not_exp).
-define(NULL_RULE,  null_rule).
-define(NULL_OPT,   null_opt).

-define(SCHED_RANDOM,      random).
-define(SCHED_PRIO_RANDOM, prio_random).

-define(CAUDER_GREEN, {34,139,34}).


% System
-record(sys, {
  % Global mailbox
  mail = [] :: [cauder_types:message()],
  % Pool of processes
  procs :: cauder_types:process_map(),
  % System log
  logs = maps:new() :: cauder_types:log_map(),
  trace = [] :: [cauder_types:trace()],
  roll = [],
  % Map
  map = [] :: [cauder_types:map_element()],
  % History Map
  hmap = [] :: [cauder_types:history_map()]
}).

%% Process
-record(proc, {
  % Process identifier
  pid :: cauder_types:proc_id(),
  % History
  hist = [] :: cauder_types:history(),
  % Call stack
  stack = [] :: cauder_types:stack(),
  % Environment
  env = maps:new() :: cauder_types:environment(),
  % List of expressions
  exprs :: [cauder_types:abstract_expr()],
  % The entry point function for this process
  spf :: mfa()
}).

%% Message
-record(msg, {
  % Target process identifier
  dest :: cauder_types:proc_id(),
  % Message
  val :: term(),
  % UID
  uid :: cauder_types:msg_id()
}).

% Option
-record(opt, {
  % integer
  pid :: cauder_types:proc_id(),
  % forward or backward
  sem :: cauder_types:semantics(),
  % seq, spawn, ...
  rule :: cauder_types:rule()
}).

% Trace
-record(trace, {
  type :: ?RULE_SPAWN | ?RULE_SEND | ?RULE_RECEIVE,
  from :: cauder_types:proc_id(),
  to :: undefined | cauder_types:proc_id(),
  val :: undefined | term(),
  time :: undefined | cauder_types:msg_id()
}).

% Replay info
-record(replay, {
  log_path :: file:filename(),
  call :: {module(), atom(), cauder_types:af_args()},
  main_pid :: cauder_types:proc_id()
}).

% Evaluation step result
-record(result, {
  env :: cauder_types:environment(),
  exprs :: [cauder_types:abstract_expr()],
  stack :: cauder_types:stack(),
  label = tau :: cauder_types:label()
}).
