-module(cauder_types).

-include("cauder.hrl").

-export_type([
  system/0,
  msg_id/0, message/0,
  log_map/0, log/0, log_entry/0,
  process_map/0, proc_id/0, process/0,
  history/0, history_entry/0,
  stack/0, stack_entry/0,
  environment/0, binding/0,
  option/0, semantics/0, rule/0,
  trace/0,
  result/0, label/0
]).

% Abstract format types
-export_type([line/0, abstract_expr/0, af_args/0, af_literal/0, af_boolean/0, af_variable/0, af_remote_call/0, af_clause_seq/0, af_clause/0, af_pattern/0, af_guard_seq/0, af_guard/0, af_guard_test/0, af_body/0]).


%% Record types

-type system() :: #sys{}.

-type msg_id() :: pos_integer().
-type message() :: #msg{}.

-type log_map() :: #{proc_id() => log()}.
-type log() :: [log_entry()].
-type log_entry() :: {spawn, proc_id()}
                   | {send, msg_id()}
                   | {'receive', msg_id()}.

-type process_map() :: #{proc_id() := process()}. % Not empty
-type proc_id() :: pos_integer() | atom().
-type process() :: #proc{}.
-type map_element() :: {atom(), proc_id()}.


-type history() :: [history_entry()].
-type history_entry() :: {tau, environment(), [abstract_expr()], stack()}
                       | {self, environment(), [abstract_expr()], stack()}
                       | {spawn, environment(), [abstract_expr()], stack(), proc_id()}
                       | {send, environment(), [abstract_expr()], stack(), message()}
                       | {sendA, environment(), [abstract_expr()], stack(), message(), map_element()}
                       | {sendF, environment(), [abstract_expr()], stack(), atom()}
                       | {rec, environment(), [abstract_expr()], stack(), message()}
                       | {'end', environment(), [abstract_expr()], stack(), map_element()}
                       | {fail, environment(), [abstract_expr()], stack()}
                       | {registerT, environment(), [abstract_expr()], stack(), map_element()}
                       | {registerF, environment(), [abstract_expr()], stack(), map_element()}
                       | {unregisterT, environment(), [abstract_expr()], stack(), map_element()}
                       | {unregisterF, environment(), [abstract_expr()], stack(), atom()}
                       | {tauM, environment(), [abstract_expr()], stack(), [map_element()]}.



-type history_map() :: [history_map_entry()].
-type history_map_entry() :: {'end',       [map_element()], proc_id(), []}
                           | {registerT,   [map_element()], proc_id(), []}
                           | {registerF,   [map_element()], proc_id(), []}
                           | {unregisterT, [map_element()], proc_id(), []}
                           | {unregisterF, [atom()],        proc_id(), []}
                           | {tauM,        [map_element()], proc_id(), []}
                           | {sendF,       [atom()],        proc_id(), []}
                           | {sendA,       [map_element()], proc_id(), message()}.

-type stack() :: [stack_entry()].
-type stack_entry() :: {mfa(), environment(), [abstract_expr()], af_variable()}
                     | {atom(), [abstract_expr()], af_variable()}.

-type environment() :: #{atom() => term()}.
-type binding() :: {atom(), term()}.

-type option() :: #opt{}.
-type semantics() :: ?FWD_SEM | ?BWD_SEM.
-type rule() :: ?RULE_SEQ | ?RULE_SELF | ?RULE_SPAWN | ?RULE_SEND | ?RULE_RECEIVE | ?RULE_END | ?RULE_REGISTER | ?RULE_UNREGISTER.

-type trace() :: #trace{}.

-type result() :: #result{}.

-type label() :: tau
               | {spawn, af_variable(), function()}
               | {spawn, af_variable(), module(), atom(), [term()]}
               | {self, af_variable()}
               | {send, proc_id(), term(), line()}
               | {rec, af_variable(), af_clause_seq()}
               | {bottom, line(), term()}
               | {register, atom(), line(),atom(), proc_id()}
               | {unregister, atom(), line(), atom()}
               | {tauM, term()}.



%% Custom of abstract format

-type line() :: non_neg_integer().

-type abstract_expr() :: af_literal()
                       | af_variable()
                       | af_variable()
                       | af_cons(abstract_expr())
                       | af_tuple(abstract_expr())
                       | af_if()
                       | af_case()
                       | af_receive()
                       | af_make_fun()
                       | af_bif_call()
                       | af_self_call()
                       | af_spawn_1_call()
                       | af_spawn_3_call()
                       | af_send_call()
                       | af_local_call()
                       | af_remote_call()
                       | af_apply()
                       | af_apply_fun()
                       | af_match(abstract_expr())
                       | af_op(abstract_expr())
                       | af_short_circuit_op(abstract_expr())
                       | af_register()
                       | af_unregister()
                       | af_tauM().

-type af_args() :: [abstract_expr()].

-type af_literal() :: {value, line(), term()}.

-type af_boolean() :: {value, line(), true | false}.

-type af_variable() :: {var, line(), atom()}.

-type af_cons(T) :: {cons, line(), T, T}.

-type af_tuple(T) :: {tuple, line(), [T]}.

-type af_if() :: {'if', line(), af_clause_seq()}.

-type af_case() :: {'case', line(), abstract_expr(), af_clause_seq()}.

-type af_receive() :: {'receive', line(), af_clause_seq()}.

-type af_make_fun() :: {make_fun, line(), atom(), af_clause_seq()}.

-type af_bif_call() :: {bif, line(), module(), atom(), af_args()}.

-type af_self_call() :: {self, line()}.

-type af_spawn_1_call() :: {spawn, line(), abstract_expr()}.

-type af_spawn_3_call() :: {spawn, line(), abstract_expr(), abstract_expr(), abstract_expr()}.

-type af_send_call() :: {send, line(), abstract_expr(), abstract_expr()}.

-type af_local_call() :: {local_call, line(), atom(), af_args()}.

-type af_remote_call() :: {remote_call, line(), module(), atom(), af_args()}.

-type af_apply() :: {apply, line(), abstract_expr(), abstract_expr(), af_args()}.

-type af_apply_fun() :: {apply_fun, line(), abstract_expr(), af_args()}.

-type af_match(T) :: {match, line(), af_pattern(), T}.

-type af_op(T) :: {op, line(), unary_op() | binary_op(), [T]}.

-type af_unary_arith_op(T) :: {op, line(), '+' | '-', [T]}.

-type af_short_circuit_op(T) :: {'andalso' | 'orelse', line(), T, T}.

-type af_register() :: {register, line(), abstract_expr(), abstract_expr()}.

-type af_unregister() :: {unregister, line(), abstract_expr()}.

-type af_tauM() :: {tauM, line(), af_args()}.

%% Clauses

-type af_clause_seq() :: [af_clause(), ...].

-type af_clause() :: {'clause', line(), [af_pattern()], af_guard_seq(), af_body()}.

-type af_pattern() :: af_literal()
                    | af_variable()
                    | af_cons(af_pattern())
                    | af_tuple(af_pattern())
                    | af_match(af_pattern())
                    | af_unary_arith_op(af_pattern()).

-type af_guard_seq() :: [af_guard()].

-type af_guard() :: [af_guard_test(), ...].

-type af_guard_test() :: af_literal()
                       | af_variable()
                       | af_cons(af_guard_test())
                       | af_tuple(af_guard_test())
                       | af_unary_arith_op(af_guard_test())
                       | af_short_circuit_op(af_guard_test())
                       | af_guard_call()
                       | af_self_call().

-type af_guard_call() :: {'bif', line(), erlang, atom(), [af_guard_test()]}.

-type af_body() :: [abstract_expr(), ...].


%% Operators

-type binary_op() :: '/' | '*' | 'div' | 'rem' | 'band' | 'and' | '+' | '-'
                   | 'bor' | 'bxor' | 'bsl' | 'bsr' | 'or' | 'xor' | '++'
                   | '--' | '==' | '/=' | '=<' | '<'  | '>=' | '>' | '=:='
                   | '=/='.

-type unary_op() :: '+' | '-' | 'bnot' | 'not'.

%% End of custom abstract format
