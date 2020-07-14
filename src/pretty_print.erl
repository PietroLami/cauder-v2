%%%-------------------------------------------------------------------
%%% @doc Pretty printing utility functions for cauder systems
%%% @end
%%%-------------------------------------------------------------------
-module(pretty_print).

-export([system/2, system_trace/1, roll_log/1]).
-export([pid/1, expression/1, to_string/1]).

-include("cauder.hrl").
-include_lib("wx/include/wx.hrl").

%% =====================================================================
%% @doc Pretty-prints a given System

-spec system(cauder_types:system(), [{cauder_types:print_option(), boolean()}]) -> string().

system(#sys{msgs = Msgs, procs = Procs}, Opts) ->
  messages(Msgs) ++ "\n" ++ processes(Procs, Opts).


-spec messages([cauder_types:message()]) -> string().

messages(Msgs) ->
  MsgsList = [message(Msg) || Msg <- Msgs],
  "GM : [" ++ string:join(MsgsList, ",") ++ "]\n".


-spec message(cauder_types:message()) -> string().

message(#msg{dest = DestPid, val = MsgValue, time = Time}) ->
  "(" ++ integer_to_list(DestPid) ++ ",{" ++ io_lib:format("~p", [MsgValue]) ++ "," ++ [{?wxRED, integer_to_list(Time)}] ++ "})".


-spec processes([cauder_types:process()], [{cauder_types:print_option(), boolean()}]) -> string().

processes(Procs, Opts) ->
  SortProcs = lists:sort(fun(P1, P2) -> P1#proc.pid < P2#proc.pid end, Procs),
  ProcsList = [process(Proc, Opts) || Proc <- SortProcs],
  string:join(ProcsList, "\n").


-spec process(cauder_types:process(), [{cauder_types:print_option(), boolean()}]) -> string().

process(#proc{pid = Pid, hist = Hist, env = Env, exprs = Es0, stack = Stack, mail = Mail, spf = MFA}, Opts) ->
  Es = cauder_syntax:to_abstract_expr(Es0),

  header(Pid, MFA) ++
  process_messages(Mail, Opts) ++
  history(Hist, Opts) ++
  stack(Stack, Opts) ++
  environment(Env, Es, Opts) ++
  expressions(Es, Opts).


-spec header(pos_integer(), undefined | {atom(), atom(), arity()}) -> string().

header(Pid, MFA) -> "=============== " ++ pid(Pid) ++ ": " ++ function(MFA) ++ " ===============\n".


-spec pid(pos_integer()) -> string().

pid(Pid) -> "Proc. " ++ integer_to_list(Pid).


-spec function(undefined | {atom(), arity()}) -> string().

function(undefined) -> "";
function({M, F, A}) -> io_lib:format("~s:~s/~p", [M, F, A]).


-spec environment(cauder_types:environment(), [cauder_types:abstract_expr()], [{cauder_types:print_option(), boolean()}]) -> string().

environment(Env, Exprs, Opts) ->
  case proplists:get_value(?PRINT_ENV, Opts) of
    false -> "";
    true -> "ENV: " ++ environment_1(Env, Exprs, Opts) ++ "\n"
  end.


-spec environment_1(cauder_types:environment(), [cauder_types:abstract_expr()], [{cauder_types:print_option(), boolean()}]) -> string().

environment_1(Env, Exprs, Opts) ->
  Bindings =
  case proplists:get_value(?PRINT_FULL_ENV, Opts) of
    true -> Env;
    false -> get_relevant_bindings(Env, Exprs)
  end,
  PairsList = lists:map(fun binding/1, Bindings),
  "{" ++ string:join(PairsList, ", ") ++ "}".


-spec binding(cauder_types:binding()) -> string().

binding({Var, Val}) -> io_lib:format("~s -> ~p", [atom_to_list(Var), Val]).


-spec get_relevant_bindings(cauder_types:environment(), [cauder_types:abstract_expr()]) -> cauder_types:environment().

get_relevant_bindings(Bs, Es) ->
  Vs = sets:union(lists:map(fun erl_syntax_lib:variables/1, Es)),
  lists:filter(fun({V, _}) -> sets:is_element(V, Vs) end, Bs).


-spec history(cauder_types:history(), [{cauder_types:print_option(), boolean()}]) -> string().

history(Hist, Opts) ->
  case proplists:get_value(?PRINT_HIST, Opts) of
    false -> "";
    true -> history_1(Hist, Opts) ++ "\n"
  end.


-spec history_1(cauder_types:history(), [{cauder_types:print_option(), boolean()}]) -> string().

history_1(Hist, Opts) ->
  FiltHist =
  case proplists:get_value(?PRINT_FULL, Opts) of
    false -> lists:filter(fun is_conc_item/1, Hist);
    true -> Hist
  end,
  StrItems = lists:map(fun history_entry/1, FiltHist),
  "H  : [" ++ string:join(StrItems, ",") ++ "]".


-spec is_conc_item(cauder_types:history_entry()) -> boolean().

is_conc_item({spawn, _, _, _, _})   -> true;
is_conc_item({send, _, _, _, _, _}) -> true;
is_conc_item({rec, _, _, _, _, _})  -> true;
is_conc_item(_)                     -> false.


-spec history_entry(cauder_types:history_entry()) -> string().

history_entry({tau, _, _, _})                    -> "seq";
history_entry({self, _, _, _})                   -> "self";
history_entry({spawn, _, _, _, Pid})             -> "spawn(" ++ [{?CAUDER_GREEN, to_string(Pid)}] ++ ")";
history_entry({send, _, _, _, _, {Value, Time}}) -> "send(" ++ to_string(Value) ++ "," ++ [{?wxRED, integer_to_list(Time)}] ++ ")";
history_entry({rec, _, _, _, {Value, Time}, _})  -> "rec(" ++ to_string(Value) ++ "," ++ [{?wxBLUE, integer_to_list(Time)}] ++ ")".


-spec process_messages([cauder_types:process_message()], [{cauder_types:print_option(), boolean()}]) -> string().

process_messages(Mail, Opts) ->
  case proplists:get_value(?PRINT_MAIL, Opts) of
    false -> "";
    true -> "LM : " ++ process_messages_1(Mail) ++ "\n"
  end.


-spec process_messages_1([cauder_types:process_message()]) -> string().

process_messages_1([]) -> "[]";
process_messages_1(Mail) ->
  MailList = lists:map(fun process_message/1, Mail),
  "[" ++ string:join(MailList, ",") ++ "]".


-spec process_message(cauder_types:process_message()) -> string().

process_message({Val, Time}) -> "{" ++ io_lib:format("~p", [Val]) ++ "," ++ [{?CAUDER_GREEN, integer_to_list(Time)}] ++ "}".


-spec expressions([erl_parse:abstract_expr()], [{cauder_types:print_option(), boolean()}]) -> string().

expressions(Exprs, Opts) ->
  case proplists:get_value(?PRINT_EXP, Opts) of
    false -> "";
    true -> "EXP:\n" ++ lists:join(",\n", lists:map(fun expression/1, Exprs)) ++ "\n"
  end.


-spec expression(erl_parse:abstract_expr()) -> string().

expression(Expr) -> lists:flatten(erl_prettypr:format(Expr)).


-spec stack(cauder_types:stack(), [{cauder_types:print_option(), boolean()}]) -> string().

stack([], _Opts) -> "STK: []\n";
stack(Stack, _Opts) ->
  EntryToString = fun
                    ({{M, F, A}, _, _, _}) -> io_lib:format("~s:~s/~p", [M, F, A]);
                    ({Type, _, _}) -> atom_to_list(Type)
                  end,
  "STK: [" ++ lists:join(", ", lists:map(EntryToString, Stack)) ++ "]\n".


%% =====================================================================
%% @doc Pretty-prints a given system trace

-spec system_trace(cauder_types:system()) -> string().

system_trace(#sys{trace = Trace}) ->
  % Trace is built as a stack (newest item is first)
  % and we must reverse it to print it
  RevTrace = lists:reverse(Trace),
  TraceStr = [trace(Item) || Item <- RevTrace],
  string:join(TraceStr, "\n").


-spec trace(cauder_types:trace()) -> [string()].

trace(#trace{type = ?RULE_SEND, from = From, to = To, val = Val, time = Time}) ->
  [pid(From), " sends ", to_string(Val), " to ", pid(To), " (", integer_to_list(Time), ")"];
trace(#trace{type = ?RULE_SPAWN, from = From, to = To, val = _, time = _}) ->
  [pid(From), " spawns ", pid(To)];
trace(#trace{type = ?RULE_RECEIVE, from = From, to = _, val = Val, time = Time}) ->
  [pid(From), " receives ", to_string(Val), " (", integer_to_list(Time), ")"].


%% =====================================================================
%% @doc Prints a given system roll log

-spec roll_log(cauder_types:system()) -> string().

roll_log(#sys{roll = RollLog}) -> string:join(RollLog, "\n").


-spec to_string(term()) -> string().

to_string(Term) -> io_lib:format("~p", [Term]).