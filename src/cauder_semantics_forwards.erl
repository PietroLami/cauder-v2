%%%-----------------------------------------------------------------------------
%%% @doc Forwards (reversible) semantics for Erlang.
%%% This module includes two functions, one to get the evaluation options for a
%%% given system and one to perform an evaluation step in a process of a given
%%% system.
%%% @end
%%%-----------------------------------------------------------------------------

-module(cauder_semantics_forwards).

%% API
-export([step/2, options/1]).

-import(cauder_eval, [is_reducible/2]).
-import(cauder_utils, [matchMap/2, getPid/2]).

-include("cauder.hrl").


%%%=============================================================================
%%% API
%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Performs a single forwards step in the process with the given Pid in the
%% given System.

-spec step(System, Pid) -> NewSystem when
  System :: cauder_types:system(),
  Pid :: cauder_types:proc_id(),
  NewSystem :: cauder_types:system().

step(#sys{mail = Ms, logs = LMap, trace = Trace, map = Map, hmap = Hmap} = Sys, Pid) ->
  {#proc{pid = Pid, hist = Hist, stack = Stk0, env = Bs0, exprs = Es0} = P0, PMap} = maps:take(Pid, Sys#sys.procs),

  #result{env = Bs, exprs = Es, stack = Stk, label = Label} = cauder_eval:seq(Bs0, Es0, Stk0,Sys),

  case Label of
    tau ->
      case Stk of
        [] ->
          case Es of
            [{value, _, _}] ->
              Atom = cauder_utils:matchMap(Pid,Map),
              NewMap = lists:delete({Atom,Pid}, Map),
              % case per atomo se presente. (se non è presente undefined (ricorda non è possibile registrare Pid morto))
              % nessun problema viene per backward si mette sempre nella storia della mappa end (e quindi il pid è presente)
              % TODO controllo su processo morto non può essere registrato
              Newh = [{'end', [{Atom,Pid}], Pid, []} | Hmap],
              P = P0#proc{
                hist  = [{'end', Bs0, Es0, Stk0, {Atom, Pid}} | Hist],
                stack = Stk,
                env   = Bs,
                exprs = Es
              },
              Sys#sys{
                procs = PMap#{Pid => P},
                hmap =  Newh,
                map = NewMap
              };
            _ ->
              P = P0#proc{
                hist  = [{tau, Bs0, Es0, Stk0} | Hist],
                stack = Stk,
                env   = Bs,
                exprs = Es
              },
              Sys#sys{
                procs = PMap#{Pid => P}
              }
          end;
        _ ->
          P = P0#proc{
            hist  = [{tau, Bs0, Es0, Stk0} | Hist],
            stack = Stk,
            env   = Bs,
            exprs = Es
          },
          Sys#sys{
            procs = PMap#{Pid => P}
          }
      end;
    {self, VarPid} ->
      P = P0#proc{
        hist  = [{self, Bs0, Es0, Stk0} | Hist],
        stack = Stk,
        env   = Bs,
        exprs = cauder_syntax:replace_variable(Es, VarPid, Pid)
      },
      Sys#sys{
        procs = PMap#{Pid => P}
      };
    {spawn, VarPid, M, F, As} ->
      {SpawnPid, NewLog} =
        case LMap of
          #{Pid := [{spawn, LogPid} | RestLog]} ->
            {LogPid, RestLog};
          _ ->
            {cauder_utils:fresh_pid(), []}
        end,

      P1 = P0#proc{
        hist  = [{spawn, Bs0, Es0, Stk0, SpawnPid} | Hist],
        stack = Stk,
        env   = Bs,
        exprs = cauder_syntax:replace_variable(Es, VarPid, SpawnPid)
      },
      P2 = #proc{
        pid   = SpawnPid,
        exprs = [cauder_syntax:remote_call(M, F, lists:map(fun cauder_eval:abstract/1, As))],
        spf   = {M, F, length(As)}
      },
      T = #trace{
        type = ?RULE_SPAWN,
        from = Pid,
        to   = SpawnPid
      },
      Sys#sys{
        procs = PMap#{Pid => P1, SpawnPid => P2},
        logs  = LMap#{Pid => NewLog},
        trace = [T | Trace]
      };
    {send, Dest, Val, Line} ->
      case is_atom(Dest) of
        true  ->
          case cauder_utils:getPid(Dest,Map) of
            undefined  -> %SendF
              P = P0#proc{
                hist  = [{sendF, Bs0, Es0, Stk0, Dest} | Hist],
                stack = Stk,
                env   = Bs,
                exprs = [{value, Line, sendFail}]
              },
              NewHMap = [{sendF, [Dest], Pid, []}| Hmap],
              Sys#sys{
                procs = PMap#{Pid => P},
                hmap = NewHMap
              };
            Pi -> %SendA
              {Uid, NewLog} =
                case LMap of
                  #{Pid := [{send, LogUid} | RestLog]} ->
                    {LogUid, RestLog};
                  _ ->
                    {cauder_utils:fresh_uid(), []}
                end,

              M = #msg{
                dest = Pi,
                val  = Val,
                uid  = Uid
              },
              P = P0#proc{
                hist  = [{sendA, Bs0, Es0, Stk0, M, {Dest, Pi}} | Hist],
                stack = Stk,
                env   = Bs,
                exprs = Es
              },
              T = #trace{
                type = ?RULE_SEND,
                from = Pid,
                to   = Pi,
                val  = Val,
                time = Uid
              },
              NewHMap = [{sendA, [{Dest,Pi}], Pid, M} | Hmap],
              Sys#sys{
                mail  = [M | Ms],
                procs = PMap#{Pid => P},
                logs  = LMap#{Pid => NewLog},
                hmap = NewHMap,
                trace = [T | Trace]
              }
          end;
        false -> %send
          {Uid, NewLog} =
            case LMap of
              #{Pid := [{send, LogUid} | RestLog]} ->
                {LogUid, RestLog};
              _ ->
                {cauder_utils:fresh_uid(), []}
            end,

          M = #msg{
            dest = Dest,
            val  = Val,
            uid  = Uid
          },
          P = P0#proc{
            hist  = [{send, Bs0, Es0, Stk0, M} | Hist],
            stack = Stk,
            env   = Bs,
            exprs = Es
          },
          T = #trace{
            type = ?RULE_SEND,
            from = Pid,
            to   = Dest,
            val  = Val,
            time = Uid
          },
          Sys#sys{
            mail  = [M | Ms],
            procs = PMap#{Pid => P},
            logs  = LMap#{Pid => NewLog},
            trace = [T | Trace]
          }
      end;
    {rec, VarBody, Cs} when Es == [VarBody] ->
      {{Bs1, Es1, M = #msg{dest = Pid, val = Val, uid = Uid}, Ms1}, NewLog} =
        case LMap of
          #{Pid := [{'receive', LogUid} | RestLog]} ->
            {cauder_eval:match_rec_uid(Cs, Bs, LogUid, Ms), RestLog};
          _ ->
            {cauder_eval:match_rec_pid(Cs, Bs, Pid, Ms), []}
        end,

      P = P0#proc{
        hist  = [{rec, Bs0, Es0, Stk0, M} | Hist],
        stack = Stk,
        env   = cauder_utils:merge_bindings(Bs, Bs1),
        exprs = Es1
      },
      T = #trace{
        type = ?RULE_RECEIVE,
        from = Pid,
        val  = Val,
        time = Uid
      },
      Sys#sys{
        mail  = Ms1,
        procs = PMap#{Pid => P},
        logs  = LMap#{Pid => NewLog},
        trace = [T | Trace]
      };
    {bottom, Line, Fail} ->
      P = P0#proc{
        hist  = [{fail, Bs0, Es0, Stk0} | Hist],
        stack = Stk,
        env   = Bs,
        exprs = [{value,Line, Fail}]
      },
      Sys#sys{
        procs = PMap#{Pid => P}
      };
    {register, IsFail, Line, A, Pi} ->
      case IsFail of
        true ->
          P = P0#proc{
            hist  = [{registerF, Bs0, Es0, Stk0, {A,Pi}} | Hist],
            stack = Stk,
            env   = Bs,
            exprs = [{value,Line,registerFail}]
          },
          Newh = [{registerF, [{A,Pi}], Pid, []} | Hmap],
          Sys#sys{
            procs = PMap#{Pid => P},
            hmap = Newh
          };
        false ->
          P = P0#proc{
            hist  = [{registerT, Bs0, Es0, Stk0, {A,Pi}} | Hist],
            stack = Stk,
            env   = Bs,
            exprs = Es
          },
          Newh = [{registerT, [{A,Pi}], Pid, []} | Hmap],
          NewMap = [ {A,Pi} | Map],
          Sys#sys{
            procs = PMap#{Pid => P},
            hmap = Newh,
            map = NewMap
          }
      end;
    {unregister, IsFail, Line, A} ->
      case IsFail of
        true ->
          P = P0#proc{
            hist  = [{unregisterF, Bs0, Es0, Stk0, A} | Hist],
            stack = Stk,
            env   = Bs,
            exprs = [{value,Line,unregisterFail}]
          },
          Newh = [{unregisterF, [A], Pid, []} | Hmap],
          Sys#sys{
            procs = PMap#{Pid => P},
            hmap = Newh
          };
        false ->
          Pi = getPid(A,Map),
          P = P0#proc{
            hist  = [{unregisterT, Bs0, Es0, Stk0, {A,Pi}} | Hist],
            stack = Stk,
            env   = Bs,
            exprs = Es
          },
          Newh = [{unregisterT, [{A,Pi}], Pid, []} | Hmap],
          NewMap = lists:delete({A,Pi}, Map),
          Sys#sys{
            procs = PMap#{Pid => P},
            hmap = Newh,
            map = NewMap
          }
      end;
    {tauM, []}  ->
      P = P0#proc{
        hist  = [{tauM, Bs0, Es0, Stk0, Map} | Hist],
        stack = Stk,
        env   = Bs,
        exprs = Es
      },
      Newh = [{tauM, Map, Pid, []} | Hmap],
      Sys#sys{
        procs = PMap#{Pid => P},
        hmap = Newh
      };
    {tauM, AP}  ->
      P = P0#proc{
        hist  = [{tauM, Bs0, Es0, Stk0, [AP]} | Hist],
        stack = Stk,
        env   = Bs,
        exprs = Es
      },
      Newh = [{tauM, [AP], Pid, []} | Hmap],
      Sys#sys{
        procs = PMap#{Pid => P},
        hmap = Newh
      }
  end.


%%------------------------------------------------------------------------------
%% @doc Returns the forwards evaluation options for the given System.

-spec options(System) -> Options when
  System :: cauder_types:system(),
  Options :: [cauder_types:option()].

options(#sys{mail = Mail, procs = PMap, logs = LMap}) ->
  lists:foldl(
    fun
      (#proc{pid = Pid, stack = Stk, env = Bs, exprs = Es}, Opts) ->
        Log = maps:get(Pid, LMap, []),
        case expression_option(Pid, Es, Bs, Stk, Log, Mail) of
          ?NOT_EXP -> Opts;
          Rule ->
            Opt = #opt{
              sem  = ?MODULE,
              pid  = Pid,
              rule = Rule
            },
            [Opt | Opts]
        end
    end,
    [],
    maps:values(PMap)
  ).


%%%=============================================================================
%%% Internal functions
%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Returns the evaluation option for the given Expression, in the given
%% System.
%%
%% @todo Refactor arguments and return value

-spec expression_option(Pid, Expressions, Environment, Stack, Log, Mail) -> Rule when
  Pid :: cauder_types:proc_id(),
  Expressions :: cauder_types:abstract_expr() | [cauder_types:abstract_expr()],
  Environment :: cauder_types:environment(),
  Stack :: cauder_types:stack(),
  Log :: cauder_types:log(),
  Mail :: [cauder_types:message()],
  Rule :: cauder_types:rule() | ?NOT_EXP.

expression_option(Pid, E, Bs, Stk, Log, Mail) when not is_list(E) ->
  expression_option(Pid, [E], Bs, Stk, Log, Mail);
expression_option(Pid, [E0 | Es0], Bs, Stk, Log, Mail) ->
  case is_reducible(E0, Bs) of
    false ->
      case {Es0, Stk} of
        {[], []} -> ?NOT_EXP;
        _ -> ?RULE_SEQ
      end;
    true ->
      case E0 of
        {var, _, _} -> ?RULE_SEQ;
        {fail, _, _} -> ?RULE_SEQ;
        {cons, _, H, T} ->
          case is_reducible(H, Bs) of
            true -> expression_option(Pid, H, Bs, Stk, Log, Mail);
            false -> expression_option(Pid, T, Bs, Stk, Log, Mail)
          end;
        {tuple, _, Es} -> expression_option(Pid, Es, Bs, Stk, Log, Mail);
        {'if', _, _} -> ?RULE_SEQ;
        {'case', _, E, _} ->
          case is_reducible(E, Bs) of
            true -> expression_option(Pid, E, Bs, Stk, Log, Mail);
            false -> ?RULE_SEQ
          end;
        {'receive', _, Cs} ->
          IsMatch =
            case Log of
              [] -> cauder_eval:match_rec_pid(Cs, Bs, Pid, Mail) =/= nomatch;
              [{'receive', Uid} | _] -> cauder_eval:match_rec_uid(Cs, Bs, Uid, Mail) =/= nomatch
            end,
          case IsMatch of
            true -> ?RULE_RECEIVE;
            false -> ?NOT_EXP
          end;
        {'make_fun', _, _, _} -> ?RULE_SEQ;
        {bif, _, _, _, As} ->
          case is_reducible(As, Bs) of
            true -> expression_option(Pid, As, Bs, Stk, Log, Mail);
            false -> ?RULE_SEQ
          end;
        {self, _} -> ?RULE_SELF;
        {spawn, _, F} ->
          case is_reducible(F, Bs) of
            true -> expression_option(Pid, F, Bs, Stk, Log, Mail);
            false -> ?RULE_SPAWN
          end;
        {spawn, _, M, F, As} ->
          case is_reducible(M, Bs) of
            true -> expression_option(Pid, M, Bs, Stk, Log, Mail);
            false ->
              case is_reducible(F, Bs) of
                true -> expression_option(Pid, F, Bs, Stk, Log, Mail);
                false ->
                  case is_reducible(As, Bs) of
                    true -> expression_option(Pid, As, Bs, Stk, Log, Mail);
                    false -> ?RULE_SPAWN
                  end
              end
          end;
        {Send, _, L, R} when Send =:= 'send' orelse Send =:= 'send_op' ->
          case is_reducible(L, Bs) of
            true -> expression_option(Pid, L, Bs, Stk, Log, Mail);
            false ->
              case is_reducible(R, Bs) of
                true -> expression_option(Pid, R, Bs, Stk, Log, Mail);
                false -> ?RULE_SEND
              end
          end;
        {local_call, _, _, As} ->
          case is_reducible(As, Bs) of
            true -> expression_option(Pid, As, Bs, Stk, Log, Mail);
            false -> ?RULE_SEQ
          end;
        {remote_call, _, _, _, As} ->
          case is_reducible(As, Bs) of
            true -> expression_option(Pid, As, Bs, Stk, Log, Mail);
            false -> ?RULE_SEQ
          end;
        {apply, _, M, F, As} ->
          case is_reducible(M, Bs) of
            true -> expression_option(Pid, M, Bs, Stk, Log, Mail);
            false ->
              case is_reducible(F, Bs) of
                true -> expression_option(Pid, F, Bs, Stk, Log, Mail);
                false ->
                  case is_reducible(As, Bs) of
                    true -> expression_option(Pid, As, Bs, Stk, Log, Mail);
                    false -> ?RULE_SEQ
                  end
              end
          end;
        {apply_fun, _, Fun, As} ->
          case is_reducible(Fun, Bs) of
            true -> expression_option(Pid, Fun, Bs, Stk, Log, Mail);
            false ->
              case is_reducible(As, Bs) of
                true -> expression_option(Pid, As, Bs, Stk, Log, Mail);
                false -> ?RULE_SEQ
              end
          end;
        {match, _, P, E} ->
          case is_reducible(E, Bs) of
            true -> expression_option(Pid, E, Bs, Stk, Log, Mail);
            false ->
              case is_reducible(P, Bs) of
                true -> expression_option(Pid, P, Bs, Stk, Log, Mail);
                false -> ?RULE_SEQ
              end
          end;
        {op, _, _, Es} ->
          case is_reducible(Es, Bs) of
            true -> expression_option(Pid, Es, Bs, Stk, Log, Mail);
            false -> ?RULE_SEQ
          end;
        {Op, _, L, R} when Op =:= 'andalso'; Op =:= 'orelse' ->
          case is_reducible(L, Bs) of
            true -> expression_option(Pid, L, Bs, Stk, Log, Mail);
            false ->
              case is_reducible(R, Bs) of
                true -> expression_option(Pid, R, Bs, Stk, Log, Mail);
                false -> ?RULE_SEQ
              end
          end;
        {register, _, L, R} ->
          case is_reducible(L, Bs) of
            true -> expression_option(Pid, L, Bs, Stk, Log, Mail);
            false ->
              case is_reducible(R, Bs) of
                true -> expression_option(Pid, R, Bs, Stk, Log, Mail);
                false -> ?RULE_REGISTER
              end
          end;
        {unregister, _, L} ->
          case is_reducible(L, Bs) of
            true -> expression_option(Pid, L, Bs, Stk, Log, Mail);
            false -> ?RULE_UNREGISTER
          end;
        {tauM, _, _} -> ?RULE_SEQ
      end
  end.
