%%%-----------------------------------------------------------------------------
%%% @doc Backwards (reversible) semantics for Erlang.
%%% @end
%%%-----------------------------------------------------------------------------

-module(cauder_semantics_backwards).

%% API
-export([step/2, options/1]).
-import(cauder_utils, [rule_mapW/2]).

-include("cauder.hrl").


%%%=============================================================================
%%% API
%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Performs a single backwards step in the process with the given Pid in
%% the given System.

-spec step(System, Pid) -> NewSystem when
  System :: cauder_types:system(),
  Pid :: cauder_types:proc_id(),
  NewSystem :: cauder_types:system().

step(#sys{mail = Ms, logs = LMap, trace = Trace, map = Map, hmap = Hmap} = Sys, Pid) ->
  {#proc{pid = Pid, hist = [Entry | RestHist]} = P0, PMap} = maps:take(Pid, Sys#sys.procs),

  case Entry of
    {Label, Bs, Es, Stk} when Label =:= tau orelse Label =:= self ->
      P = P0#proc{
        hist  = RestHist,
        stack = Stk,
        env   = Bs,
        exprs = Es
      },
      Sys#sys{
        mail  = Ms,
        procs = PMap#{Pid => P}
      };
    {spawn, Bs, Es, Stk, Gid} ->
      P = P0#proc{
        hist  = RestHist,
        stack = Stk,
        env   = Bs,
        exprs = Es
      },
      T = #trace{
        type = ?RULE_SPAWN,
        from = Pid,
        to   = Gid
      },
      Sys#sys{
        mail  = Ms,
        procs = maps:remove(Gid, PMap#{Pid => P}),
        logs  = maps:update_with(Pid, fun(Log) -> [{spawn, Gid} | Log] end, [], LMap),
        trace = lists:delete(T, Trace)
      };
    {send, Bs, Es, Stk, #msg{dest = Dest, val = Val, uid = Uid}} ->
      {_Msg, OldMsgs} = cauder_utils:take_message(Ms, Uid),
      P = P0#proc{
        hist  = RestHist,
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
        mail  = OldMsgs,
        procs = PMap#{Pid => P},
        logs  = maps:update_with(Pid, fun(Log) -> [{send, Uid} | Log] end, [], LMap),
        trace = lists:delete(T, Trace)
      };
    {rec, Bs, Es, Stk, M = #msg{dest = Pid, val = Val, uid = Uid}} ->
      P = P0#proc{
        hist  = RestHist,
        stack = Stk,
        env   = Bs,
        exprs = Es
      },
      T = #trace{
        type = ?RULE_RECEIVE,
        from = Pid,
        val  = Val,
        time = Uid
      },
      Sys#sys{
        mail  = [M | Ms],
        procs = PMap#{Pid => P},
        logs  = maps:update_with(Pid, fun(Log) -> [{'receive', Uid} | Log] end, [], LMap),
        trace = lists:delete(T, Trace)
      };
    {'end', Bs, Es, Stk, {A,Pi}} ->
      P = P0#proc{
        hist  = RestHist,
        stack = Stk,
        env   = Bs,
        exprs = Es
      },
      NewHMap = lists:delete({'end', [{A,Pi}], Pid, []}, Hmap),
      case A of
        undefined ->
          Sys#sys{
            mail  = Ms,
            hmap = NewHMap,
            procs = PMap#{Pid => P}
          };
        _ ->
          NewMap = [ {A,Pi} | Map],
          Sys#sys{
            procs = PMap#{Pid => P},
            hmap = NewHMap,
            map = NewMap
          }
      end
  end.


%%------------------------------------------------------------------------------
%% @doc Returns the backwards evaluation options for the given System.

-spec options(System) -> Options when
  System :: cauder_types:system(),
  Options :: [cauder_types:option()].

options(#sys{procs = PMap} = Sys) ->
  maps:fold(
    fun
      (Pid, Proc, Opts) ->
        case process_option(Sys#sys{procs = maps:without([Pid], PMap)}, Proc) of
          ?NULL_OPT -> Opts;
          Opt -> [Opt | Opts]
        end
    end,
    [],
    PMap
  ).


%%%=============================================================================
%%% Internal functions
%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Returns the evaluation option for the given Process, in the given
%% System.

-spec process_option(System, Process) -> Option when
  System :: cauder_types:system(),
  Process :: cauder_types:process(),
  Option :: cauder_types:option() | ?NULL_OPT.

process_option(_, #proc{hist = []}) ->
  ?NULL_OPT;
process_option(_, #proc{pid = Pid, hist = [{tau, _Bs, _Es, _Stk} | _]}) ->
  #opt{sem = ?MODULE, pid = Pid, rule = ?RULE_SEQ};
process_option(_, #proc{pid = Pid, hist = [{self, _Bs, _Es, _Stk} | _]}) ->
  #opt{sem = ?MODULE, pid = Pid, rule = ?RULE_SELF};
process_option(#sys{procs = PMap}, #proc{pid = Pid, hist = [{spawn, _Bs, _Es, _Stk, SpawnPid} | _]}) ->
  #proc{hist = Hist} = maps:get(SpawnPid, PMap),
  case Hist of
    [] -> #opt{sem = ?MODULE, pid = Pid, rule = ?RULE_SPAWN};
    _ -> ?NULL_OPT
  end;
process_option(#sys{mail = Mail}, #proc{pid = Pid, hist = [{send, _Bs, _Es, _Stk, #msg{uid = Uid}} | _]}) ->
  case cauder_utils:find_message(Mail, Uid) of
    {value, _} -> #opt{sem = ?MODULE, pid = Pid, rule = ?RULE_SEND};
    false -> ?NULL_OPT
  end;
process_option(_, #proc{pid = Pid, hist = [{rec, _Bs, _Es, _Stk, _Msg} | _]}) ->
  #opt{sem = ?MODULE, pid = Pid, rule = ?RULE_RECEIVE};
process_option(#sys{hmap = Hmap}, #proc{pid = Pid, hist = [{'end', _Bs, _Es, _Stk, El}| _]}) ->
  case cauder_utils:rule_mapW({'end', [El], Pid, []},Hmap) of
    true  ->   #opt{sem = ?MODULE, pid = Pid, rule = ?RULE_END};
    false ->   ?NULL_OPT
  end.
