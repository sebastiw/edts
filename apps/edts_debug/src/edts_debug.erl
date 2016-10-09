%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Erlang interpreter interface through a gen_server for communication
%%% with external processes
%%% @end
%%% @author Thomas Järvstrand <tjarvstrand@gmail.com>
%%% @copyright
%%% Copyright 2012 Thomas Järvstrand <tjarvstrand@gmail.com>
%%%
%%% This file is part of EDTS.
%%%
%%% EDTS is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU Lesser General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% EDTS is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Lesser General Public License for more details.
%%%
%%% You should have received a copy of the GNU Lesser General Public License
%%% along with EDTS. If not, see <http://www.gnu.org/licenses/>.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration =======================================================
-module(edts_debug).

-behaviour(edts_plugins).

%%%_* Exports =================================================================

%% Behaviour exports
-export([edts_server_services/0,
         event_formatters/0,
         project_node_modules/0,
         project_node_services/0,
         spec/2]).


-export([bound_variables/1,
         break/3,
         breakpoint_exists_p/2,
         breakpoints/0,
         breakpoints/1,
         ensure_started/0,
         get_bindings_pretty/3,
         interpret_module/2,
         interpreted_modules/0,
         module_interpretable_p/1,
         module_interpreted_p/1,
         process_state/1,
         processes/0,

         %% interpreter process commands
         continue/1,
         finish/1,
         step_into/1,
         step_over/1]).


%%%_* Includes =================================================================

%%%_* Defines ==================================================================

%%%_* Types ====================================================================

%%%_* API ======================================================================

%% Behaviour callbacks
edts_server_services()  -> [].
event_formatters()      -> [{edts_debug, edts_events_debug}].
project_node_modules()  -> [?MODULE, edts_debug_server].
project_node_services() -> [].

spec(bound_variables,      1) -> [{pid, pid}];
spec(break,                3) -> [{module,   atom},
                                  {line,     integer},
                                  {break,    atom}];
spec(breakpoints,          0) -> [];
spec(breakpoints,          1) -> [{module,   atom}];
spec(continue,             1) -> [{pid, pid}];
spec(finish,               1) -> [{pid, pid}];
spec(get_bindings_pretty,  3) -> [{pid, pid},
                                  {indent, integer},
                                  {max_column, integer}];
spec(interpreted_modules,  0) -> [];
spec(interpret_module,     2) -> [{module,   atom},
                                 {interpret, atom}];
spec(module_interpreted_p, 1) -> [{module,   atom}];
spec(processes,            0) -> [];
spec(step_into,            1) -> [{pid, pid}];
spec(step_over,            1) -> [{pid, pid}].


%%------------------------------------------------------------------------------
%% @doc
%% Create or delete a breakpoint at Line in Module. Returns true if a break
%% point was created or already existed, false otherwise.
%% @end
-spec break(Module :: module(),
            Line   :: non_neg_integer(),
            Break :: true | false | toggle) -> boolean().
%%------------------------------------------------------------------------------
break(Module, Line, Break) ->
  ensure_started(),
  do_break(Module, Line, Break).

do_break(Module, Line, toggle) ->
  do_break(Module, Line, not breakpoint_exists_p(Module, Line));
do_break(Module, Line, true) ->
  case module_interpreted_p(Module) of
    false ->
      case interpret_module(Module, true) of
        {error, _} = E -> E;
        true           ->
          int:break(Module, Line),
          true
      end;
    true ->
      int:break(Module, Line),
      true
  end;
do_break(Module, Line, false) ->
  int:delete_break(Module, Line),
  false.

%%------------------------------------------------------------------------------
%% @doc
%% Returns true if there exists a breakpoint at Line in Module
%% @end
-spec breakpoint_exists_p(Module :: module(),
                          Line   :: non_neg_integer()) -> boolean().
%%------------------------------------------------------------------------------
breakpoint_exists_p(Module, Line) ->
  lists:keymember({Module, Line}, 1, int:all_breaks()).





%%------------------------------------------------------------------------------
%% @doc
%% Get all breakpoints and their status in the current interpreter
%% @end
-spec breakpoints() -> [{ { Module :: module()
                              , Line   :: non_neg_integer()
                              }
                            , Options  :: [term()]
                            }].
%%------------------------------------------------------------------------------
breakpoints() ->
  ensure_started(),
  lists:map(fun({{Module, Line}, [Status, Trigger, null, Condition]}) ->
                [{module,     Module},
                 {line,      Line},
                 {status,    Status},
                 {trigger,   Trigger},
                 {condition, edts_plugins:to_ret_str(Condition)}]
            end,
            int:all_breaks()).

%%------------------------------------------------------------------------------
%% @doc
%% Get all breakpoints and their status in the current interpreter
%% @end
-spec breakpoints(Module :: module()) ->
                     [{{Module :: module(), Line   :: non_neg_integer()},
                       Options  :: [term()]}].
%%------------------------------------------------------------------------------
breakpoints(Module) ->
  ensure_started(),
  %% int:all_breaks/1 is broken in OTP < R15.
  lists:filter(fun(B) -> {module, Module} =:= lists:keyfind(module, 1, B) end,
               breakpoints()).


%%------------------------------------------------------------------------------
%% @doc
%% Orders the debugger to continue execution until it reaches another
%% breakpoint or execution terminates.
%% @end
-spec continue(pid()) -> ok.
%%------------------------------------------------------------------------------
continue(Pid) ->
  ensure_started(),
  int:continue(Pid),
  ok.


%%------------------------------------------------------------------------------
%% @doc
%% Ensure that the debug-server is running
%% @end
-spec ensure_started() -> ok.
%%------------------------------------------------------------------------------
ensure_started() ->
  case whereis(dbg_iserver) of
    undefined -> dbg_iserver:start();
    _         -> ok
  end,
  edts_debug_server:ensure_started().

%%------------------------------------------------------------------------------
%% @doc
%% Orders the debugger to continue execution until termination, without
%% considering any further breakpoints.
%% @end
-spec finish(pid()) -> ok.
%%------------------------------------------------------------------------------
finish(Pid) ->
  ensure_started(),
  int:finish(Pid),
  ok.

%%------------------------------------------------------------------------------
%% @doc
%% Return a list of all of Pid's currently bound variables.
%% @end
-spec bound_variables(pid()) -> [atom()].
%%------------------------------------------------------------------------------
bound_variables(Pid) ->
  [Var || {Var, _Binding} <- get_bindings(Pid)].


%%------------------------------------------------------------------------------
%% @doc
%% Return a list of all of Pid's current variable bindings pretty printed as
%% strings with Indent spaces of indentantion and line breaks at MaxColumn
%% @end
-spec get_bindings_pretty(pid(), non_neg_integer(), non_neg_integer()) ->
                             [{atom(), binary()}].
%%------------------------------------------------------------------------------
get_bindings_pretty(Pid, Indent, MaxColumn) ->
  Fun = fun({Binding, Value}) ->
            {Binding, edts_plugins:to_ret_str(Value, Indent, MaxColumn)}
        end,
  lists:sort(lists:map(Fun, get_bindings(Pid))).


%%------------------------------------------------------------------------------
%% @doc
%% Return a list of all of Pid's current variable bindings. Unless the process
%% is in a 'break' state, this will be [].
%% @end
-spec get_bindings(pid()) -> [{atom(), binary()}].
%%------------------------------------------------------------------------------
get_bindings(Pid) ->
  ensure_started(),
  case process_state(Pid) of
    {ok, ProcessState} ->
      case process_status(ProcessState) of
        break ->
          {ok, Meta} = dbg_iserver:call({get_meta, Pid}),
          int:meta(Meta, bindings, nostack);
        _ ->
          []
      end;
    {error, _} = Err ->
      Err
  end.


%%------------------------------------------------------------------------------
%% @doc
%% Change the interpretation state of Module depending on Intepret. Returns
%% {ok, Bool} where Bool is true if Module is now interpreted and false
%% otherwise.
%% @end
-spec interpret_module(Modules   :: module(),
                       Interpret :: true | false | toggle) ->
                          {ok, boolean()}.
%%------------------------------------------------------------------------------
interpret_module(Module, Interpret) ->
  ensure_started(),
  do_interpret_module(Module, Interpret).

do_interpret_module(Module, toggle) ->
  do_interpret_module(Module, not module_interpreted_p(Module));
do_interpret_module(Module, true) ->
  case module_interpretable_p(Module) of
    false -> {error, uninterpretable};
    true  ->
      {module, Module} = int:i(Module),
      true
  end;
do_interpret_module(Module, false) ->
  ok = int:n(Module),
  false.


%%------------------------------------------------------------------------------
%% @doc
%% Return a list of all interpreted modules.
%% @end
-spec interpreted_modules() -> [module()].
%%------------------------------------------------------------------------------
interpreted_modules() ->
  ensure_started(),
  int:interpreted().


%%------------------------------------------------------------------------------
%% @doc
%% Reports if Module is interpreted.
%% @end
-spec module_interpreted_p(Module :: module()) -> boolean().
%%------------------------------------------------------------------------------
module_interpreted_p(Module) ->
  ensure_started(),
  lists:member(Module, interpreted_modules()).

%%------------------------------------------------------------------------------
%% @doc
%% Return true if Module is interpretable, false otherwise
%% @end
-spec module_interpretable_p(module()) -> boolean().
%%------------------------------------------------------------------------------
module_interpretable_p(Module) ->
  ensure_started(),
  case int:interpretable(Module) of
    true       -> true;
    {error, _} -> false
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Return the status of Pid if it's known to the the debug server.
%% @end
%% @see int:snapshot/0
-spec process_state(Pid :: pid()) ->
                       {{Module :: module(), Line :: non_neg_integer()},
                        Options  :: [term()]}.
%%------------------------------------------------------------------------------
process_state(Pid) ->
  ensure_started(),
  case lists:keyfind(Pid, 1, int:snapshot()) of
    false -> {error, not_found};
    Proc  -> {ok, Proc}
  end.


%%------------------------------------------------------------------------------
%% @doc
%% Return a list of all non terminated processes known to the debug server.
%% @end
%% @see int:snapshot/0
-spec processes() -> [pid()].
%%------------------------------------------------------------------------------
processes() ->
  ensure_started(),
  Procs = [P || {_, _, Status, _}  = P <- int:snapshot(), Status =/= exit],
  [format_process(P) || P <- Procs].

format_process({Pid, Init, Status, Info}) ->
  orddict:from_list([{pid,      Pid},
                     {init,     Init},
                     {status,   Status},
                     {module,   info_to_module(Info)},
                     {line,     info_to_line(Info)},
                     {info,     Info}]).

info_to_module({Mod, _Line}) -> Mod;
info_to_module(_)            -> null.

info_to_line({_Mod, Line}) -> Line;
info_to_line(_)            -> -1.


%%------------------------------------------------------------------------------
%% @doc
%% Orders the debugger to execute the next expression. If the expression is
%% a function call, break on the first line of the function.
%% @end
-spec step_into(pid()) -> ok.
%%------------------------------------------------------------------------------
step_into(Pid) ->
  ensure_started(),
  int:step(Pid),
  ok.

%%------------------------------------------------------------------------------
%% @doc
%% Orders the debugger to execute the next expression. If the expression is
%% a function call, break when returning from that call.
%% @end
-spec step_over(pid()) -> ok.
%%------------------------------------------------------------------------------
step_over(Pid) ->
  ensure_started(),
  int:next(Pid),
  ok.


%%%_* Internal functions =======================================================

process_status({_Pid, _Fun, Status, _Info}) -> Status.

%%%_* Unit tests ===============================================================

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
