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

-behaviour(edts_plugin).

%%%_* Exports =================================================================

%% Behaviour exports
-export([edts_server_services/0,
         project_node_modules/0,
         project_node_services/0]).


-export([break/3,
         breakpoint_exists_p/2,
         breakpoints/0,
         breakpoints/1,
         continue/1,
         ensure_started/0,
         interpret_module/2,
         interpreted_modules/0,
         module_interpretable_p/1,
         module_interpreted_p/1,
         processes/0]).


%%%_* Includes =================================================================

%%%_* Defines ==================================================================

%%%_* Types ====================================================================

%%%_* API ======================================================================

%% Behaviour callbacks
edts_server_services()  -> [].
project_node_modules()  -> [?MODULE, edts_debug_server].
project_node_services() -> [].


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
  case interpret_module(Module, true) of
    {error, _} = E -> E;
    true           ->
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
  int:all_breaks().


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
  [Break || Break = {{M, _},_} <- int:all_breaks(), M =:= Module].


%%------------------------------------------------------------------------------
%% @doc
%% Orders the debugger to continue execution until it reaches another
%% breakpoint or execution terminates.
%% @end
-spec continue(pid()) -> ok.
%%------------------------------------------------------------------------------
continue(Pid) ->
  ensure_started(),
  int:continue(Pid).


%%------------------------------------------------------------------------------
%% @doc
%% Ensure that the debug-server is running
%% @end
-spec ensure_started() -> ok.
%%------------------------------------------------------------------------------
ensure_started() ->
  case dbg_iserver:find() of
    undefined -> dbg_iserver:start();
    _         -> ok
  end,
  edts_debug_server:ensure_started().


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
%% Return a list of all non terminated processes known to the debug server.
%% @end
%% @see int:snapshot/0
-spec processes() -> [{ { Module :: module()
                              , Line   :: non_neg_integer()
                              }
                            , Options  :: [term()]
                            }].
%%------------------------------------------------------------------------------
processes() ->
  ensure_started(),
  [Proc || {_, _, Status, _}  = Proc <- int:snapshot(), Status =/= exit].


%%%_* Internal functions =======================================================

%%%_* Unit tests ===============================================================

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
