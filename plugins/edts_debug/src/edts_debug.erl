%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Erlang interpreter interface through a gen_server for communication
%%% with external processes
%%% @end
%%% @author João Neves <sevenjp@gmail.com>
%%% @copyright
%%% Copyright 2012 João Neves <sevenjp@gmail.com>
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


-export([break/4,
         breakpoints/1,
         breakpoints/2,
         ensure_started/1,
         interpret_module/3,
         interpreted_modules/1,
         module_interpretable_p/2,
         module_interpreted_p/2]).

%%%_* Includes =================================================================

%%%_* Defines ==================================================================

%%%_* Types ====================================================================

%%%_* API ======================================================================

%% Behaviour callbacks
edts_server_services()  -> [].
project_node_modules()  -> [edts_debug_server].
project_node_services() -> [].

break(Node, Module, Line, Break) ->
  ensure_started(Node),
  edts:call(Node, edts_debug_server, break, [Module, Line, Break]).

breakpoints(Node) ->
  ensure_started(Node),
  edts:call(Node, edts_debug_server, breakpoints, []).

breakpoints(Node, Module) ->
  ensure_started(Node),
  edts:call(Node, edts_debug_server, breakpoints, [Module]).

module_interpreted_p(Node, Module) ->
  ensure_started(Node),
  edts:call(Node, edts_debug_server, module_interpreted_p, [Module]).


module_interpretable_p(Node, Module) ->
  ensure_started(Node),
  edts:call(Node, edts_debug_server, module_interpretable_p, [Module]).

ensure_started(Node) ->
  edts:call(Node, edts_debug_server, ensure_started, []).

interpret_module(Node, Module, Interpret) ->
  ensure_started(Node),
  edts:call(Node,
            edts_debug_server,
            interpret_module,
            [Module, Interpret]).

interpreted_modules(Node) ->
  ensure_started(Node),
  edts:call(Node, edts_debug_server, interpreted_modules).


%%%_* Internal functions =======================================================

%%%_* Unit tests ===============================================================

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
