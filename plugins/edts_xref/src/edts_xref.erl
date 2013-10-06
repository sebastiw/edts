%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% @end
%%% @author Thomas Järvstrand <tjarvstrand@gmail.com>
%%% @copyright
%%% Copyright 2012 Thomas Järvstrand <tjarvstrand@gmail.com>
%%%
%%% This file is part of EDTS.
%%%
%%% EDTS is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% EDTS is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with EDTS. If not, see <http://www.gnu.org/licenses/>.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration =======================================================
-module(edts_xref).

-behaviour(edts_plugins).

%%%_* Exports ==================================================================

%% EDTS plugin API
-export([edts_server_services/0,
         event_formatters/0,
         project_node_modules/0,
         project_node_services/0,
         spec/2]).

-export([analyze/2,
         start/0,
         who_calls/3]).

%%%_* Includes =================================================================
-include_lib("eunit/include/eunit.hrl").

%%%_* Defines ==================================================================

%%%_* Types ====================================================================

%%%_* API ======================================================================

%% EDTS Plugin API
edts_server_services() -> [].
event_formatters()     -> [].
project_node_modules() -> [?MODULE, edts_xref_server].
%% Just the empty list here. We want to start the server asynchronously from
%% the node-init-hook to avoid a long blocking node initialization.
project_node_services() -> [].

spec(analyze,   2) -> [{modules,     [atom]},
                       {xref_checks, [atom]}];
spec(start,     0) -> [];
spec(who_calls, 3) -> [{module,   atom},
                       {function, atom},
                       {arity,    integer}].

analyze(Modules, Checks) ->
  Res = edts_xref_server:check_modules(Modules, Checks),
  [[{type, Type},
    {file, list_to_binary(File)},
    {line, Line},
    {description, list_to_binary(Desc)}] ||
    {Type, File, Line, Desc} <- Res].

start() -> edts_xref_server:start().

who_calls(Module, Function, Arity) ->
  Res = edts_xref_server:who_calls(Module, Function, Arity),
  [[{module,   M},
    {function, F},
    {arity,    A},
    {lines, Lines}] ||
    {{M, F, A}, Lines} <- Res].

%%%_* INTERNAL functions =======================================================

%%%_* Unit tests ===============================================================

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

