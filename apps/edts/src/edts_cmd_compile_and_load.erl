%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc compile_and_load_command command
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
-module(edts_cmd_compile_and_load).

%%%_* Exports ==================================================================

%% API
-export([spec/0,
         execute/1]).

%%%_* Includes =================================================================
%%%_* Defines ==================================================================
%%%_* Types ====================================================================
%%%_* API ======================================================================

spec() ->
  [nodename, file].

execute(Ctx) ->
    Node     = orddict:fetch(nodename, Ctx),
    Filename = orddict:fetch(file, Ctx),
    {ok, {Result, {Errors0, Warnings0}}} =
        edts:call(Node, edts_code, compile_and_load, [Filename]),
    Errors   = {array, [format_error(Error) || Error <- Errors0]},
    Warnings = {array, [format_error(Warning) || Warning <- Warnings0]},
    {ok, {struct, [{result, Result}, {warnings, Warnings}, {errors, Errors}]}}.

%%%_* Internal functions =======================================================

format_error({Type, File, Line, Desc}) ->
    [ {type, Type}
    , {file, list_to_binary(File)}
    , {line, Line}
    , {description, list_to_binary(Desc)}].


%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
