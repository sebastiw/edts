%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc run_module_eunit command
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
-module(edts_cmd_run_eunit).

%%%_* Exports ==================================================================

%% API
-export([spec/0,
         execute/1]).

%%%_* Includes =================================================================
%%%_* Defines ==================================================================
%%%_* Types ====================================================================
%%%_* API ======================================================================

spec() ->
  [nodename, module].

execute(Ctx) ->
    Node   = orddict:fetch(nodename, Ctx),
    Module = orddict:fetch(module, Ctx),
    {ok, {ok, Result}} = edts:call(Node, edts_eunit, run_tests, [Module]),
    {Passed, Failed} = lists:partition(fun passed_test_p/1, Result),
    {ok, [{passed, {array, [format_test(Test) || Test <- Passed]}},
          {failed, {array, [format_test(Test) || Test <- Failed]}}]}.

%%%_* Internal functions =======================================================

passed_test_p({Type, _, _, _}) ->
    Type =:= 'passed-test'.

format_test({Type, File, Line, Desc}) ->
  {struct, [ {type, Type}
           , {file, list_to_binary(File)}
           , {line, Line}
           , {description, unicode:characters_to_binary(Desc)}]}.


%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
