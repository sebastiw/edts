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
-module(edts).

%%%_* Exports ==================================================================

%% API
-export([compile_and_load/2,
         get_dialyzer_result/4,
         get_function_info/4,
         get_module_eunit_result/2,
         get_module_info/3,
         get_module_xref_analysis/3,
         init_node/3,
         is_node/1,
         node_available_p/1,
         modules/1,
         node_reachable/1,
         nodes/0,
         who_calls/4]).

%%%_* Includes =================================================================
-include_lib("eunit/include/eunit.hrl").

%%%_* Defines ==================================================================

%%%_* Types ====================================================================

%%%_* API ======================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Compiles Module on Node and returns a list of any errors and warnings.
%% If there are no errors, the module will be loaded.
%% @end
-spec compile_and_load(Node::node(), Filename::file:filename()) ->
                          [term()] | {error, not_found}.
%%------------------------------------------------------------------------------
compile_and_load(Node, Filename) ->
  edts_log:debug("compile_and_load ~p on ~p", [Filename, Node]),
  edts_server:wait_for_node(Node),
  case edts_dist:call(Node, edts_code, compile_and_load, [Filename]) of
    {badrpc, E} ->
      Fmt = "Error in remote call edts_code:compile_and_load/1 on ~p: ~p",
      edts_log:error(Fmt, [Node, E]),
      {error, not_found};
    Result      -> Result
  end.


%%------------------------------------------------------------------------------
%% @doc
%% Returns the result of dialyzing Files on Node.
%% @end
%%
-spec get_dialyzer_result(Node::node(),
                          OtpPlt::filename:filename(),
                          OutPlt::filename:filename(),
                          Files::[filename:filename()]) ->
                             {ok, edts_code:issue()}.
%%------------------------------------------------------------------------------
get_dialyzer_result(Node, OtpPlt, OutPlt, Files) ->
  edts_log:debug("get_dialyzer_result ~p (~p) -> ~p, ~p",
                 [Files, OtpPlt, OutPlt, Node]),
  case edts_dist:call(Node, edts_dialyzer, run, [OtpPlt, OutPlt, Files]) of
    {badrpc, _} -> {error, not_found};
    Result      -> Result
  end.


%%------------------------------------------------------------------------------
%% @doc
%% Returns information about Module:Function/Arity on Node.
%% @end
%%
-spec get_function_info( Node    ::node()
                       , Module  ::module()
                       , Function::atom()
                       , Arity   ::non_neg_integer()) ->
                           [{atom(), term()}] | {error, not_found}.
%%------------------------------------------------------------------------------
get_function_info(Node, Module, Function, Arity) ->
  edts_log:debug("get_function info ~p:~p/~p on ~p",
              [Module, Function, Arity, Node]),
  Args = [Module, Function, Arity],
  case edts_dist:call(Node, edts_code, get_function_info, Args) of
    {badrpc, E} ->
      Fmt = "Error in remote call edts_code:get_function_info/3 on ~p: ~p",
      edts_log:error(Fmt, [Node, E]),
      {error, not_found};
    Info  -> Info
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Returns a list of the functions calling Module:Function/Arity on Node.
%% @end
%%
-spec who_calls( Node    ::node()
               , Module  ::module()
               , Function::atom()
               , Arity   ::non_neg_integer()) ->
                   [{module(), atom(), term()}].
%%------------------------------------------------------------------------------
who_calls(Node, Module, Function, Arity) ->
  edts_log:debug("who_calls ~p:~p/~p on ~p", [Module, Function, Arity, Node]),
  edts_server:wait_for_node(Node),
  Args = [Module, Function, Arity],
  case edts_dist:call(Node, edts_code, who_calls, Args) of
    {badrpc, E} ->
      Fmt = "Error in remote call edts_code:who_calls/2 on ~p: ~p",
      edts_log:error(Fmt, [Node, E]),
      {error, not_found};
    Info  -> Info
  end.


%%------------------------------------------------------------------------------
%% @doc
%% Returns information about Module on Node.
%% @end
%%
-spec get_module_info(Node::node(), Module::module(),
                      Level::detailed | basic) ->
                         {ok, [{atom(), term()}]}.
%%------------------------------------------------------------------------------
get_module_info(Node, Module, Level) ->
  edts_log:debug("get_module_info ~p, ~p on ~p", [Module, Level, Node]),
  case edts_dist:call(Node, edts_code, get_module_info, [Module, Level]) of
    {badrpc, E} ->
      Fmt = "Error in remote call edts_code:get_module_info/2 on ~p: ~p",
      edts_log:error(Fmt, [Node, E]),
      {error, not_found};
    Info  -> Info
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Returns the result of eunit tests in Module on Node
%% @end
%%
-spec get_module_eunit_result(Node::node(), Module::module()) ->
                                 {ok, [{error,
                                        File::file:filename(),
                                        non_neg_integer(),
                                        Desc::string()}]}.
%%------------------------------------------------------------------------------
get_module_eunit_result(Node, Module) ->
  edts_log:debug("get_module_eunit_result ~p, ~p", [Module, Node]),
  case edts_dist:call(Node, edts_eunit, run_tests, [Module]) of
    {badrpc, _} -> {error, not_found};
    Result      -> Result
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Returns the result of xref checks of Module on Node
%% @end
%%
-spec get_module_xref_analysis(Node::node(), Module::module(),
                      Checks::[atom()]) ->
                         {ok, [{error,
                                File::file:filename(),
                                non_neg_integer(),
                                Desc::string()}]}.
%%------------------------------------------------------------------------------
get_module_xref_analysis(Node, Module, Checks) ->
  edts_log:debug("get_module_xref_analysis ~p, ~p on ~p",
                 [Module, Checks, Node]),
  case edts_dist:call(Node, edts_code, check_module, [Module, Checks]) of
    {badrpc, E} ->
      Fmt = "Error in remote call edts_code:check_module/2 on ~p: ~p",
      edts_log:error(Fmt, [Node, E]),
      {error, not_found};
    Info  -> Info
  end.


%%------------------------------------------------------------------------------
%% @doc
%% Initializes a new edts node.
%% @end
%%
-spec init_node(Node::node(), filename:filename(), [string()]) -> ok.
%%------------------------------------------------------------------------------
init_node(Node, ProjectRoot, LibDirs) ->
  edts_server:init_node(Node, ProjectRoot, LibDirs).

%%------------------------------------------------------------------------------
%% @doc
%% Returns true iff Node is registered with this edts instance.
%% @end
%%
-spec is_node(Node::node()) -> boolean().
%%------------------------------------------------------------------------------
is_node(Node) ->
  edts_server:is_node(Node).

%%------------------------------------------------------------------------------
%% @doc
%% Returns true iff Node is registered with this edts instance and has fisished
%% its initialization.
%% @end
%%
-spec node_available_p(Node::node()) -> boolean().
%%------------------------------------------------------------------------------
node_available_p(Node) ->
  edts_server:node_available_p(Node).


%%------------------------------------------------------------------------------
%% @doc
%% Returns a list of all erlang modules available on Node.
%% @end
%%
-spec modules(Node::node()) -> [module()].
%%------------------------------------------------------------------------------
modules(Node) ->
  edts_server:wait_for_node(Node),
  edts_dist:call(Node, edts_code, modules).


%%------------------------------------------------------------------------------
%% @doc
%% Returns true if Node is registerend with the epmd on localhost.
%% @end
%%
-spec node_reachable(Node::node()) -> boolean().
%%------------------------------------------------------------------------------
node_reachable(Node) ->
  edts_server:node_registered_p(Node) andalso net_adm:ping(Node) =:= pong.

%%------------------------------------------------------------------------------
%% @doc
%% Returns a list of the edts_nodes currently registered with this
%% edts-instance.
%% @end
%%
-spec nodes() -> [node()].
%%------------------------------------------------------------------------------
nodes() ->
  edts_server:nodes().

%%%_* Internal functions =======================================================

%%%_* Tests ====================================================================

get_module_eunit_result_test_() ->
  [?_assertMatch({ok, [{'passed-test', _Source, 43, "no asserts failed"},
                       {'passed-test', _Source, 44, "no asserts failed"}]},
                 get_module_eunit_result(node(), test_module)),
   ?_assertEqual({error, not_found},
                 get_module_eunit_result(not_a_node, test_module)),
   ?_assertEqual({ok, []},
                 get_module_eunit_result(node(), not_a_module))].

get_dialyzer_result_test_() ->
  [{setup,
   fun() ->
       meck:unload(),
       meck:new(edts_dist)
   end,
   fun(_) ->
       meck:unload()
   end,
   [fun() ->
        F = fun(n, edts_dialyzer, run, ["plt", "out", "foo"]) ->
                {badrpc, error}
            end,
        meck:expect(edts_dist, call, F),
        ?assertEqual({error, not_found},
                     get_dialyzer_result(n, "plt", "out", "foo"))
    end,
    fun() ->
        F = fun(n, edts_dialyzer, run, ["plt", "out", "foo"]) -> res end,
        meck:expect(edts_dist, call, F),
        ?assertEqual(res, get_dialyzer_result(n, "plt", "out", "foo"))

    end]
  }].

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
