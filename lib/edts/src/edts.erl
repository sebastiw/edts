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
-export([ compile_and_load/2
        , debugger_continue/1
        , debugger_step/1
        , debugger_step_out/1
        , debugger_stop/1
        , debugger_toggle_breakpoint/3
        , get_breakpoints/1
        , get_function_info/4
        , get_module_eunit_result/2
        , get_module_info/3
        , get_module_xref_analysis/3
        , init_node/3
        , interpret_modules/2
        , interpret_node/2
        , is_node/1
        , is_node_interpreted/1
        , node_available_p/1
        , modules/1
        , node_reachable/1
        , nodes/0
        , uninterpret_modules/2
        , uninterpret_node/1
        , wait_for_debugger/1
        , who_calls/4]).

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
  edts_server:ensure_node_initialized(Node),
  case edts_dist:call(Node, edts_code, compile_and_load, [Filename]) of
    {badrpc, E} ->
      Fmt = "Error in remote call edts_code:compile_and_load/1 on ~p: ~p",
      edts_log:error(Fmt, [Node, E]),
      {error, not_found};
    Result      -> Result
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Returns information about all breakpoints on Node.
%% @end
%%
-spec get_breakpoints(Node :: node()) -> [{ { Module  :: module()
                                            , Line    :: non_neg_integer()
                                            }
                                          , Options :: [term()]
                                          }].
%%------------------------------------------------------------------------------
get_breakpoints(Node) ->
  case edts_dist:call(Node, edts_debug_server, get_breakpoints, []) of
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
  edts_server:ensure_node_initialized(Node),
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
%% Interprets all code loaded in Node, if possible, returning the list
%% of interpreted modules.
%% @end
-spec interpret_node( Node :: node(), Exclusions :: [module()] ) ->
                           [module()] | {error, not_found}.
%%------------------------------------------------------------------------------
interpret_node(Node, Exclusions) ->
  case edts_dist:call(Node, edts_debug_server, interpret_node, [Exclusions]) of
    {badrpc, _} -> {error, not_found};
    Interpreted -> {ok, Interpreted}
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Interprets Modules in Node, if possible, returning the list of interpreted
%% modules.
%% @end
-spec interpret_modules( Node :: node()
                       , Modules :: [module()] ) ->
                           [module()] | {error, not_found}.
%%------------------------------------------------------------------------------
interpret_modules(Node, Modules) ->
  case edts_dist:call(Node, edts_debug_server, interpret_modules, [Modules]) of
    {badrpc, _} -> {error, not_found};
    Interpreted -> Interpreted
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Uninterprets all code loaded in Node.
%% modules.
%% @end
-spec uninterpret_node( Node :: node() )
                      -> {ok, uninterpreted} | {error, not_found}.
%%------------------------------------------------------------------------------
uninterpret_node(Node) ->
  case edts_dist:call(Node, edts_debug_server, uninterpret_node, []) of
    {badrpc, _} -> {error, not_found};
    Result      ->  Result
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Uninterpret Modules in Node.
%% @end
-spec uninterpret_modules( Node :: node()
                         , Modules :: [module()] ) ->
                             ok | {error, not_found}.
%%------------------------------------------------------------------------------
uninterpret_modules(Node, Modules) ->
  case edts_dist:call(Node,
                      edts_debug_server, uninterpret_modules, [Modules]) of
    {badrpc, _} -> {error, not_found};
    ok          -> ok
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Toggles a breakpoint in Module:Line at Node.
%% @end
-spec debugger_toggle_breakpoint( Node   :: node()
                                , Module :: module()
                                , Line   :: non_neg_integer())
                                ->
                                    {ok, set, {Module, Line}}
                                    | {ok, unset, {Module, Line}}
                                    | {error, not_found}.
%%------------------------------------------------------------------------------
debugger_toggle_breakpoint(Node, Module, Line) ->
  Args = [Module, Line],
  case edts_dist:call(Node, edts_debug_server, toggle_breakpoint, Args) of
    {badrpc, _} -> {error, not_found};
    Result      -> Result
  end.


%%------------------------------------------------------------------------------
%% @doc
%% Step through in execution while debugging, in Node.
%% @end
-spec debugger_step(Node :: node())
                   -> {ok, Info :: term()} | {error, not_found}.
%%------------------------------------------------------------------------------
debugger_step(Node) ->
  case edts_dist:call(Node, edts_debug_server, step, []) of
    {badrpc, _} -> {error, not_found};
    Result      -> Result
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Step out of the current function while debugging, in Node.
%% @end
-spec debugger_step_out(Node :: node())
                       -> {ok, Info :: term()} | {error, not_found}.
%%------------------------------------------------------------------------------
debugger_step_out(Node) ->
  case edts_dist:call(Node, edts_debug_server, step_out, []) of
    {badrpc, _} -> {error, not_found};
    Result      -> Result
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Continue execution until a breakpoint is hit or execution terminates.
%% @end
-spec debugger_continue(Node :: node())
                       -> {ok, Info :: term()} | {error, not_found}.
%%------------------------------------------------------------------------------
debugger_continue(Node) ->
  case edts_dist:call(Node, edts_debug_server, continue, []) of
    {badrpc, _} -> {error, not_found};
    Result      -> Result
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Stop debugging
%% @end
-spec debugger_stop(Node :: node()) -> {ok, finished} | {error, not_found}.
%%------------------------------------------------------------------------------
debugger_stop(Node) ->
  case edts_dist:call(Node, edts_debug_server, stop_debug, []) of
    {badrpc, _} -> {error, not_found};
    Result      -> Result
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Wait for debugger to attach.
%% @end
-spec wait_for_debugger(Node :: node()) ->
                           {ok, {module(), non_neg_integer()}}
                         | {error, not_found}.
%%------------------------------------------------------------------------------
wait_for_debugger(Node) ->
  case edts_dist:call(Node, edts_debug_server, wait_for_debugger, []) of
    {badrpc, _} -> {error, not_found};
    Result      -> Result
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Returns true iff Node is running interpreted code.
%% @end
%%
-spec is_node_interpreted(Node::node()) -> boolean().
%%------------------------------------------------------------------------------
is_node_interpreted(Node) ->
  case edts_dist:call(Node, edts_debug_server, is_node_interpreted, []) of
    {badrpc, _} -> {error, not_found};
    Result      -> {ok, Result}
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
  edts_server:ensure_node_initialized(Node),
  edts_dist:call(Node, edts_code, modules).


%%------------------------------------------------------------------------------
%% @doc
%% Returns true if Node is registerend with the epmd on localhost.
%% @end
%%
-spec node_reachable(Node::node()) -> boolean().
%%------------------------------------------------------------------------------
node_reachable(Node) ->
  case net_adm:ping(Node) of
    pong -> true;
    pang -> false
  end.

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
  [ ?_assertMatch({ok, [ {'passed-test', _Source, 43, "no asserts failed"}
                       , {'passed-test', _Source, 44, "no asserts failed"}
                       ]},
                  get_module_eunit_result(node(), test_module))
  , ?_assertEqual({error, not_found},
                  get_module_eunit_result(not_a_node, test_module))
  , ?_assertEqual({ok, []},
                  get_module_eunit_result(node(), not_a_module))

  ].

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
