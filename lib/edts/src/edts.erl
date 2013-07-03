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
-export([call/3,
         call/4,
         call/5,
         compile_and_load/2,
         debugger_continue/1,
         debugger_step/1,
         debugger_step_out/1,
         debugger_stop/1,
         debugger_toggle_breakpoint/3,
         get_breakpoints/1,
         get_dialyzer_result/4,
         get_function_info/4,
         get_module_eunit_result/2,
         get_module_info/3,
         get_module_xref_analysis/3,
         init_node/6,
         is_node/1,
         node_available_p/1,
         modules/1,
         node_reachable/1,
         nodes/0,
         wait_for_debugger/1,
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
  call(Node, edts_code, compile_and_load, [Filename]).


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
  call(Node, edts_dialyzer, run, [OtpPlt, OutPlt, Files]).


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
  call(Node, edts_debug_server, get_breakpoints).


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
  call(Node, edts_code, get_function_info, [Module, Function, Arity]).


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
  call(Node, edts_code, who_calls, [Module, Function, Arity]).


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
  call(Node, edts_debug_server, toggle_breakpoint, [Module, Line]).


%%------------------------------------------------------------------------------
%% @doc
%% Step through in execution while debugging, in Node.
%% @end
-spec debugger_step(Node :: node())
                   -> {ok, Info :: term()} | {error, not_found}.
%%------------------------------------------------------------------------------
debugger_step(Node) ->
  call(Node, edts_debug_server, step).

%%------------------------------------------------------------------------------
%% @doc
%% Step out of the current function while debugging, in Node.
%% @end
-spec debugger_step_out(Node :: node())
                       -> {ok, Info :: term()} | {error, not_found}.
%%------------------------------------------------------------------------------
debugger_step_out(Node) ->
  call(Node, edts_debug_server, step_out).

%%------------------------------------------------------------------------------
%% @doc
%% Continue execution until a breakpoint is hit or execution terminates.
%% @end
-spec debugger_continue(Node :: node())
                       -> {ok, Info :: term()} | {error, not_found}.
%%------------------------------------------------------------------------------
debugger_continue(Node) ->
  call(Node, edts_debug_server, continue).

%%------------------------------------------------------------------------------
%% @doc
%% Stop debugging
%% @end
-spec debugger_stop(Node :: node()) -> {ok, finished} | {error, not_found}.
%%------------------------------------------------------------------------------
debugger_stop(Node) ->
  call(Node, edts_debug_server, stop_debug).

%%------------------------------------------------------------------------------
%% @doc
%% Wait for debugger to attach.
%% @end
-spec wait_for_debugger(Node :: node()) ->
                           {ok, {module(), non_neg_integer()}}
                         | {error, not_found}.
%%------------------------------------------------------------------------------
wait_for_debugger(Node) ->
  call(Node, edts_debug_server, wait_for_debugger).

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
  call(Node, edts_code, get_module_info, [Module, Level]).


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
  call(Node, edts_eunit, run_tests, [Module]).


%%------------------------------------------------------------------------------
%% @doc
%% Returns the result of xref checks of Module on Node
%% @end
%%
-spec get_module_xref_analysis(Node::node(), [Modules::module()],
                      Checks::[atom()]) ->
                         {ok, [{error,
                                File::file:filename(),
                                non_neg_integer(),
                                Desc::string()}]}.
%%------------------------------------------------------------------------------
get_module_xref_analysis(Node, Modules, Checks) ->
  edts_log:debug("get_module_xref_analysis ~p, ~p on ~p",
                 [Modules, Checks, Node]),
  call(Node, edts_code, check_modules, [Modules, Checks]).


%%------------------------------------------------------------------------------
%% @doc
%% Initializes a new edts node.
%% @end
%%
-spec init_node(ProjectName    :: string(),
                Node           :: node(),
                ProjectRoot    :: filename:filename(),
                LibDirs        :: [filename:filename()],
                AppIncludeDirs :: [filename:filename()],
                SysIncludeDirs :: [filename:filename()]) -> ok.
%%------------------------------------------------------------------------------
init_node(ProjectName,
          Node,
          ProjectRoot,
          LibDirs,
          AppIncludeDirs,
          SysIncludeDirs) ->
  edts_server:init_node(ProjectName,
                        Node,
                        ProjectRoot,
                        LibDirs,
                        AppIncludeDirs,
                        SysIncludeDirs).

%%------------------------------------------------------------------------------
%% @doc
%% Returns true iff Node is registered with this edts instance.
%% @end
%%
-spec is_node(Node::node()) -> boolean().
%%------------------------------------------------------------------------------
is_node(Node) ->
  edts_server:node_registered_p(Node).

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
  call(Node, edts_code, modules).


%%------------------------------------------------------------------------------
%% @doc
%% Returns true if Node is registerend with the epmd on localhost.
%% @end
%%
-spec node_reachable(Node::node()) -> boolean().
%%------------------------------------------------------------------------------
node_reachable(Node) ->
  net_adm:ping(Node) =:= pong.

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

call(Node, Mod, Fun) ->
  call(Node, Mod, Fun, []).

call(Node, Mod, Fun, Args) ->
  call(Node, Mod, Fun, Args, true).

call(Node, Mod, Fun, Args, LogError) ->
  try {ok, edts_dist:call(Node, Mod, Fun, Args)}
  catch
    error:Err ->
      case LogError of
        false -> ok;
        true  ->
          edts_log:error("Error in remote call ~p:~p/~p on ~p: ~p",
                         [Mod, Fun, length(Args), Node, Err])
      end,
      {error, Err}
  end.

%%%_* Tests ====================================================================

get_module_eunit_result_test_() ->
  [?_assertMatch({ok, [{'passed-test', _Source, Line1, "no asserts failed"},
                       {'passed-test', _Source, Line2, "no asserts failed"}]}
                 when is_integer(Line1) andalso is_integer(Line2),
                 get_module_eunit_result(node(), edts_test_module)),
   ?_assertMatch({error, _},
                 get_module_eunit_result(not_a_node, edts_test_module)),
   ?_assertMatch({error, _},
                 get_module_eunit_result(node(), not_a_module))
  ].

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
                {error, foo}
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
