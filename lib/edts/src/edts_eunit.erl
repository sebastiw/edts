%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Provides support for running eunit tests from EDTS
%%% @end
%%% @author Hakan Nilsson
%%% @copyright
%%% Copyright 2012 Hakan Nilsson
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
-module(edts_eunit).

-behaviour(eunit_listener).

%%%_* Includes =================================================================
-include_lib("eunit/include/eunit.hrl").

%%%_* Exports ==================================================================

%% API
-export([ test/1
        , run_tests/1
        ]).

%% Eunit listener callbacks
-export([ handle_begin/3
        , handle_cancel/3
        , handle_end/3
        , init/1
        , start/0
        , start/1
        , terminate/2
        ]).

%%%_* Defines ==================================================================
-define(DEBUG, true).

%%%_* Types ====================================================================

-type eunit_info()    :: orddict:orddict().
-type eunit_result()  :: {ok, {eunit_summary(), [eunit_test()]}}
                       | {error, any()}.
-type eunit_summary() :: orddict:orddict().
-type eunit_test()    :: {mfa(), [eunit_info()]}.

%%%_* API ======================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Run eunit tests on Module and return result as "issues".
-spec test(Module::module()) -> {ok, [edts_code:issue()]}
                              | {error, Reason::any()}.
%%------------------------------------------------------------------------------
test(Module) ->
  case run_tests(Module) of
    {ok, {Summary, Tests}} ->
      debug("run tests returned ok: ~p", [Summary]),
      {ok, lists:flatten([format_test(Test, Module) || Test <- Tests])};
    {error, Reason} = Error ->
      debug("Error in eunit test result in ~p: ~p", [Module, Reason]),
      Error
  end.

%%%_* Internal functions =======================================================

%% @doc Run eunit tests in `Module' and return the test result
-spec run_tests(Module::module()) -> Result::eunit_result().
run_tests(Module) ->
  Options = [{report, {?MODULE, [{parent, self()}]}}],
  eunit:test(Module, Options),
  receive
    {result, Result} -> {ok, Result};
    {error, _} = Err -> Err
  after
    5000 -> {error, timeout}
  end.

format_test({{M,_,_}, _}, Module) when Module =/= M ->
  debug("format test, module not matching ~p =/= ~p", [Module, M]),
  [];
format_test({Mfa, []}, _Module) ->
  debug("passed test: ~w", [Mfa]),
  {Source, Line} = get_source_and_line(Mfa),
  {'passed test', Source, Line, ""};
format_test({Mfa, Fails}, _Module) ->
  debug("failed test: ~w", [Mfa]),
  Formatted = [format_fail(Mfa, Fail) || Fail <- Fails],
  {Source, Line} = get_source_and_line(Mfa),
  [ {'failed test', Source, Line, "testcase failed"}
  | Formatted].

get_source_and_line({Module, Function, Arity}) ->
  Info = edts_code:get_function_info(Module, Function, Arity),
  {source, Source} = lists:keyfind(source, 1, Info),
  {line, Line}     = lists:keyfind(line,   1, Info),
  {Source, Line}.

format_fail(_Mfa, [{reason, _Reason}]) -> [];
format_fail({M,_F,_A} = Mfa, Info)    ->
  Module = orddict:fetch(module, Info),
  Line   = orddict:fetch(line,   Info),
  case Module =:= M of
    false -> [];
    true  ->
      {Source, _} = get_source_and_line(Mfa),
      {'failed test', Source, Line, format_reason(Info)}
  end.

format_reason(Info) ->
  Get = fun(Key) ->
            case orddict:find(Key, Info) of
              {ok, Value} -> Value;
              error       -> undefined
            end
        end,
  Reason = Get(reason),
  {FormatStr, Args} =
    case Reason of
      assertEqual_failed ->
        {"expected: ~w, got value: ~w", [Get(expected), Get(value)]};
      assertNotEqual_failed ->
        {"expected: ~w, got value: ~w", [Get(expected), Get(expected)]};
      assertException_failed ->
        case Get(unexpected_exception) of
          {ExceptionType, Exception, [Mfa|_]} ->
            { "expected exception: ~w, got exception: ~w:~w in ~w"
            , [Get(pattern), ExceptionType, Exception, Mfa]};
          undefined ->
            { "expected exception: ~w, got value: ~w"
            , [Get(pattern), Get(unexpected_success)]}
        end;
      assertNotException_failed ->
        {ExceptionType, Exception, [Mfa|_]} = Get(unexpected_exception),
        { "expected exception: ~w, got exception: ~w:~w in ~w"
        , [Get(pattern), ExceptionType, Exception, Mfa]};
      assertMatch_failed ->
        {"expected: ~w, got value: ~w", [Get(pattern), Get(value)]};
      assertNotMatch_failed ->
        {"expected: ~w, got value: ~w", [Get(pattern), Get(value)]};
      assertion_failed ->
        {"expected: ~w, got value: ~w", [Get(expected), Get(value)]};
      command_failed ->
        { "expected status: ~w, got status: ~w"
        , [Get(expected_status), Get(status)]};
      assertCmd_failed ->
        { "expected status: ~w, got status: ~w"
        , [Get(expected_status), Get(status)]};
      assertCmdOutput_failed ->
        { "expected output: ~w, got output: ~w"
        , [Get(expected_output), Get(output)]};
      Reason ->
        {"unknown failure", []}
    end,
  lists:flatten(io_lib:format("(~p) " ++ FormatStr, [Reason|Args])).

%%%_* Listener =================================================================

-record(state, { parent
               , ref
               , tests = []
               }).

start() -> start([]).

start(Options) -> eunit_listener:start(?MODULE, Options).

init(Options) ->
  debug("init with options: ~p", [Options]),
  receive
    {start, Reference} ->
      #state{ ref    = Reference
            , parent = proplists:get_value(parent, Options)
            }
  end.

handle_begin(test, Data, #state{tests=Tests} = State) ->
  debug("begin test: ~p", [Data]),
  Source = proplists:get_value(source, Data),
  case orddict:is_key(Source, Tests) of
    true  -> State;
    false -> State#state{tests = orddict:store(Source, [], Tests)}
  end;
handle_begin(L, Data, State) ->
  debug("begin ~p: ~p", [L, Data]),
  State.

handle_end(test, Data, #state{tests=Tests} = State) ->
  debug("end test: ~p", [Data]),
  case proplists:get_value(status, Data, ok) of
    ok              -> State;
    {error, {_What, Error, _StackTrace}} ->
      Source = proplists:get_value(source, Data),
      Fail   = mk_fail(Error),
      Fails  = orddict:fetch(Source, Tests),
      State#state{tests = orddict:store(Source, [Fail|Fails], Tests)};
    Other           ->
      debug("other: ~p", [Other]),
      State
  end;
handle_end(L, Data, State) ->
  debug("end ~p: ~p", [L, Data]),
  State.

mk_fail({Reason, Info}) ->
  orddict:from_list([{reason, Reason}|Info]);
mk_fail(Reason) ->
  mk_fail({Reason, []}).

handle_cancel(L, Data, State) ->
  debug("cancel ~p: ~p", [L, Data]),
  State.

terminate({ok, Summary}, State) ->
  debug("terminate: ~p", [Summary]),
  #state{ref=Ref, parent=Parent, tests=Tests} = State,
  receive
    {stop, Ref, _Pid} ->
      debug("sending to ~p: ~p", [Parent, Tests]),
      Parent ! {result, {orddict:from_list(Summary), Tests}}
  end;
terminate(Other, _State) ->
  debug("terminate: ~p", [Other]),
  {error, Other}.

-ifdef(DEBUG).
debug(FmtStr, Args) ->
  ct:pal(FmtStr, Args).
-else.
debug(_FmtStr, _Args) ->
  ok.
-endif.

%%%_* Tests ====================================================================

init_test() ->
  self() ! {start, ref},
  ?assertEqual(#state{ref = ref, parent = foo}, init([{parent, foo}])).

handle_begin_test_() ->
  [ ?_assertEqual(#state{tests=[{a, []}]},
                  handle_begin(test, [{source, a}], #state{tests=[]}))
  , ?_assertEqual(#state{tests=[{a, [1]}]},
                  handle_begin(test, [{source, a}], #state{tests=[{a, [1]}]}))
  , ?_assertEqual(#state{tests=[{a, [1]}, {b, []}]},
                  handle_begin(test, [{source, b}], #state{tests=[{a, [1]}]}))
  , ?_assertEqual(#state{}, handle_begin(foo, [], #state{}))
  ].

handle_end_test_() ->
  [ ?_assertEqual(#state{tests=[{foo, []}]},
                  handle_end(test, [], #state{tests=[{foo, []}]}))
  , ?_assertEqual(#state{tests=[{foo, []}]},
                  handle_end(foo, [], #state{tests=[{foo, []}]}))
  , ?_assertEqual(#state{tests=[{foo, []}]},
                  handle_end(foo, [{status, asdf}], #state{tests=[{foo, []}]}))
  , ?_assertEqual(#state{tests=[{foo, [[{reason, bar}]]}]},
                  handle_end(test, [ {status, {error, {error, bar, st}}}
                                   , {source, foo}
                                   ], #state{tests=[{foo, []}]}))
  , ?_assertEqual(#state{tests=[{foo, [[{a, 1}, {reason, bar}]]}]},
                  handle_end(test,
                             [ {status, {error, {error, {bar, [{a,1}]}, st}}}
                             , {source, foo}
                             ], #state{tests=[{foo, []}]}))
  , ?_assertEqual(#state{tests=[{foo, [[{a, 1}, {reason, bar}], baz]}]},
                  handle_end(test,
                             [ {status, {error, {error, {bar, [{a,1}]}, st}}}
                             , {source, foo}
                             ], #state{tests=[{foo, [baz]}]}))
  ].

terminate_test() ->
  State = #state{ref=ref, parent=self(), tests=[foo, bar]},
  Exp   = {result, {[{a, 1}, {b, 2}], [foo, bar]}},
  self() ! {stop, ref, self()},
  ?assertEqual(Exp, terminate({ok, [{b, 2}, {a, 1}]}, State)),
  ?assertEqual(Exp, receive Res -> Res end),
  ?assertEqual({error, foo}, terminate(foo, State)).

mk_fail_test_() ->
  [ ?_assertEqual([{reason, foo}]             , mk_fail(foo))
  , ?_assertEqual([{reason, foo}]             , mk_fail({foo, []}))
  , ?_assertEqual([{aaa, bar}, {reason, foo}] , mk_fail({foo, [{aaa, bar}]}))
  ].

handle_cancel_test() ->
  State = #state{ref=foo, parent=bar, tests=baz},
  ?assertEqual(State, handle_cancel(l, data, State)).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
