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
%-define(DEBUG, true).

%%%_* Types ====================================================================
-type eunit_info()    :: orddict:orddict().
-type eunit_result()  :: {ok, {eunit_summary(), [eunit_test()]}}
                       | {error, any()}.
-type eunit_summary() :: orddict:orddict().
-type eunit_test()    :: {mfa(), [eunit_info()]}.

%%%_* API ======================================================================

%%------------------------------------------------------------------------------
%% @doc Run eunit tests on Module and return result as "issues".
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
  debug("running eunit tests in: ~p", [Module]),
  Listener = ?MODULE:start([{parent, self()}]),
  case eunit_server:start_test(eunit_server, Listener, Module, []) of
    {ok, Ref}    -> run_tests(Ref, Listener);
    {error, Err} -> {error, Err}
  end.

run_tests(Ref, Listener) ->
  debug("waiting for start..."),
  receive
    {start, Ref} -> Listener ! {start, Ref}
  end,
  debug("waiting for result..."),
  receive
    {result, Ref, Result} -> {ok, Result};
    {error, Err}          -> {error, Err}
  after
    5000 -> {error, timeout}
  end.

format_test({{M,_,_}, _}, Module) when Module =/= M ->
  debug("format test, module not matching ~p =/= ~p", [Module, M]),
  [];
format_test({Mfa, []}, _Module) ->
  debug("passed test: ~w", [Mfa]),
  {'passed test', get_source(Mfa), get_line(Mfa), "no asserts failed"};
format_test({Mfa, Fails}, _Module) ->
  debug("failed test: ~w", [Mfa]),
  Formatted      = lists:flatten([format_fail(Mfa, Fail) || Fail <- Fails]),
  [ {'failed test', get_source(Mfa), get_line(Mfa), failed_test_str(Formatted)}
  | Formatted].

failed_test_str([])             -> "test aborted";
failed_test_str([{_,_,Line,_}]) -> format("failed assert on line ~w", [Line]);
failed_test_str(Formatted)      ->
  Lines    = lists:sort([integer_to_list(Line) || {_,_,Line,_} <- Formatted]),
  LinesStr = string:join(Lines, ", "),
  format("~p failed asserts on lines ~s", [length(Lines), LinesStr]).

get_source({Module, Function, Arity}) ->
  Info = edts_code:get_function_info(Module, Function, Arity),
  {source, Source} = lists:keyfind(source, 1, Info),
  Source.

get_line({Module, Function, Arity}) ->
  Info = edts_code:get_function_info(Module, Function, Arity),
  {line, Line} = lists:keyfind(line, 1, Info),
  Line.

format_fail(_Mfa, [{reason, _Reason}]) -> [];
format_fail({M,_F,_A} = Mfa, Info)     ->
  Module = orddict:fetch(module, Info),
  Line   = orddict:fetch(line,   Info),
  case Module =:= M of
    true  -> {'failed test', get_source(Mfa), Line, format_reason(Info)};
    false -> []
  end.

format_reason(Info) ->
  Fetch = fun(Key) ->
              case orddict:find(Key, Info) of
                {ok, Val} -> Val;
                error     -> undefined
              end
          end,
  {Expected, Got} = fmt(Fetch(reason), Fetch),
  format("(~p) expected: ~s, got: ~s",
         [Fetch(reason), to_str(Expected), to_str(Got)]).

fmt(assertException_failed,    F) -> format_args_assert_exception(F);
fmt(assertNotException_failed, F) -> format_args_assert_exception(F);
fmt(assertCmdOutput_failed,    F) -> {F(expected_output), F(output)};
fmt(assertCmd_failed,          F) -> {F(expected_status), F(status)};
fmt(assertEqual_failed,        F) -> {F(expected),        F(value)};
fmt(assertMatch_failed,        F) -> {F(pattern),         F(value)};
fmt(assertNotEqual_failed,     F) -> {F(expected),        F(expected)};
fmt(assertNotMatch_failed,     F) -> {F(pattern),         F(value)};
fmt(assertion_failed,          F) -> {F(expected),        F(value)};
fmt(command_failed,            F) -> {F(expected_status), F(status)};
fmt(_Reason, _F)                  -> {undefined,          undefined}.

%% We want to format with ~p, except that we don't want it to add line breaks
to_str(X) ->
  lists:filter(fun (C) -> C =/= $\n end, format("~p", [X])).

format_args_assert_exception(Fetch) ->
  case Fetch(unexpected_exception) of
    undefined             -> {Fetch(pattern), Fetch(unexpected_success)};
    {ExType, Ex, [{M,F,A}|_]} ->
      {Fetch(pattern), format("~w:~w in ~w:~w/~w", [ExType, Ex, M, F, A])}
  end.

format(FormatStr, Args) ->
  lists:flatten(io_lib:format(FormatStr, Args)).

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

mk_fail({Reason, Info}) -> orddict:from_list([{reason, Reason}|Info]);
mk_fail(Reason)         -> orddict:from_list([{reason, Reason}]).

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
debug(FmtStr, Args) -> ct:pal(FmtStr, Args).
-else.
debug(_FmtStr, _Args) -> ok.
-endif.

%%%_* Tests ====================================================================

format_test_test_() ->
  { setup
  , mock_get_function_info()
  , fun meck:unload/1
  , [ ?_assertEqual([], format_test({{m,f,a}, []}, not_m))
    , ?_assertEqual({'passed test', "m.erl", 1, "no asserts failed"},
                    format_test({{m,f,a}, []}, m))
    , ?_assertEqual([{'failed test', "m.erl", 1, "test aborted"}],
                    format_test({{m,f,a}, [[{reason, foo}]]}, m))
    ]
  }.

failed_test_str_test_() ->
  [ ?_assertEqual("test aborted",            failed_test_str([]))
  , ?_assertEqual("failed assert on line 1", failed_test_str([{a, b, 1, c}]))
  , ?_assertEqual("2 failed asserts on lines 1, 2",
                  failed_test_str([{a, b, 1, c}, {a, b, 2, c}]))
  ].

format_fail_test_() ->
  { setup
  , mock_get_function_info()
  , fun meck:unload/1
  , [ ?_assertEqual([], format_fail({m,f,a}, [{reason, foo}]))
    , ?_assertEqual([], format_fail({m,f,a}, [{line, 2}, {module, not_m}]))
    , ?_assertEqual({'failed test', "m.erl", 2,
                     "(undefined) expected: undefined, got: undefined"},
                    format_fail({m,f,a}, [{line, 2}, {module, m}]))
    ]
  }.

mock_get_function_info() ->
 fun() ->
     F = fun(_, _, _) -> [{line, 1}, {source, "m.erl"}] end,
     meck:new(edts_code, [passthrough]),
     meck:expect(edts_code, get_function_info, F),
     edts_code
 end.

format_reason_test_() ->
  [ ?_assertEqual("(undefined) expected: undefined, got: undefined",
                  format_reason([]))
  , ?_assertEqual("(foo) expected: undefined, got: undefined",
                  format_reason([{reason, foo}]))
  , ?_assertEqual("(assertion_failed) expected: foo, got: bar",
                  format_reason([ {expected, foo}
                                , {reason,   assertion_failed}
                                , {value,    bar}]))
  ].

fmt_test_() ->
  F  = fun(expected_output)    -> a;
          (expected_status)    -> a;
          (expected)           -> a;
          (pattern)            -> a;
          (output)             -> b;
          (status)             -> b;
          (value)              -> b;
          (unexpected_success) -> b;
          (_)                  -> undefined
       end,
  [ ?_assertEqual({a, b}, fmt(assertException_failed,     F))
  , ?_assertEqual({a, b}, fmt(assertNotException_failed,  F))
  , ?_assertEqual({a, b}, fmt(assertCmdOutput_failed,     F))
  , ?_assertEqual({a, b}, fmt(assertCmd_failed,           F))
  , ?_assertEqual({a, b}, fmt(assertEqual_failed,         F))
  , ?_assertEqual({a, b}, fmt(assertMatch_failed,         F))
  , ?_assertEqual({a, a}, fmt(assertNotEqual_failed,      F))
  , ?_assertEqual({a, b}, fmt(assertNotMatch_failed,      F))
  , ?_assertEqual({a, b}, fmt(assertion_failed,           F))
  , ?_assertEqual({a, b}, fmt(command_failed,             F))
  , ?_assertEqual({undefined, undefined}, fmt(foo,        F))
  ].

to_str_test_() ->
  [ ?_assertEqual("foo",         to_str(foo))
  , ?_assertEqual("\"foo\"",     to_str("foo"))
  , ?_assertEqual("\"foo\"",     to_str("foo"))
  , ?_assertEqual("[{foo,123}]", to_str([{foo,123}]))
  ].

format_args_assert_exception_test_() ->
  Def   = fun(unexpected_exception) -> {a, b, [{x, y, 0}]};
             (pattern)              -> a
          end,
 Undef  = fun(unexpected_exception) -> undefined;
             (pattern)              -> a;
             (unexpected_success)   -> b
          end,
  [ ?_assertEqual({a, "a:b in x:y/0"}, format_args_assert_exception(Def))
  , ?_assertEqual({a, b},              format_args_assert_exception(Undef))
  ].

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
  [ ?_assertEqual([{reason, foo}],             mk_fail(foo))
  , ?_assertEqual([{reason, foo}],             mk_fail({foo, []}))
  , ?_assertEqual([{aaa, bar}, {reason, foo}], mk_fail({foo, [{aaa, bar}]}))
  ].

handle_cancel_test() ->
  State = #state{ref=foo, parent=bar, tests=baz},
  ?assertEqual(State, handle_cancel(l, data, State)).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
