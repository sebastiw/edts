%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Provides support for running eunit tests from EDTS
%%% @end
%%% @author Håkan Nilsson <haakan@gmail.com>
%%% @copyright
%%% Copyright 2012 Håkan Nilsson <haakan@gmail.com>
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

%%%_* Includes =================================================================
-include_lib("eunit/include/eunit.hrl").

%%%_* Exports ==================================================================

%% API
-export([ run_tests/1
        ]).

%%%_* Defines ==================================================================
%-define(DEBUG, true).

%%%_* Types ====================================================================
-type info()    :: orddict:orddict().
-type result()  :: {ok, {summary(), [test()]}}
                 | {error, term()}.
-type summary() :: orddict:orddict().
-type test()    :: {mfa(), [info()]}.
-type reason()  :: atom().

%%%_* API ======================================================================

%%------------------------------------------------------------------------------
%% @doc Run eunit tests on Module and return result as "issues".
-spec run_tests(module()) -> {ok, [edts_code:issue()]}
                           | {error, term()}.
%%------------------------------------------------------------------------------
run_tests(Module) ->
  case do_run_tests(Module) of
    {ok, {Summary, Tests}} ->
      debug("run tests returned ok: ~p", [Summary]),
      {ok, lists:flatten([format_test_result(Test, Module) || Test <- Tests])};
    {error, Reason} = Error ->
      debug("Error in eunit test result in ~p: ~p", [Module, Reason]),
      Error
  end.

%%%_* Internal functions =======================================================

-spec do_run_tests(module()) -> result().
do_run_tests(Module) ->
  debug("running eunit tests in: ~p", [Module]),
  Listener = edts_eunit_listener:start([{parent, self()}]),
  case eunit_server:start_test(eunit_server, Listener, Module, []) of
    {ok, Ref}    -> do_run_tests(Ref, Listener);
    {error, Err} -> {error, Err}
  end.

-spec do_run_tests(reference(), pid()) -> result().
do_run_tests(Ref, Listener) ->
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

-spec format_test_result(test(), module()) -> [edts_code:issue()].
format_test_result({{M,_,_}, _}, Module) when Module =/= M ->
  debug("format_test_result: module not matching ~p =/= ~p", [Module, M]),
  [];
format_test_result({Mfa, []}, _Module) ->
  debug("passed test: ~w", [Mfa]),
  {Source, Line} = get_source_and_line(Mfa),
  [{'passed-test', Source, Line, "no asserts failed"}];
format_test_result({Mfa, Fails}, _Module) ->
  debug("failed test: ~w", [Mfa]),
  Formatted      = lists:flatten([format_fail(Mfa, Fail) || Fail <- Fails]),
  {Source, Line} = get_source_and_line(Mfa),
  [ {'failed-test', Source, Line, failed_test_str(Formatted)}
  | Formatted].

-spec failed_test_str([edts_code:issue()]) -> string().
failed_test_str([])             -> "test aborted";
failed_test_str([{_,_,Line,_}]) -> format("failed assert on line ~w", [Line]);
failed_test_str(Formatted)      ->
  Lines    = lists:sort([integer_to_list(Line) || {_,_,Line,_} <- Formatted]),
  LinesStr = string:join(Lines, ", "),
  format("~p failed asserts on lines ~s", [length(Lines), LinesStr]).

-spec get_source_and_line(mfa()) -> {string(), non_neg_integer()}.
get_source_and_line({Module, Function, Arity}) ->
  Info             = edts_code:get_function_info(Module, Function, Arity),
  {line, Line}     = lists:keyfind(line,   1, Info),
  {source, Source} = lists:keyfind(source, 1, Info),
  {Source, Line}.

-spec format_fail(mfa(), info()) -> edts_code:issue() | [].
format_fail(_Mfa, [{reason, _Reason}]) -> [];
format_fail({M,_F,_A} = Mfa, Info)     ->
  Module      = orddict:fetch(module, Info),
  Line        = orddict:fetch(line,   Info),
  {Source, _} = get_source_and_line(Mfa),
  case Module =:= M of
    true  -> {'failed-test', Source, Line, format_reason(Info)};
    false -> []
  end.

-spec format_reason(info()) -> string().
format_reason(Info) ->
  Fetch = fun(Key) ->
              case orddict:find(Key, Info) of
                {ok, Val} -> Val;
                error     -> undefined
              end
          end,
  {Expected, Got} = fmt(Fetch(reason), Fetch),
  format("(~p)\n"
         "expected: ~s\n"
         "     got: ~s",
         [Fetch(reason), to_str(Expected), to_str(Got)]).

-spec fmt(reason(), fun((atom()) -> term())) -> {term(), term()}.
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

-spec to_str(term()) -> string().
to_str(Term) ->
  %% We want to format with ~p, except that we don't want it to add line breaks
  lists:filter(fun (C) -> C =/= $\n end, format("~p", [Term])).

-spec format_args_assert_exception(fun((atom()) -> term())) -> {term(), term()}.
format_args_assert_exception(Fetch) ->
  case Fetch(unexpected_exception) of
    undefined             -> {Fetch(pattern), Fetch(unexpected_success)};
    {ExType, Ex, [{M,F,A}|_]} ->
      {Fetch(pattern), format("~w:~w in ~w:~w/~w", [ExType, Ex, M, F, A])}
  end.

-spec format(string(), [term()]) -> string().
format(FormatStr, Args) ->
  lists:flatten(io_lib:format(FormatStr, Args)).

debug(Str) -> debug(Str, []).

-ifdef(DEBUG).
debug(FmtStr, Args) -> error_logger:info_msg(FmtStr, Args).
-else.
debug(_FmtStr, _Args) -> ok.
-endif.

%%%_* Unit tests ===============================================================

do_run_tests_ok_test() ->
  {Ref, Pid} = run_tests_common(),
  Pid ! {result, Ref, foo},
  assert_receive({ok, foo}).

do_run_tests_error_test() ->
  {_Ref, Pid} = run_tests_common(),
  Pid ! {error, foobar},
  assert_receive({error, foobar}).

format_test_result_test_() ->
  { setup
  , mock_get_function_info()
  , fun meck:unload/1
  , [ ?_assertEqual([], format_test_result({{m,f,a}, []}, not_m))
    , ?_assertEqual([{'passed-test', "m.erl", 1, "no asserts failed"}],
                    format_test_result({{m,f,a}, []}, m))
    , ?_assertEqual([{'failed-test', "m.erl", 1, "test aborted"}],
                    format_test_result({{m,f,a}, [[{reason, foo}]]}, m))
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
    , ?_assertEqual({'failed-test', "m.erl", 2,
                     "(undefined)\nexpected: undefined\n     got: undefined"},
                    format_fail({m,f,a}, [{line, 2}, {module, m}]))
    ]
  }.

format_reason_test_() ->
  [ ?_assertEqual("(undefined)\nexpected: undefined\n     got: undefined",
                  format_reason([]))
  , ?_assertEqual("(foo)\nexpected: undefined\n     got: undefined",
                  format_reason([{reason, foo}]))
  , ?_assertEqual("(assertion_failed)\nexpected: foo\n     got: bar",
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
  , ?_assertEqual("[{foo,123}]", to_str([{foo,123}]))
  ].

format_args_assert_exception_test_() ->
  Def   = fun(unexpected_exception) -> {a, b, [{x, y, 0}]};
             (pattern)              -> a
          end,
  Undef = fun(unexpected_exception) -> undefined;
             (pattern)              -> a;
             (unexpected_success)   -> b
          end,
  [ ?_assertEqual({a, "a:b in x:y/0"}, format_args_assert_exception(Def))
  , ?_assertEqual({a, b},              format_args_assert_exception(Undef))
  ].

%%%_* Test helpers -------------------------------------------------------------

mock_get_function_info() ->
 fun() ->
     F = fun(_, _, _) -> [{line, 1}, {source, "m.erl"}] end,
     meck:new(edts_code, [passthrough]),
     meck:expect(edts_code, get_function_info, F),
     edts_code
 end.

run_tests_common() ->
  Ref      = make_ref(),
  Listener = self(),
  Pid      = spawn(fun() -> Listener ! do_run_tests(Ref, Listener) end),
  Pid ! {start, Ref},
  assert_receive({start, Ref}),
  {Ref, Pid}.

assert_receive(Expected) ->
  ?assertEqual(Expected, receive Expected -> Expected end).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
