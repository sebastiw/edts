%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Provides support for running eunit tests from EDTS
%%% @end
%%% @author Håkan Nilsson <haakan@gmail.com>
%%% @copyright
%%% Copyright 2012 Håkan Nilsson <haakan@gmail.com>
%%%           2013 Thomas Järvstrand <tjarvstrand@gmail.com>
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
-type result()  :: ok() | error().
-type ok()      :: {ok, {summary(), [test()]}}.
-type error()   :: {error, term()}.
-type summary() :: orddict:orddict().
-type test()    :: {mfa(), [info()]}.
-type reason()  :: atom().

-export_type([info/0,
              result/0,
              ok/0,
              error/0,
              summary/0,
              test/0,
              reason/0
             ]).

%%%_* API ======================================================================

%%------------------------------------------------------------------------------
%% @doc Run eunit tests on Module and return result as "issues".
-spec run_tests(module()) -> {ok, [edts_code:issue()]}
                           | {error, term()}.
%%------------------------------------------------------------------------------
run_tests(Module) ->
  try try_run_tests(Module)
  catch _:E -> {error, E}
  end.

try_run_tests(Module) ->
  case do_run_tests(Module) of
    {ok, {Summary, Results}} ->
      debug("run tests returned ok: ~p", [Summary]),
      {ok, Source} = edts_code:get_module_source(Module),
      {ok, format_results(Source, Results)};
    {error, Reason} = Error ->
      debug("Error in eunit test result in ~p: ~p", [Module, Reason]),
      Error
  end.


%%%_* Internal functions =======================================================

-spec do_run_tests(module()) -> result().
do_run_tests(Module) ->
  debug("running eunit tests in: ~p", [Module]),
  Listener = edts_eunit_listener:start([{parent, self()}]),
  Tests = filter_module_tests(Module, eunit_data:get_module_tests(Module)),
  case eunit_server:start_test(eunit_server, Listener, Tests, []) of
    {ok, Ref}    -> do_run_tests(Ref, Listener, 20000);
    {error, Err} -> {error, Err}
  end.

filter_module_tests(Module, Tests) ->
  Fun = fun({_Type, TestModule, _Fun} = Test, Acc) when TestModule =:= Module ->
            [Test|Acc];
           ({WeirdModuleString, ModuleTest}, Acc) when is_atom(ModuleTest) ->
            [{WeirdModuleString, ModuleTest}|Acc];
           ({WeirdModuleString, ModuleTests}, Acc) ->
            case filter_module_tests(Module, ModuleTests) of
              [] -> Acc;
              Tests -> [{WeirdModuleString, Tests}|Acc]
            end;
           (_, Acc) -> Acc
        end,
  lists:reverse(lists:foldl(Fun, [], Tests)).

-spec do_run_tests(reference(), pid(), non_neg_integer()) -> result().
do_run_tests(Ref, Listener, Timeout) ->
  debug("waiting for start..."),
  receive
    {start, Ref} ->
      Listener ! {start, Ref}
  end,
  debug("waiting for result..."),
  receive
    {result, Ref, Result} -> {ok, Result};
    {error, Err}          -> {error, Err}
  after
    Timeout -> {error, timeout}
  end.

format_results(Source, Results) ->
  lists:map(fun(Result) -> format_successful(Source, Result) end,
            orddict:fetch(successful, Results)) ++
    lists:map(fun(Result) -> format_failed(Source, Result) end,
              orddict:fetch(failed, Results)) ++
    lists:map(fun(Result) -> format_cancelled(Source, Result) end,
              orddict:fetch(cancelled, Results)).


format_successful(Source, Result) ->
  {'passed-test', Source, get_line(Result), "no asserts failed"}.

format_failed(Source, Result) ->
  {error, Err} = proplists:get_value(status, Result),
  {'failed-test', Source, get_line(Result), format_error(Err)}.

format_cancelled(Source, Result) ->
  Reason = to_str(proplists:get_value(reason, Result)),
  {'cancelled-test', Source, get_line(Result), Reason}.

format_error({error, {Reason, Info} = Err, _Stack}) ->
  case reason_to_props(Reason) of
    {ok, {ExpectProp, ValueProp}} ->
      Expected = proplists:get_value(ExpectProp, Info),
      Value = get_first_prop(ValueProp, Info),
      io_lib:format("~p\n"
                    "expected: ~s\n"
                    "value:    ~s",
                    [Reason, to_str(Expected), to_str(Value)]);
    {error, not_found} ->
      io_lib:format("~p", [Err])
  end;
format_error({Err,Reason,_Stack}) ->
  io_lib:format("~p", [{Err,Reason}]);
format_error(Err) ->
  io_lib:format("~p", [Err]).

get_first_prop([Key|Keys], Proplist) ->
  case proplists:get_value(Key, Proplist) of
    undefined -> get_first_prop(Keys, Proplist);
    Value     -> Value
  end.


get_line(Result) ->
  case proplists:get_value(line, Result) of
    Line when is_integer(Line) andalso
              Line > 0 ->
      Line;
    _ ->
      try get_error_line(Result)
      catch
        _:_ ->
          case proplists:get_value(source, Result) of
            {M, F, A} ->
              proplists:get_value(line, edts_code:get_function_info(M, F, A));
            _         ->
              1
          end
      end
  end.

get_error_line(Result) ->
  {error, {error, Error, Loc}} = proplists:get_value(status, Result),
  case Error of
    {Reason, [{_, _}|_] = Details} when is_atom(Reason) ->
      proplists:get_value(line, Details);
    _ -> get_error_line_from_loc(Loc)
  end.

%% No clause for unexpected arguments because we wouldn't know what to do with
%% them anyway. Crash and let get_line/1 deal with it.
get_error_line_from_loc([{_M, _F, _A, Src}]) ->
  Line = proplists:get_value(line, Src),
  true = is_integer(Line),
  Line.

reason_to_props(Reason) ->
  Mapping =
    [{assertException_failed, {pattern, [unexpected_success,
                                         unexpected_exception]}},
     {assertNotException_failed, {pattern, [unexpected_exception]}},
     {assertCmdOutput_failed, {expected_output, [output]}},
     {assertCmd_failed, {expected_status, [status]}},
     {assertEqual_failed, {expected, [value]}},
     {assertMatch_failed, {pattern, [value]}},
     {assertNotEqual_failed, {expected, [expected]}},
     {assertNotMatch_failed, {pattern, [value]}},
     {assertion_failed, {expected, [value]}},
     {command_failed, {expected_status, [status]}}],
  case lists:keyfind(Reason, 1, Mapping) of
    {Reason, Props} -> {ok, Props};
    false           -> {error, not_found}
  end.


-spec to_str(term()) -> string().
to_str(Term) ->
  %% Remave line breaks
  [C || C <- lists:flatten(io_lib:format("~p", [Term])), C =/= $\n].

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

do_run_tests_timeout_test() ->
  Ref = make_ref(),
  self() ! {start, Ref},
  ?assertEqual({error, timeout}, do_run_tests(Ref, self(), 0)).

format_results_test_() ->
  ErrorInfo = [{expected, foo},
               {value, bar}],
  PropList =
    [{successful, [ [{line, 1}]]},
     {failed,
      [[{line, 1},
        {status, {error, {error, {assertion_failed, ErrorInfo}, []}}}]]},
     {cancelled,  [ [{line, 1}] ]}],
  [ ?_assertMatch([{'passed-test', source, 1,  _},
                   {'failed-test', source, 1, _},
                   {'cancelled-test', source, 1, _}],
                  format_results(source, orddict:from_list(PropList)))
  ].

to_str_test_() ->
  [?_assertEqual("foo",         lists:flatten(to_str(foo))),
   ?_assertEqual("\"foo\"",     lists:flatten(to_str("foo"))),
   ?_assertEqual("[{foo,123}]", lists:flatten(to_str([{foo,123}])))
  ].

reason_to_props_test_() ->
  [?_assertEqual({ok, {expected, [value]}}, reason_to_props(assertion_failed)),
   ?_assertEqual({error, not_found}, reason_to_props(foooo))
  ].

%%%_* Test helpers -------------------------------------------------------------

run_tests_common() ->
  Ref      = make_ref(),
  Listener = self(),
  Pid      = spawn(fun() -> Listener ! do_run_tests(Ref, Listener, 1) end),
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
