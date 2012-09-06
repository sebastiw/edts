-module(edts_dbg).

-export([ start/0
        , stop/0
        , interpret_modules/1
        , receive_traces/0
        , toggle_breakpoint/2
        , trace_function/2
        , wait_for_debugger/1 ]).

-define(DEBUGGER, edts_debugger).

start() ->
  int:auto_attach(break, {?MODULE, start_debugging, []}).

stop() ->
  ok.

interpret_modules(Modules) ->
  {ok, [int:i(M) || M <- Modules, int:interpretable(M)]}.

toggle_breakpoint(Module, Line) ->
  case lists:keymember({Module, Line}, 1, int:all_breaks()) of
    true  -> int:delete_break(Module, Line),
             {ok, unset, {Module, Line}};
    false -> int:break(Module, Line),
             {ok, set, {Module, Line}}
  end.

trace_function(Trace, Opts) ->
  redbug:start(Trace, Opts).

receive_traces() ->
  receive
    {trace_consumer, TC} -> erlang:monitor(process, TC),
                            trace_loop()
  end.

trace_loop() ->
  receive
    {'DOWN', _, _, _, R} -> io:format("Trace exiting: ~p~n", [R]);
    Msg                  -> io:format("~p~n", [redbug:printp(Msg)]),
                            trace_loop()
  end.

start_debugging([Pid | _Args])  ->
  erlang:register(?DEBUGGER),
  do_debug(Pid).

do_debug(Pid) ->
  receive
    _ -> do_debug(Pid)
  end.

wait_for_debugger(0) ->
  io:format("Debugger not up. Giving up...~n"),
  {error, attempts_exceeded};
wait_for_debugger(Attempts) ->
  case lists:member(?DEBUGGER, erlang:registered()) of
    true ->
      io:format("Debugger up!~n"),
      ok;
    _    ->
      io:format("Debugger not up yet... Trying ~p more time(s)~n", [Attempts]),
      timer:sleep(1000),
      wait_for_debugger(Attempts - 1)
  end.
