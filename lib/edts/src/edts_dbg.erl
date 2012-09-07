-module(edts_dbg).

-export([ start/0
        , stop/0
        , interpret_modules/1
        , receive_traces/0
        , start_debugging/1
        , toggle_breakpoint/2
        , trace_function/2
        , wait_for_debugger/2 ]).

-define(DEBUGGER, edts_debugger).

start() ->
  int:auto_attach([break], {?MODULE, start_debugging, []}).

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

%% Hey, I just wrote you, and this is crazy, so turn into a gen_server, maybe?
start_debugging(Pid)  ->
  erlang:register(?DEBUGGER, self()),
  io:format("[DEBUGGER] registered..."),
  do_debug(Pid).

do_debug(Pid) ->
  io:format(" starting...~n"),
  receive
    continue -> io:format("[DEBUGGER] Continuing!~n"),
                int:continue(Pid);
    _        -> do_debug(Pid)
  end.

wait_for_debugger(_, 0) ->
  io:format("Debugger not up. Giving up...~n"),
  {error, attempts_exceeded};
wait_for_debugger(Node, Attempts) ->
  RemoteRegistered = rpc:call(Node, erlang, registered, []),
  case lists:member(?DEBUGGER, RemoteRegistered) of
    true ->
      io:format("Debugger up!~n"),
      io:format("Ordering debugger to continue...~n"),
      {?DEBUGGER, Node} ! continue,
      ok;
    _    ->
      io:format("Debugger not up yet... Trying ~p more time(s)~n", [Attempts]),
      timer:sleep(1000),
      wait_for_debugger(Node, Attempts - 1)
  end.
