-module(edts_dbg).

-export([ start/0
        , stop/0
        , interpret_modules/1
        , receive_traces/0
        , set_breakpoint/2
        , trace_function/2 ]).

start() ->
  ok.

stop() ->
  ok.

interpret_modules(Modules) ->
  int:i(Modules).

set_breakpoint(Module, Line) ->
  int:break(Module, Line).

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
