-module(edts_dbg).

-export([ start/0
        , stop/0
        , do_trace/0
        , trace_function/2 ]).

start() ->
  ok.

stop() ->
  ok.

trace_function(Trace, Opts) ->
  redbug:start(Trace, Opts).

do_trace() ->
  receive
    {trace_consumer, TC} = Msg-> io:format("~p~n", [Msg]),
                                 erlang:monitor(process, TC),
                                 trace_loop()
  end.

trace_loop() ->
  receive
    {'DOWN', _, _, _, R} -> io:format("Trace exiting: ~p~n", [R]);
    Msg                  -> io:format("~p~n", [redbug:printp(Msg)]),
                            trace_loop()
  end.
