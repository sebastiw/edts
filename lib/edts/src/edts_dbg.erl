-module(edts_dbg).

-export([ receive_traces/0
        , trace_function/2 ]).

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
