-module(edts_dbg).

-export([ trace_function/2 ]).

trace_function(Trace, Opts0) ->
  TraceLog = spawn_link(fun do_trace/0),
  Opts = Opts0 ++ [{print_pid, TraceLog}],
  redbug:start(Trace, Opts).

do_trace() ->
  receive
    Msg -> io:format("~p~n", [Msg]),
           do_trace()
  end.
