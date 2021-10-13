-module(edts_logger).

-export([should_log_p/1, log/4]).

-define(LOG_LEVELS, [{debug, 4},
                     {info, 3},
                     {warning, 2},
                     {error, 1}]).

should_log_p(Level) ->
    LogLevel = application:get_env(edts, log_level, info),
    proplists:get_value(LogLevel, ?LOG_LEVELS, info) >=
        proplists:get_value(Level, ?LOG_LEVELS).

log(Level, Loc, Fmt, Args) ->
    case should_log_p(Level) of
        true ->
            io:format("[~p](~p) ~s~n", [Level, Loc, io_lib:format(Fmt, Args)]);
        false ->
            ok
    end.

