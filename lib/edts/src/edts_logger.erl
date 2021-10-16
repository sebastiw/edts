-module(edts_logger).

-export([should_log_p/1, log/4, set/1]).

-include("logger.hrl").

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
            io:format("[~p]~s:~n~s~n", [Level, location(Loc), io_lib:format(Fmt, Args)]);
        false ->
            ok
    end.

set(debug) ->
    application:set_env(edts, log_level, debug);
set(info) ->
    application:set_env(edts, log_level, info);
set(warning) ->
    application:set_env(edts, log_level, warning);
set(error) ->
    application:set_env(edts, log_level, error);
set(A) ->
    ?LOG_WARNING("Unknown log_level ~p: specify debug | info | warning | error", [A]).

location(#{file := File, mfa := {M, F, A}, line := Line}) ->
    [atom_to_list(M), $:, atom_to_list(F), $/, integer_to_list(A),
     $(, File, $:, integer_to_list(Line), $)].
