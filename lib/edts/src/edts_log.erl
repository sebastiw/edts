%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Logging intrastructure
%%% @end
%%% @author Thomas Järvstrand <tjarvstrand@gmail.com>
%%% @copyright
%%% Copyright 2012 Thomas Järvstrand <tjarvstrand@gmail.com>
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

-module(edts_log).

%%%_* Exports ==================================================================
%% API

-export(
  [
    debug/1,
    debug/2,
    info/1,
    info/2,
    warning/1,
    warning/2,
    error/1,
    error/2,
    log/3,
    get_log_level/0,
    set_log_level/1
  ]
).

-compile({no_auto_import, [error/2]}).

%%%_* Includes =================================================================
%%%_* Defines ==================================================================

-define(log_levels, [{debug, 4}, {info, 3}, {warning, 2}, {error, 1}]).

%%%_* Types ====================================================================
%%%_* API ======================================================================

debug(Fmt) -> debug(Fmt, []).

debug(Fmt, Args) -> log(debug, Fmt, Args).

info(Fmt) -> info(Fmt, []).

info(Fmt, Args) -> log(info, Fmt, Args).

warning(Fmt) -> warning(Fmt, []).

warning(Fmt, Args) -> log(warning, Fmt, Args).

error(Fmt) -> error(Fmt, []).

error(Fmt, Args) -> log(error, Fmt, Args).

log(Level, Fmt, Args) ->
  case should_log_p(Level) of
    true -> io:format("[~p] ~s~n", [Level, io_lib:format(Fmt, Args)]);
    false -> ok
  end.


get_log_level() ->
  case application:get_env(edts, log_level) of
    {ok, Lvl} -> Lvl;
    undefined -> info
  end.


set_log_level(Level) -> application:set_env(edts, log_level, Level).

%%%_* Internal functions =======================================================

should_log_p(Level) ->
  proplists:get_value(get_log_level(), ?log_levels)
  >=
  proplists:get_value(Level, ?log_levels).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
