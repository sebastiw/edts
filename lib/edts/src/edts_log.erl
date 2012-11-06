%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Wrapper for lager.
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
-export([debug/2,
         info/2,
         notice/2,
         warning/2,
         error/2,
         critical/2,
         alert/2,
         emergency/2,

         set_log_level/1]).

%%%_* Includes =================================================================

%%%_* Defines ==================================================================

%%%_* Types ====================================================================

%%%_* API ======================================================================
debug(    Fmt, Args) -> lager:debug(    Fmt, Args).
info(     Fmt, Args) -> lager:info(     Fmt, Args).
notice(   Fmt, Args) -> lager:notice(   Fmt, Args).
warning(  Fmt, Args) -> lager:notice(   Fmt, Args).
error(    Fmt, Args) -> lager:error(    Fmt, Args).
critical( Fmt, Args) -> lager:critical( Fmt, Args).
alert(    Fmt, Args) -> lager:alert(    Fmt, Args).
emergency(Fmt, Args) -> lager:emergency(Fmt, Args).

set_log_level(Level) -> lager:set_loglevel(lager_console_backend, Level).

%%%_* Internal functions =======================================================

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
