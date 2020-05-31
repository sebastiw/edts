%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc get_free_vars command
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

-module(edts_cmd_get_mfas).

-behaviour(edts_cmd).

%%%_* Exports ==================================================================
%% API

-export([spec/0, execute/1]).

%%%_* Includes =================================================================
%%%_* Defines ==================================================================
%%%_* Types ====================================================================
%%%_* API ======================================================================

spec() -> [expressions].

execute(Ctx) ->
  MFAStrings = orddict:fetch(expressions, Ctx),
  {
    ok,
    lists:filtermap(
      fun
        (MFAString) ->
          case edts_code:string_to_mfa(MFAString) of
            {ok, MFA} -> {true, MFA};
            {error, _} -> false
          end
      end,
      MFAStrings
    )
  }.

%%%_* Internal functions =======================================================
