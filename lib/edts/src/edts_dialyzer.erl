%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc EDTS Dialyzer integration code.
%%% @end
%%% @author Thomas Järvstrand <tjarvstrand@gmail.com>
%%% @copyright
%%% Copyright 2012 Thomas Järvstrand <tjarvstrand@gmail.com>
%%%
%%% This file is part of EDTS.
%%%
%%% EDTS is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% EDTS is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with EDTS. If not, see <http://www.gnu.org/licenses/>.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration =======================================================
-module(edts_dialyzer).

%%%_* Exports ==================================================================

%% API
-export([run/2]).

%%%_* Includes =================================================================

%%%_* Defines ==================================================================

%%%_* Types ====================================================================

%%%_* API ======================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Runs dialyzer.
%%
%% Add all currently loaded non-otp modules to OutPlt, creating it if it doesn't
%% exist. Then analyze OtpPlt and OutPlt together.
%% @end
-spec run(filename:filename() | undefined, filename:filename()) -> ok.
%%------------------------------------------------------------------------------
run(OtpPlt, OutPlt) ->
  Files = gather_beam_files(),
  dialyzer:run([{files, Files},
                {output_plt, OutPlt},
                {analysis_type, plt_add}]),

  Plts = case OtpPlt of
           undefined -> [OutPlt];
           _         -> [OtpPlt, OutPlt]
         end,
  format_warnings(
    dialyzer:run([{files, Files},
                {plts, Plts},
                {analysis_type, succ_typings}])).

%%%_* Internal functions =======================================================

gather_beam_files() ->
  OtpDir = code:lib_dir(),
  F = fun({_M, Loc}, Acc) when is_list(Loc) ->
          case lists:suffix(".beam", Loc) andalso % \ Loaded from file and
               not lists:prefix(OtpDir, Loc) of   % / not an otp library.
            true  -> [Loc|Acc];
            false -> Acc
          end;
         (_, Acc) -> Acc
      end,
  lists:foldl(F, [], code:all_loaded()).

format_warnings(Warnings) ->
  F = fun({_, {File, Line}, _} = Warning) ->
          {warning, File, Line, dialyzer:format_warning(Warning)}
      end,
  lists:map(F, Warnings).


%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

