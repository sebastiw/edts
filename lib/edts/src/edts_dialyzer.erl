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
-export([run/3]).

%%%_* Includes =================================================================

%%%_* Defines ==================================================================

%%%_* Types ====================================================================

%%%_* API ======================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Runs dialyzer.
%%
%% Add Files to OutPlt, creating it if it doesn't exist. Then analyze OtpPlt and
%% OutPlt together.
%% @end
-spec run(Plts::filename:filename() | undefined,
          OutPlt::filename:filename(),
          Files::[filename:filename()] | all) -> ok.
%%------------------------------------------------------------------------------
run(Plts, OutPlt, Modules) ->
  LoadedFiles = gather_beam_files(), % Non-otp modules
  Warnings = get_warnings(LoadedFiles, OutPlt, Plts),
  Filtered =
    case Modules of
      all -> Warnings;
      _   ->
        Fun =
          fun({_, {F, _}, _}) ->
              Module = list_to_atom(filename:rootname(filename:basename(F))),
              lists:member(Module, Modules)
          end,
        lists:filter(Fun, Warnings)
    end,
  format_warnings(Filtered).

%%%_* Internal functions =======================================================

get_warnings(LoadedFiles, OutPlt, Plts) ->
  Opts =
    case filelib:is_file(OutPlt) of
      false -> %% Build plt from scratch, dialyzer will analyze in the process
        [{files, LoadedFiles},
         {output_plt, OutPlt},
         {plts, Plts},
         {analysis_type, plt_build}];
      true ->
        {ok, OldFiles} = dialyzer_plt:included_files(OutPlt),
        case LoadedFiles -- OldFiles of
          [] ->
            %% No new files to add, may need to force an analysis
            case dialyzer_plt:check_plt(OutPlt, [], []) of
              ok -> % No changes to any files, exlplicit call needed
                [{files, LoadedFiles},
                 {init_plt, OutPlt},
                 {analysis_type, succ_typings}];
              {error, _}  = Err -> throw(Err); %% Something went wrong
              _ -> % Some file changed. dialyzer will update plt and analyze
                [{files, LoadedFiles},
                 {init_plt, OutPlt},
                 {output_plt, OutPlt},
                 {analysis_type, plt_check}]
            end;
          FilesToAdd ->
            %% Add new files. This will cause dialyzer to run analysis
            [{files, FilesToAdd},
             {init_plt, OutPlt},
             {output_plt, OutPlt},
             {analysis_type, plt_add}]
        end
    end,
  dialyzer:run([{get_warnings, true}|Opts]).


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

