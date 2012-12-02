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
-include_lib("eunit/include/eunit.hrl").

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
  LoadedFiles = % Non-otp modules
    gather_beam_files(code:lib_dir(), code:all_loaded()),
  Warnings = dialyzer:run(get_options(LoadedFiles, OutPlt, Plts)),
  format_warnings(filter_warnings(Modules, Warnings)).

%%%_* Internal functions =======================================================

filter_warnings(Modules, Warnings) ->
  case Modules of
    all -> Warnings;
    _   ->
      Fun =
        fun({_, {F, _}, _}) ->
            Module = list_to_atom(filename:rootname(filename:basename(F))),
            lists:member(Module, Modules)
        end,
      lists:filter(Fun, Warnings)
  end.


get_options(LoadedFiles, OutPlt, Plts) ->
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
  [{get_warnings, true}|Opts].


gather_beam_files(OtpLibDir, LoadedModules) ->
  F = fun({_M, Loc}, Acc) when is_list(Loc) ->
          case lists:suffix(".beam", Loc) andalso % \ Loaded from file and
               not lists:prefix(OtpLibDir, Loc) of   % / not an otp library.
            true  -> [Loc|Acc];
            false -> Acc
          end;
         (_, Acc) -> Acc
      end,
  lists:foldl(F, [], LoadedModules).

format_warnings(Warnings) ->
  F = fun({_, {File, Line}, _} = Warning) ->
          {warning, File, Line, dialyzer:format_warning(Warning)}
      end,
  lists:map(F, Warnings).

%%%_* Unit tests ===============================================================

filter_warnings_test_() ->
  [?_assertEqual([foo], filter_warnings(all, [foo])),
   ?_assertEqual([], filter_warnings(all, [])),
   ?_assertEqual([], filter_warnings([],  [{error, {"src/foo.erl", 1}, bla}])),
   ?_assertEqual([], filter_warnings([bar],  [{error, {"src/foo.erl", 1}, bla}])),
   ?_assertEqual([{error, {"src/foo.erl", 1}, bla}],
                 filter_warnings([foo],  [{error, {"src/foo.erl", 1}, bla},
                                          {error, {"src/bar.erl", 1}, bla}])),
   ?_assertEqual([{error, {"src/foo.erl", 1}, bla}],
                 filter_warnings([foo, bar], [{error, {"src/foo.erl", 1}, bla}]))
  ].

get_options_test_() ->
  File1 = filename:join(code:priv_dir(edts), "out_plt1"),
  File2 = filename:join(code:priv_dir(edts), "out_plt2"),
  ok = file:write_file(File2, <<"">>),
  File3 = filename:join(code:priv_dir(edts), "out_plt3"),
  ok = file:write_file(File3, <<"">>),
  File4 = filename:join(code:priv_dir(edts), "out_plt4"),
  ok = file:write_file(File4, <<"">>),
  [{setup,
    fun() ->
        meck:unload(),
        meck:new(dialyzer),
        meck:new(dialyzer_plt),
        meck:expect(dialyzer_plt, included_files,
                    fun(F) when F =:= File2 orelse
                                F =:= File3 orelse
                                F =:= File4 -> {ok, ["one"]}
                    end),
        meck:expect(dialyzer_plt, check_plt,
                    fun(F2, [], []) when F2 =:= File2 -> ok;
                       (F3, [], []) when F3 =:= File3 -> {error, some_error};
                       (F4, [], []) when F4 =:= File4 -> changed
                    end)
    end,
    fun(_) ->
        meck:unload(),
        file:delete(File2),
        file:delete(File3),
        file:delete(File4)
    end,
    [?_assertEqual(lists:sort([{get_warnings, true},
                               {files, ["one"]},
                               {output_plt, File1},
                               {plts, ["otp_plt"]},
                               {analysis_type, plt_build}]),
                   lists:sort(get_options(["one"], File1, ["otp_plt"]))),
     ?_assertEqual(lists:sort([{get_warnings, true},
                              {files, ["one"]},
                              {init_plt, File2},
                              {analysis_type, succ_typings}]),
                   lists:sort(get_options(["one"], File2, ["otp_plt"]))),
     ?_assertThrow({error, some_error},
                   get_options(["one"], File3, ["otp_plt"])),
     ?_assertEqual(lists:sort([{get_warnings, true},
                              {files, ["one"]},
                              {init_plt, File4},
                              {output_plt, File4},
                              {analysis_type, plt_check}]),
                   lists:sort(get_options(["one"], File4, ["otp_plt"]))),
     ?_assertEqual(lists:sort([{get_warnings, true},
                              {files, ["two"]},
                              {init_plt, File2},
                              {output_plt, File2},
                              {analysis_type, plt_add}]),
                   lists:sort(get_options(["one", "two"], File2, ["otp_plt"])))
    ]}].

format_warnings_test_() ->
  [{setup,
    fun() ->
        meck:unload(),
        meck:new(dialyzer),
        meck:expect(dialyzer, format_warning,
                    fun({foo, {"file", 1337}, "warning"}) -> "warning" end)
    end,
    fun(_) ->
        meck:unload()
    end,
    [?_assertEqual([{warning, "file", 1337, "warning"}],
                   format_warnings([{foo, {"file", 1337}, "warning"}]))]}].

gather_beam_files_test_() ->
  {ok, Cwd} = file:get_cwd(),
  OtpDir = filename:join([Cwd, "otp", "lib"]),
  [?_assertEqual([], gather_beam_files(OtpDir, [{test, preloaded}])),
   ?_assertEqual([], gather_beam_files(OtpDir, [{test, OtpDir}])),
   ?_assertEqual([], gather_beam_files(OtpDir, [{test, "test"}])),
   ?_assertEqual(["test.beam"],
                 gather_beam_files(OtpDir, [{test, "test.beam"}]))].

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

