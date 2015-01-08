%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc EDTS Dialyzer integration code.
%%% @end
%%% @author Thomas Järvstrand <tjarvstrand@gmail.com>
%%% @copyright
%%% Copyright 2012-2013 Thomas Järvstrand <tjarvstrand@gmail.com>
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

-behaviour(edts_plugins).

%%%_* Includes =================================================================
-include_lib("eunit/include/eunit.hrl").

%%%_* Exports ==================================================================

%% API
-export([analyze/2, analyze/3]).

%% EDTS plugin API
-export([edts_server_services/0,
         event_formatters/0,
         project_node_modules/0,
         project_node_services/0,
         spec/2]).

%%%_* Defines ==================================================================

%%%_* Types ====================================================================

%%%_* API ======================================================================

%% EDTS Plugin API
edts_server_services()  -> [].
event_formatters()      -> [].
project_node_modules()  -> [ ?MODULE ].
project_node_services() -> [].

spec(analyze, 2) -> [ {out_plt, string}
                    , {modules, [atom]}];
spec(analyze, 3) -> [ {otp_plt, string}
                    , {out_plt, string}
                    , {modules, [atom]}].

%%------------------------------------------------------------------------------
%% @equiv analyze(undefined, OutPlt, Modules)
-spec analyze(OutPlt ::filename:filename(),
              Modules::[filename:filename()] | all) -> [term()].
%%------------------------------------------------------------------------------
analyze(OutPlt, Modules) ->
  analyze(undefined, OutPlt, Modules).


%%------------------------------------------------------------------------------
%% @doc
%% Runs dialyzer.
%%
%% If OutPlt doesn't exist, create it based on BasePlt and add all loaded non-
%% otp modules to it. If it exists, update it to ensure that it's files are the
%% same ones as the currently loaded modules. Try to use Dialyzer's default plt
%% if BasePlt is 'undefined' or the file does not exist. Then analyze the plt
%% and return warnings for all modules in Modules or for all modules if
%% Modules =:= 'all'.
%% @end
-spec analyze(BasePlt::filename:filename() | undefined,
              OutPlt ::filename:filename(),
              Modules::[filename:filename()] | all) -> [term()].
%%------------------------------------------------------------------------------
analyze(BasePlt, OutPlt, Modules) ->
  LoadedFiles = % Non-otp modules
    non_otp_beam_files(code:lib_dir(), code:all_loaded()),
  try
    ok = update_plt(BasePlt, OutPlt, LoadedFiles),
    Warnings = get_plt_warnings(OutPlt),
    format_warnings(filter_warnings(Modules, Warnings))
  catch
    throw:{dialyzer_error, Err} when is_list(Err) ->
      {error, lists:flatten(Err)};
    throw:{dialyzer_error, Err}                   ->
      {error, Err}
  end.

%%%_* Internal functions =======================================================

update_plt(BasePlt, OutPlt, Files) ->
  %% FIXME What to do if BasePlt has changed?
  case filelib:is_file(OutPlt) of
    false -> create_plt(BasePlt, OutPlt, Files);
    true  ->
      FileSet    = ordsets:from_list(Files),
      case ordsets:from_list(get_included_files(OutPlt)) of
        FileSet    -> check_plt(OutPlt, Files);
        OldFileSet ->
          Files2Add    = ordsets:to_list(ordsets:subtract(FileSet, OldFileSet)),
          Files2Remove = ordsets:to_list(ordsets:subtract(OldFileSet, FileSet)),
          case Files2Add of
            [] -> ok;
            _  -> add_to_plt(OutPlt, Files2Add)
          end,
          case Files2Remove of
            [] -> ok;
            _  -> remove_from_plt(OutPlt, Files2Remove)
          end
      end
  end,
  ok.

get_plt_warnings(Plt) ->
  Opts2 = [{files, get_included_files(Plt)},
           {plts, [Plt]},
           {analysis_type, succ_typings}],
  dialyzer:run(Opts2).


check_plt(Plt, Files) ->
  Opts = [{get_warnings, false},
          {files, Files},
          {plts, [Plt]},
          {analysis_type, plt_check}],
  dialyzer:run(Opts).


remove_from_plt(Plt, Files) ->
  Opts = [{get_warnings, false},
          {files, Files},
          {init_plt, Plt},
          {output_plt, Plt},
          {analysis_type, plt_remove}],
  dialyzer:run(Opts).


add_to_plt(Plt, Files) ->
  Opts = [{get_warnings, false},
          {files, Files},
          {init_plt, Plt},
          {output_plt, Plt},
          {analysis_type, plt_add}],
  dialyzer:run(Opts).


create_plt(BasePlt0, OutPlt, Files) ->
  BaseOpts =
    [{get_warnings, false},
     {files, Files},
     {output_plt, OutPlt}],

  BasePlt =
    case BasePlt0 of
      undefined ->
        try dialyzer_plt:get_default_plt()
        catch throw:{dialyzer_error, _} -> undefined
        end;
      _ when is_list(BasePlt0) -> BasePlt0
    end,
  VarOpts =
    case filelib:is_file(BasePlt) of
      true  -> [{plts, [BasePlt]}, {analysis_type, plt_add}];
      false -> [{analysis_type, plt_build}]
    end,

  dialyzer:run(BaseOpts ++ VarOpts).

get_included_files(Plt) ->
  {ok, Info} = dialyzer:plt_info(Plt),
  {files, Files} = lists:keyfind(files, 1, Info),
  Files.


%%------------------------------------------------------------------------------
%% @doc
%% Return a list of the beam-files of all currently loaded modules that where
%% loaded from such, compiled with debug-info and are not otp-modules.
-spec non_otp_beam_files(filename:filename(),
                         [{module(), filename:filename()}]) ->
                            [filename:filename()].
%%------------------------------------------------------------------------------
non_otp_beam_files(OtpLibDir, LoadedModules) ->
  F = fun({M, Loc}, Acc) ->
          case non_otp_beam_file_p(OtpLibDir, M, Loc) of
            true  -> [edts_util:shorten_path(Loc)|Acc];
            false -> Acc
          end
      end,
  lists:foldl(F, [], LoadedModules).

non_otp_beam_file_p(_OtpLibDir, _M, Loc) when is_atom(Loc) -> false;
non_otp_beam_file_p( OtpLibDir,  M, Loc)                   ->
  Opts = proplists:get_value(options, M:module_info(compile), []),
  lists:member(debug_info, Opts) andalso
    filename:extension(Loc) =:= ".beam" andalso
    not lists:prefix(OtpLibDir, Loc).


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


format_warnings(Warnings) ->
  F = fun({_, {File, Line}, _} = Warning) ->
          {warning, File, Line, dialyzer:format_warning(Warning)}
      end,
  lists:map(F, Warnings).

%%%_* Unit tests ===============================================================

update_plt_test_() ->
  {ok, Cwd} = file:get_cwd(),
  File = filename:join(Cwd, "foo1"),
  BadFile = filename:join(Cwd, "foo2"),
  [{setup,
    fun() ->
        meck:unload(),
        meck:new(dialyzer_plt, [passthrough]),
        file:write_file(File, "foo")
    end,
    fun(_) ->
        file:delete(File),
        meck:unload()
    end,
    [{foreach,
      fun() ->
          catch meck:unload(dialyzer),
          meck:new(dialyzer),
          meck:expect(dialyzer, run, fun(_) -> ok end)
      end,
      fun(_) -> ok end,
      [%% Base plt doesn't exist
       ?_assertEqual({analysis_type, plt_build},
                     begin
                       ok = update_plt("", BadFile, ["file1"]),
                       Hist = meck:history(dialyzer),
                       [{_, {dialyzer, run, [Arg]}, ok}] = Hist,
                       lists:keyfind(analysis_type, 1, Arg)
                     end),
       %% Base plt is 'undefined' and default_plt does not exist
       {setup,
        fun() ->
            meck:expect(dialyzer_plt, get_default_plt, fun() -> "foo.plt" end),
            ok = update_plt(undefined, BadFile, ["file1"])
        end,
        fun(_) -> ok end,
        [?_assertEqual({analysis_type, plt_build},
                       begin
                         Hist = meck:history(dialyzer),
                         [{_, {dialyzer, run, [Arg]}, ok}] = Hist,
                         lists:keyfind(analysis_type, 1, Arg)
                       end)]},
       %% Base plt is 'undefined' and default_plt throws exception
       {setup,
        fun() ->
            meck:expect(dialyzer_plt,
                        get_default_plt,
                        fun() ->
                            meck:exception(throw, {dialyzer_error, foo})
                        end),
                       ok = update_plt(undefined, BadFile, ["file1"])
        end,
        fun(_) -> ok end,
        [?_assertEqual({analysis_type, plt_build},
                     begin
                       Hist = meck:history(dialyzer),
                       [{_, {dialyzer, run, [Arg]}, ok}] = Hist,
                       lists:keyfind(analysis_type, 1, Arg)
                     end)]},
       %% Base plt is 'undefined' and default_plt exists
       {setup,
        fun() ->
            PltFile = "foo.plt",
            meck:expect(dialyzer_plt, get_default_plt, fun() -> PltFile end),
            meck:new(filelib, [passthrough, unstick]),
            meck:expect(filelib, is_file, fun(F) when F =:= PltFile -> true;
                                             (A) -> meck:passthrough([A])
                                          end),
            ok = update_plt(undefined, BadFile, ["file1"])
        end,
        fun(_) -> ok end,
        [?_assertEqual({analysis_type, plt_add},
                       begin
                         Hist = meck:history(dialyzer),
                         [{_, {dialyzer, run, [Arg]}, ok}] = Hist,
                         lists:keyfind(analysis_type, 1, Arg)
                       end)]},

       {setup,
        fun() ->
            meck:expect(dialyzer, plt_info, 1, {ok, [{files, ["file1"]}]})
        end,
        fun(_) -> ok end,
        ?_assertEqual({analysis_type, plt_check},
                       begin
                         ok = update_plt("", File, ["file1"]),
                         Hist = meck_history(dialyzer, run, 1),
                         [{[Arg], ok}] = Hist,
                         lists:keyfind(analysis_type, 1, Arg)
                       end)},
       {setup,
        fun() ->
            meck:expect(dialyzer, plt_info, 1, {ok, [{files, ["file1"]}]})
        end,
        fun(_) -> ok end,
        ?_assertEqual({analysis_type, plt_add},
                      begin
                        ok = update_plt("", File, ["file1", "file2"]),
                        Hist = meck_history(dialyzer, run, 1),
                        [{[Arg], ok}] = Hist,
                        lists:keyfind(analysis_type, 1, Arg)
                      end)},
       {setup,
        fun() ->
            Ret = {ok, [{files, ["file1", "file2"]}]},
            meck:expect(dialyzer, plt_info, 1, Ret)
        end,
        fun(_) -> ok end,
        ?_assertEqual({analysis_type, plt_remove},
                      begin
                        ok = update_plt("", File, ["file1"]),
                        Hist = meck_history(dialyzer, run, 1),
                        [{[Arg], ok}] = Hist,
                        lists:keyfind(analysis_type, 1, Arg)
                      end)},
       {setup,
        fun() ->
            Ret = {ok, [{files, ["file1", "file2"]}]},
            meck:expect(dialyzer, plt_info, 1, Ret)
        end,
        fun(_) -> ok end,
        ?_assertEqual({{analysis_type, plt_add},{analysis_type, plt_remove}},
                      begin
                        ok = update_plt("", File, ["file1", "file3"]),
                        Hist = meck_history(dialyzer, run, 1),
                        [{[Arg4], ok}, {[Arg5], ok}] = Hist,
                        Type1 = lists:keyfind(analysis_type, 1, Arg4),
                        Type2 = lists:keyfind(analysis_type, 1, Arg5),
                        {Type1, Type2}
                      end)}
       ]}]}].

non_otp_beam_files_test_() ->
  Mod = edts_dialyzer_test_module,
  {ok, Cwd} = file:get_cwd(),
  OtpDir = filename:join([Cwd, "otp", "lib"]),
  c:l(Mod),
  [?_assertEqual([], non_otp_beam_files(OtpDir, [{Mod, preloaded}])),
   ?_assertEqual([], non_otp_beam_files(OtpDir, [{Mod, OtpDir}])),
   ?_assertEqual([], non_otp_beam_files(OtpDir, [{Mod, "test"}])),
   ?_assertEqual(["test.beam"],
                 non_otp_beam_files(OtpDir, [{Mod, "test.beam"}]))
  ].

filter_warnings_test_() ->
  [?_assertEqual([foo],
                 filter_warnings(all, [foo])),
   ?_assertEqual([],
                 filter_warnings(all, [])),
   ?_assertEqual([],
                 filter_warnings([],    [{error, {"src/foo.erl", 1}, bla}])),
   ?_assertEqual([],
                 filter_warnings([bar], [{error, {"src/foo.erl", 1}, bla}])),
   ?_assertEqual([{error, {"src/foo.erl", 1}, bla}],
                 filter_warnings([foo],  [{error, {"src/foo.erl", 1}, bla},
                                          {error, {"src/bar.erl", 1}, bla}])),
   ?_assertEqual([{error, {"src/foo.erl", 1}, bla}],
                 filter_warnings([foo, bar], [{error, {"src/foo.erl", 1}, bla}]))
  ].

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

%%%_* Unit test helpers =======================================================-

meck_history(M0, F0, A0) ->
  [{Args, Ret} || {_, {M, F, Args}, Ret} <- meck:history(M0),
                                            M =:= M0,
                                            F =:= F0,
                                            length(Args) =:= A0].

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

