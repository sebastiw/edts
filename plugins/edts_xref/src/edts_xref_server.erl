%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
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
-module(edts_xref_server).

%%%_* Exports ==================================================================

%% API
-export([allowed_checks/0,
         check_modules/2,
         get_state/0,
         start/0,
         stop/0,
         started_p/0,
         update/0,
         update_modules/1,
         who_calls/3]).


%% Internal exports.
%% -export([save_xref_state/0]).

%% Internal exports.
-export([init/1]).


-export_type([xref_check/0]).

-type xref_check() :: xref:analysis().

%%%_* Includes =================================================================
-include_lib("eunit/include/eunit.hrl").

%%%_* Defines ==================================================================
-define(SERVER, ?MODULE).

%%%_* Types ====================================================================

%%%_* API ======================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Return a list of all implemented xref-checks.
%% @end
-spec allowed_checks() -> [xref_check()].
%%------------------------------------------------------------------------------
allowed_checks() -> [undefined_function_calls, unused_exports].


%%------------------------------------------------------------------------------
%% @doc
%% Starts the edts xref-server on the local node.
%% @end
-spec start() -> {ok, pid()} | {error, already_started}.
%%------------------------------------------------------------------------------
start() ->
  File = xref_file(),
  try
    case read_file(File) of
      {ok, BinState}      ->
        error_logger:info_msg("Found previous state to start from in ~p.",
                              [File]),
        State = binary_to_term(BinState),
        do_start(State);
      {error, enoent}     ->
        error_logger:info_msg("Found no previous state to start from."),
        do_start();
      {error, _} = Error  ->
        error_logger:error_msg("Reading ~p failed with: ~p", [File, Error]),
        do_start()
    end
  catch
    C:E ->
      error_logger:error_msg("Starting xref from ~p failed with: ~p:~p~n~n"
                             "Starting with clean state instead.",
                             [File, C, E]),
      do_start()
  end,
  ok = update().

do_start() ->
  do_start_with(fun() -> xref:start(?MODULE) end).

do_start(State) ->
  do_start_with(fun() -> proc_lib:start(?MODULE, init, [State]) end).

do_start_with(StartF) ->
  case started_p() of
    true  -> ok;
    false ->
      {ok, _Pid} = StartF(),
      ok = xref:set_default(?SERVER, [{verbose,false}, {warnings,false}]),
      wait_until_started()
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Returns true if the edts xref-server is running, false otherwise
%% @end
-spec started_p() -> boolean().
%%------------------------------------------------------------------------------
started_p() -> whereis(?SERVER) =/= undefined.


%%------------------------------------------------------------------------------
%% @doc
%% Stops the edts xref-server.
%% @end
-spec stop() -> ok.
%%------------------------------------------------------------------------------
stop() ->
  case whereis(?SERVER) of
    undefined -> {error, not_started};
    _Pid      ->
      Ref = erlang:monitor(process, ?SERVER),
      xref:stop(?SERVER),
      receive {'DOWN', Ref, _, _, _} -> ok
      after 1000 -> {error, timeout}
      end
  end.


%%------------------------------------------------------------------------------
%% @doc
%% Returns the internal state of the edts xref-server.
%% @end
-spec get_state() -> term().
%%------------------------------------------------------------------------------
get_state() ->
  {status, _, _, [_, _, _, _, Misc]} = sys:get_status(?SERVER),
  proplists:get_value("State", lists:append([D || {data, D} <- Misc])).


%%------------------------------------------------------------------------------
%% @doc
%% Do an xref-analysis of Module, applying Checks
%% @end
-spec check_modules([Modules::module()], Checks::[xref:analysis()]) ->
                       {ok, [edts_code:issue()]}.
%%------------------------------------------------------------------------------
check_modules(Modules0, Checks) ->
  MaybeReloadFun =
    fun(Module) ->
        %% We can only do xref checks on compiled, successfully loaded modules
        %% atm (We don't have the location of the source-file at this point).
        case edts_code:ensure_module_loaded(false, Module) of
          Res when is_boolean(Res) -> true;
          {error, _}               -> false
        end
    end,
  Modules = lists:filter(MaybeReloadFun, Modules0),
  update(),
  update_modules(Modules),
  Files = [proplists:get_value(source, M:module_info(compile)) || M <- Modules],
  Fun  = fun(Check) -> do_check_modules(Modules, Files, Check) end,
  lists:append(lists:map(Fun, Checks)).

do_check_modules(Modules, Files, undefined_function_calls) ->
  Res = get_undefined_function_calls(Modules),
  ModuleFiles = lists:zip(Modules, Files),
  FmtFun = fun({{{Mod, _, _}, {CM, CF, CA}}, Lines})->
               case lists:keyfind(Mod, 1, ModuleFiles) of
                 false       -> [];
                 {Mod, File} ->
                   Desc = io_lib:format("Call to undefined function ~p:~p/~p",
                                        [CM, CF, CA]),
                   [{error, File, Line, lists:flatten(Desc)} || Line <- Lines]
               end
           end,
  lists:flatmap(FmtFun, Res);
do_check_modules(Modules, Files, unused_exports) ->
  %% This will not work properly in most cases since edts does the optimization
  %% of only keeping explicitly referenced modules in the xref-server state. Ie.
  %% when working with module foo, only foo and the modules that foo calls will
  %% be added to the xref server, there is no analysis to find the modules that
  %% call foo.
  Res = get_unused_exports(Modules),
  ModuleFiles = lists:zip(Modules, Files),
  FmtFun = fun({{M, F, A}, Line}) ->
               case ignored_p(M, F, A) of
                 true -> [];
                 false ->
                   {M, File} = lists:keyfind(M, 1, ModuleFiles),
                   Desc = io_lib:format("Unused export ~p:~p/~p",
                                        [M, F, A]),
                   [{error, File, Line, lists:flatten(Desc)}]
               end
           end,
  lists:flatmap(FmtFun, Res).

ignored_p(M, F, A) ->
  ignored_test_fun_p(M, F, A) orelse
    lists:member({M, F, A}, get_xref_ignores(M)).


%%------------------------------------------------------------------------------
%% @doc
%% Returns alist with all functions that call M:F/A on the local node.
%% @end
-spec who_calls(module(), atom(), non_neg_integer()) ->
                   [{module(), atom(), non_neg_integer()}].
%%------------------------------------------------------------------------------
who_calls(M, F, A) ->
  Str = lists:flatten(io_lib:format("(Lin) (E || ~p)", [{M, F, A}])),
  {ok, Calls} = xref:q(?SERVER, Str),
  [{Caller, Lines} || {{Caller, _Callee}, Lines} <- Calls].

%%------------------------------------------------------------------------------
%% @doc
%% Update the internal state of the xref server.
%% @end
-spec update() -> {ok, [module()]}.
%%------------------------------------------------------------------------------
update() ->
  {ErlLibDirs, AppDirs} = edts_util:lib_and_app_dirs(),
  update_paths(ErlLibDirs, AppDirs),
  xref:update(?SERVER),
  save_state().

update_modules(Modules) ->
  Added  = ordsets:from_list([M || {M, _Props} <- xref:info(?SERVER, modules)]),
  ToAdd0 = ordsets:to_list(ordsets:subtract(ordsets:from_list(Modules), Added)),
  ok     = lists:foreach(fun try_add_module/1, ToAdd0),

  Undef     = get_undefined_function_calls(Modules),
  UndefMods = ordsets:from_list([M || {{{_, _, _}, {M, _, _}}, _} <- Undef]),
  ToAdd1    = ordsets:subtract(UndefMods, ordsets:union(Added, ToAdd0)),
  ok        = lists:foreach(fun try_add_module/1, ToAdd1),
  xref:update(?SERVER).


%%%_* INTERNAL functions =======================================================

init(State) ->
  erlang:register(?SERVER, self()),
  proc_lib:init_ack({ok, self()}),
  gen_server:enter_loop(xref, [], State).


update_paths(LibDirs, AppDirs) ->
  ok = xref:set_library_path(?SERVER, LibDirs),
  ModsToLoad =
    dict:from_list(
      lists:flatmap(
        fun(Dir) ->
            Beams = filelib:wildcard(filename:join(Dir, "*.beam")),
            [{list_to_atom(filename:basename(B, ".beam")), B} || B <- Beams]
        end, AppDirs)),
  ModsLoaded = dict:from_list(xref:info(?SERVER, modules)),
  %% Add/update new modules
  LoadF =
    fun(Mod, Beam) ->
        case dict:find(Mod, ModsLoaded) of
          error -> % New module
            try_add_module(Mod, Beam);
          {ok, Info} ->
            Dir = filename:dirname(Beam),
            case proplists:get_value(directory, Info) of
              Dir     -> ok; % Same as old module
              _OldDir -> xref:replace_module(?SERVER, Mod, Beam) % Mod has moved
            end
        end
    end,
  dict:map(LoadF, ModsToLoad),

  %% Remove delete modules
  UnloadF =
    fun(Mod, _Info) ->
        case dict:is_key(Mod, ModsToLoad) of
          true  -> ok;
          false -> xref:remove_module(?SERVER, Mod)
        end
    end,
  dict:map(UnloadF, ModsLoaded).

wait_until_started() ->
  case started_p() of
    true  -> ok;
    false ->
      timer:sleep(200),
      wait_until_started()
  end.

get_undefined_function_calls([])      -> [];
get_undefined_function_calls(Modules) ->
  %% This query means: external resolved calls (XC - UC) from any of Modules to
  %% unknown functions (U) and local functions (L).
  do_query("(XLin) (((XC - UC) | ~p : Mod) || (U + L))", [Modules]).

get_unused_exports([])      -> [];
get_unused_exports(Modules) ->
  do_query("(Lin) ((X - XU) * (~p : Mod * X))", [Modules]).

do_query(QueryFmt, Args) ->
  QueryStr = lists:flatten(io_lib:format(QueryFmt, Args)),
  {ok, Res} = xref:q(?SERVER, QueryStr),
  Res.

try_add_module(Mod) ->
  case find_beam(Mod) of
    non_existing -> {error, {no_beam, Mod}};
    Beam         -> try_add_module(Mod, Beam)
  end.

find_beam(Mod) ->
  case code:is_loaded(Mod) of
    {file, Beam} -> Beam;
    false        -> code:where_is_file(atom_to_list(Mod) ++
                                         code:objfile_extension())
  end.

try_add_module(Mod, Beam) ->
  try
    Opts = proplists:get_value(options, Mod:module_info(compile)),
    case Opts =/= undefined andalso lists:member(debug_info, Opts) of
      true ->
        case xref:add_module(?SERVER, Beam) of
          {ok, Mod} -> ok;
          {error, _ErrSrc, Err} ->
            error_logger:error_msg("xref failed to add ~p: ~p", [Mod, Err]),
            {error, Err}
        end;
      false ->
         error_logger:error_msg("xref can't add ~p: no debug-info", [Mod]),
        {error, {no_beam, Mod}}
    end
  catch
    error:undef ->
      error_logger:error_msg("xref can't add ~p: no beam-file", [Mod]),
      {error, undef}
  end.

ignored_test_fun_p(M, F, 0) ->
  case lists:member({test, 0}, M:module_info(exports)) of
    false -> false;
    true  ->
      FStr = atom_to_list(F),
      lists:suffix("test", FStr) orelse lists:suffix("test_", FStr)
  end;
ignored_test_fun_p(_M, _F, _A) -> false.

get_xref_ignores(Mod) ->
  F = fun({ignore_xref, Ignores}, Acc) -> Ignores ++ Acc;
         (_, Acc) -> Acc
      end,
  lists:foldl(F, [], Mod:module_info(attributes)).

save_state() ->
  File = xref_file(),
  State = get_state(),
  case write_file(File, term_to_binary(State)) of
    ok -> ok;
    {error, _} = Error ->
      error_logger:error_msg("Failed to write ~p: ~p", [File, Error])
  end.

xref_file() ->
  edts_code:project_specific_data_file(".xref").

write_file(File, Data) ->
  {ok, BE} = application:get_env(edts_xref, file_backend),
  BE:write_file(File, Data).

read_file(File) ->
  {ok, BE} = application:get_env(edts_xref, file_backend),
  BE:read_file(File).


%%%_* Unit tests ===============================================================

start_test() ->
  eunit_test_init(),
  ?assertNot(started_p()),
  start(),
  ?assert(started_p()),
  stop(),
  teardown_eunit().

start_from_state_test() ->
  eunit_test_init(),
  start(),
  [{Mod, _}|_] = xref:info(?SERVER, modules),
  xref:remove_module(?SERVER, Mod),
  State = get_state(),
  xref:stop(?SERVER),
  do_start(State),
  ?assertEqual(State, get_state()),
  teardown_eunit().

update_paths_test() ->
  OrigPath = code:get_path(),
  ?assertEqual(undefined, whereis(?SERVER)),
  xref:start(?SERVER),
  ?assertEqual(lists:sort([{verbose,  false},
                           {warnings, true},
                           {builtins, false},
                           {recurse,  false}]),
               lists:sort(xref:get_default(?SERVER))),
  {ok, Cwd} = file:get_cwd(),
  update_paths([], []),
  ?assertEqual({ok, []}, xref:get_library_path(?SERVER)),
  xref:stop(?SERVER),
  xref:start(?SERVER),
  update_paths([Cwd], []),
  ?assertEqual({ok, [Cwd]}, xref:get_library_path(?SERVER)),
  ?assertEqual([], xref:info(?SERVER, applications)),
  xref:set_library_path(?SERVER, []),
  ModPath = code:where_is_file(atom_to_list(?MODULE) ++ ".beam"),
  AppPath =
    filename:join((filename:dirname(filename:dirname(ModPath))), "ebin"),
  DudPath = filename:dirname(AppPath),
  update_paths([], [DudPath]),
  ?assertMatch([], xref:info(?SERVER, applications)),
  stop(),
  code:set_path(OrigPath).

who_calls_test() ->
  eunit_test_init(),
  ok = start(),
  compile_and_add_test_modules(),
  ?assertEqual([], who_calls(edts_test_module2, bar, 1)),
  ?assertEqual(
    [{{edts_test_module,  bar, 1}, [37]},
     {{edts_test_module,  baz, 1}, [7]},
     {{edts_test_module2, bar, 1}, [32]}],
     who_calls(edts_test_module, bar, 1)),
  teardown_eunit().

check_undefined_functions_calls_test() ->
  eunit_test_init(),
  ok = start(),
  compile_and_add_test_modules(),
  ?assertEqual([], check_modules([], [undefined_function_calls])),
  ?assertEqual([], check_modules([edts_test_module],
                                    [undefined_function_calls])),
  ?assertMatch([{error, _File, _Line, Str}] when is_list(Str),
               check_modules([edts_test_module2],
                               [undefined_function_calls])),
  teardown_eunit().

check_unused_exports_test() ->
  eunit_test_init(),
  ok = start(),
  compile_and_add_test_modules(),
  ?assertEqual([], check_modules([], [unused_exports])),
  ?assertEqual([], check_modules([edts_test_module], [unused_exports])),
  ?assertMatch([{error, _File2, _Line1, Str1},
                {error, _File2, _Line2, Str2}] when is_list(Str1) andalso
                                                    is_list(Str2),
               check_modules([edts_test_module2], [unused_exports])),
  teardown_eunit().

check_modules_test() ->
  eunit_test_init(),
  ok = start(),
  compile_and_add_test_modules(),
  Checks = [unused_exports, undefined_function_calls],
  ?assertEqual([],     check_modules([], Checks)),
  ?assertEqual([],     check_modules([edts_test_module], Checks)),
  ?assertMatch([_, _, _], check_modules([edts_test_module2], Checks)),
  teardown_eunit().

%%%_* Unit test helpers ========================================================

eunit_test_init() ->
  stop(),
  meck:unload(),
  meck:new(dummy_file_backend, [non_strict]),
  meck:expect(dummy_file_backend, read_file, fun(_) -> {error, enoent} end),
  meck:expect(dummy_file_backend, write_file, fun(_, _) -> ok end),
  meck:new(edts_code, [passthrough]),
  meck:expect(edts_code, project_specific_data_file,
           fun(_) -> "eunit-test.xref" end),
  application:set_env(edts_xref, file_backend, dummy_file_backend).

teardown_eunit() ->
  stop(),
  meck:unload(),
  application:set_env(edts_xref, file_backend, file).

compile_and_add_test_modules() ->
  TestDir = code:lib_dir(edts, 'test'),
  EbinDir = code:lib_dir(edts, 'ebin'),
  true    = code:add_patha(EbinDir),
  compile_and_add_test_module(TestDir, EbinDir, edts_test_module),
  compile_and_add_test_module(TestDir, EbinDir, edts_test_module2).

compile_and_add_test_module(SrcDir, OutDir, Mod) ->
  case lists:keymember(Mod, 1, xref:info(?SERVER, modules)) of
    true -> ok;
    false ->
      Opts    = [debug_info, {outdir, OutDir}],
      File1   = filename:join(SrcDir, atom_to_list(Mod) ++ ".erl"),
      {ok, _} = c:c(File1, Opts),
      ok      = try_add_module(Mod)
  end.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

