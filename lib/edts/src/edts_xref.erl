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
-module(edts_xref).

%%%_* Exports ==================================================================

%% Extended xref API
-export([start/0,
         stop/0
        ]).

%% API
-export([allowed_checks/0,
         check_modules/2,
         get_state/0,
         start/1,
         started_p/0,
         update/0,
         update_modules/1,
         who_calls/3
        ]).


%% Internal exports.
-export([do_start_from_state/1]).

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
%% Startns the edts xref-server on the local node.
%% @end
-spec start() -> {ok, pid()} | {error, already_started}.
%%------------------------------------------------------------------------------
start() ->
  case started_p() of
    false ->
      {ok, _Pid} = do_start_from_scratch(),
      update(),
      ok;
    true ->
      {error, already_started}
  end.


%%------------------------------------------------------------------------------
%% @doc
%% Starts the edts xref-server with State on the local node.
%% @end
-spec start(State::term()) -> ok | {error, already_started}.
%%------------------------------------------------------------------------------
start(State) ->
  case started_p() of
    false ->
      proc_lib:start(?MODULE, do_start_from_state, [State]),
      ok = xref:set_default(?SERVER, [{verbose,false}, {warnings,false}]),
      wait_until_started(),
      {ok, _} = update(),
      ok;
    true ->
      {error, already_started}
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
%% Do an xref-analysis of Modules, applying Checks
%% @end
-spec check_modules([Modules::module()], Checks::[xref:analysis()]) ->
                       {ok, [{ Type::error,
                               File::string(),
                               Line::non_neg_integer(),
                               Description::string()}]}.
%%------------------------------------------------------------------------------
check_modules(Modules, Checks) ->
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
  Str = lists:flatten(io_lib:format("(E || ~p)", [{M, F, A}])),
  {ok, Calls} = xref:q(edts_xref, Str),
  [Caller || {Caller, _Callee} <- Calls].


%%------------------------------------------------------------------------------
%% @doc
%% Update the internal state of the xref server.
%% @end
-spec update() -> {ok, [module()]}.
%%------------------------------------------------------------------------------
update() ->
  {ErlLibDirs, AppDirs} = edts_util:lib_and_app_dirs(),
  update_paths(ErlLibDirs, AppDirs),
  xref:update(?SERVER).

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

do_start_from_scratch() ->
  {ok, Pid} = xref:start(?SERVER),
  ok = xref:set_default(?SERVER, [{verbose,false}, {warnings,false}]),
  {ok, Pid}.

do_start_from_state(State) ->
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

%%%_* Unit tests ===============================================================

start_test() ->
  case started_p() of
    true  -> stop();
    false -> ok
  end,
  OrigPath = code:get_path(),
  code:set_path(mock_path()),
  MonitorRef = erlang:monitor(process, ?SERVER),
  stop(),
  receive
    {'DOWN', MonitorRef, _Type, _Object, _Info} ->
      ?assertEqual(undefined, whereis(?SERVER)),
      {error, not_started} = stop()
  end,
  start(),
  {error, already_started} = start(),
  ?assertMatch(Pid when is_pid(Pid), whereis(?SERVER)),
  State = get_state(),
  xref:stop(?SERVER),
  ?assertEqual(undefined, whereis(?SERVER)),
  start(State),
  {error, already_started} = start(State),
  State = get_state(),
  stop(),
  code:set_path(OrigPath).

update_paths_test() ->
  case started_p() of
    true  -> stop();
    false -> ok
  end,
  OrigPath = code:get_path(),
  code:set_path(mock_path()),
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
  stop(),
  ok = start(),
  compile_and_add_test_modules(),
  ?assertEqual([], who_calls(edts_test_module2, bar, 1)),
  ?assertEqual(
    [{edts_test_module,  bar, 1},
     {edts_test_module,  baz, 1},
     {edts_test_module2, bar, 1}],
     who_calls(edts_test_module, bar, 1)),
  stop().

check_undefined_functions_calls_test() ->
  stop(),
  ok = start(),
  compile_and_add_test_modules(),
  ?assertEqual([], check_modules([edts_test_module],
                                    [undefined_function_calls])),
  ?assertMatch([{error, _File, _Line, Str}] when is_list(Str),
               check_modules([edts_test_module2],
                               [undefined_function_calls])),
  stop().

check_unused_exports_test() ->
  stop(),
  ok = start(),
  compile_and_add_test_modules(),
  ?assertEqual([], check_modules([edts_test_module], [unused_exports])),
  ?assertMatch([{error, _File2, _Line1, Str1},
                {error, _File2, _Line2, Str2}] when is_list(Str1) andalso
                                                    is_list(Str2),
               check_modules([edts_test_module2], [unused_exports])),
  stop().

check_modules_test() ->
  stop(),
  ok = start(),
  compile_and_add_test_modules(),
  Checks = [unused_exports, undefined_function_calls],
  ?assertEqual([],     check_modules([edts_test_module], Checks)),
  ?assertMatch([_, _, _], check_modules([edts_test_module2], Checks)),
  stop().

%%%_* Unit test helpers ========================================================

mock_path() ->
  {LibDirs, AppDirs0} = edts_util:lib_and_app_dirs(),
  case lists:filter(fun(P) -> filename:basename(P) =:= ".eunit" end, AppDirs0) of
    [_] = EunitDir -> EunitDir ++ LibDirs;
    _              -> AppDirs0 ++ LibDirs
  end.

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

