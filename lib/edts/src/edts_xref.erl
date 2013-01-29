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
         start/1,
         stop/0
        ]).

%% API
-export([check_modules/2,
         allowed_checks/0,
         get_state/0,
         started_p/0,
         update/0,
         who_calls/3
        ]).

%% Internal exports.
-export([do_start/1]).

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
  case started_p() of
    false ->
      {ok, Pid} = xref:start(?SERVER),
      ok = xref:set_default(?SERVER, [{verbose,false}, {warnings,false}]),
      update(),
      {ok, Pid};
    true ->
      {error, already_started}
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Starts the edts xref-server with State on the local node.
%% @end
-spec start(State::term()) -> {ok , node()} | {error, already_started}.
%%------------------------------------------------------------------------------
start(State) ->
  case started_p() of
    false ->
      proc_lib:start(?MODULE, do_start, [State]),
      wait_until_started(),
      ok = xref:set_default(?SERVER, [{verbose,false}, {warnings,false}]),
      update(),
      {ok, node()};
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
    _Pid      -> xref:stop(?SERVER)
  end.

%% API

%%------------------------------------------------------------------------------
%% @doc
%% Returns the internal state of the edts xref-server.
%% @end
-spec get_state() -> term().
%%------------------------------------------------------------------------------
get_state() ->
  update(),
  {status, _, _, [_, _, _, _, Misc]} = sys:get_status(?SERVER),
  proplists:get_value("State", lists:append([D || {data, D} <- Misc])).

update() ->
  {LibDirs, AppDirs} = lib_and_app_dirs(),
  update_paths(LibDirs, AppDirs),
  xref:update(?SERVER).

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
  Files = [proplists:get_value(source, M:module_info(compile)) || M <- Modules],
  Fun  = fun(Check) -> do_check_modules(Modules, Files, Check) end,
  lists:append(lists:map(Fun, Checks)).

do_check_modules(Modules, Files, undefined_function_calls) ->
  QueryFmt = "(XLin) ((XC - UC) || (XU - X - B) * XC | ~p : Mod)",
  QueryStr = lists:flatten(io_lib:format(QueryFmt, [Modules])),
  {ok, Res} = xref:q(?SERVER, QueryStr),
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
  QueryFmt  = "(Lin) ((X - XU) * (~p : Mod * X))",
  QueryStr  = lists:flatten(io_lib:format(QueryFmt, [Modules])),
  %% Ignores   = [{Mod, sets:from_list(get_xref_ignores(Mod))} || Mod <- Modules],
  ModuleFiles = lists:zip(Modules, Files),
  {ok, Res} = xref:q(?SERVER, QueryStr),
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


%%%_* INTERNAL functions =======================================================
do_start(State) ->
  erlang:register(?SERVER, self()),
  proc_lib:init_ack({ok, self()}),
  gen_server:enter_loop(xref, [], State).

wait_until_started() ->
  case started_p() of
    true  -> ok;
    false ->
      timer:sleep(200),
      wait_until_started()
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Ensure that all new beam-files are added to the xref-server state.
%% @end
-spec update_paths([filename:filename()], [filename:filename()]) -> ok.
%%------------------------------------------------------------------------------
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

try_add_module(Mod, Beam) ->
  try
    Opts = proplists:get_value(options, Mod:module_info(compile)),
    case lists:member(debug_info, Opts) of
      true ->
        case xref:add_module(?SERVER, Beam) of
          {ok, Mod} -> ok;
          {error, _ErrSrc, Err} ->
            error_logger:error_msg("xref failed to add ~p: ~p", [Mod, Err]),
            {error, Err}
        end;
      false ->
         error_logger:error_msg("xref can't add ~p: no debug-info", [Mod]),
        {error, no_beam}
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

lib_and_app_dirs() ->
  ErlLibDir = code:lib_dir(),
  Paths = [D || D <- code:get_path(), filelib:is_dir(D), D =/= "."],
  lists:partition(fun(Path) -> lists:prefix(ErlLibDir, Path) end, Paths).

%%%_* Unit tests ===============================================================

start_test() ->
  OrigPath = code:get_path(),
  code:set_path(mock_path()),
  stop(),
  ?assertEqual(undefined, whereis(?SERVER)),
  {error, not_started} = stop(),
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
  OrigPath = code:get_path(),
  code:set_path(mock_path()),
  start(),
  xref:add_module(?SERVER, test_module),
  xref:add_module(?SERVER, test_module2),
  ?assertEqual([], who_calls(test_module2, bar, 1)),
  ?assertEqual(
    [{test_module, bar, 1}, {test_module, baz, 1}, {test_module2, bar, 1}],
     who_calls(test_module, bar, 1)),
  stop(),
  code:set_path(OrigPath).

check_undefined_functions_calls_test() ->
  OrigPath = code:get_path(),
  code:set_path(mock_path()),
  start(),
  {ok, Cwd} = file:get_cwd(),
  {ok, test_module} = xref:add_module(?SERVER, test_module),
  {ok, test_module2} = xref:add_module(?SERVER, test_module2),
  File1 = filename:join(Cwd, "test_module.erl"),
  File2 = filename:join(Cwd, "test_module2.erl"),
  ?assertEqual([], do_check_modules([test_module],
                                    [File1],
                                    undefined_function_calls)),
  ?assertMatch([{error, File2, 33, Str}] when is_list(Str),
               do_check_modules([test_module2],
                               [File2],
                               undefined_function_calls)),
  stop(),
  code:set_path(OrigPath).

check_unused_exports_test() ->
  OrigPath = code:get_path(),
  code:set_path(mock_path()),
  start(),
  {ok, Cwd} = file:get_cwd(),
  xref:add_module(?SERVER, test_module),
  xref:add_module(?SERVER, test_module2),
  File1 = filename:join(Cwd, "test_module.erl"),
  File2 = filename:join(Cwd, "test_module2.erl"),
  ?assertEqual([],
               do_check_modules([test_module], [File1], unused_exports)),
  ?assertMatch([{error, File2, 31, Str}] when is_list(Str),
               do_check_modules([test_module2], [File2], unused_exports)),
  stop(),
  code:set_path(OrigPath).

check_modules_test() ->
  OrigPath = code:get_path(),
  code:set_path(mock_path()),
  start(),
  xref:add_module(?SERVER, test_module),
  xref:add_module(?SERVER, test_module2),
  Checks = [unused_exports, undefined_function_calls],
  ?assertEqual([],     check_modules([test_module], Checks)),
  ?assertMatch([_, _], check_modules([test_module2], Checks)),
  stop(),
  code:set_path(OrigPath).

%%%_* Unit test helpers ========================================================

mock_path() ->
  {LibDirs, AppDirs0} = lib_and_app_dirs(),
  case lists:filter(fun(P) -> filename:basename(P) =:= ".eunit" end, AppDirs0) of
    [_] = EunitDir -> EunitDir ++ LibDirs;
    _              -> AppDirs0 ++ LibDirs
  end.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

