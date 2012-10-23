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
-export([check_module/2,
         get_state/0,
         started_p/0,
         update/0,
         who_calls/3
        ]).

%% Internal exports.
-export([do_start/1]).

%%%_* Includes =================================================================
-include_lib("eunit/include/eunit.hrl").

%%%_* Defines ==================================================================
-define(SERVER, ?MODULE).

%%%_* Types ====================================================================

%%%_* API ======================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Starts the edts xref-server on the local node.
%% @end
-spec start() -> {ok , node()} | {error, already_started}.
%%------------------------------------------------------------------------------
start() ->
  case started_p() of
    false ->
      xref:start(?SERVER),
      ok = xref:set_default(?SERVER, [{verbose,false}, {warnings,false}]),
      update();
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
%% Do an xref-analysis of Module, applying Checks
%% @end
-spec check_module(Module::module(), Checks::[xref:analysis()]) ->
                      {ok, [{ Type::error
                            , File::string()
                            , Line::non_neg_integer()
                            , Description::string()}]}.
%%------------------------------------------------------------------------------
check_module(Module, Checks) ->
  File = proplists:get_value(source, Module:module_info(compile)),
  Fun  = fun(Check) -> do_check_module(Module, File, Check) end,
  lists:append(lists:map(Fun, Checks)).

do_check_module(Mod0, File, undefined_function_calls) ->
  QueryFmt = "(XLin) ((XC - UC) || (XU - X - B) * XC | ~p : Mod)",
  QueryStr = lists:flatten(io_lib:format(QueryFmt, [Mod0])),
  {ok, Res} = xref:q(?SERVER, QueryStr),
  FmtFun = fun({{{Mod, _, _}, {CM, CF, CA}}, Lines}) when Mod =:= Mod0 ->
               Desc = io_lib:format("Call to undefined function ~p:~p/~p",
                                    [CM, CF, CA]),
               [{error, File, Line, lists:flatten(Desc)} || Line <- Lines]
           end,
  lists:flatmap(FmtFun, Res);
do_check_module(Mod, File, unused_exports) ->
  QueryFmt  = "(Lin) ((X - XU) * (~p : Mod * X))",
  QueryStr  = lists:flatten(io_lib:format(QueryFmt, [Mod])),
  Ignores   = sets:from_list(get_xref_ignores(Mod)),
  {ok, Res} = xref:q(?SERVER, QueryStr),
  FmtFun = fun({{M, F, A}, Line}, Acc) ->
               case sets:is_element({F, A}, Ignores) orelse
                    ignored_test_fun_p(M, F, A) of
                 true  -> Acc;
                 false ->
                   Desc = io_lib:format("Unused export ~p:~p/~p", [M, F, A]),
                   [{error, File, Line, lists:flatten(Desc)}|Acc]
               end
           end,
  lists:foldl(FmtFun, [], Res).

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

update_paths(LibDirs, AppDirs) ->
  ok = xref:set_library_path(?SERVER, LibDirs),
  ModsToLoad =
    lists:flatmap(
      fun(Dir) ->
          Beams = filelib:wildcard(filename:join(Dir, "*.beam")),
          [{list_to_atom(filename:basename(B, ".beam")), B} || B <- Beams]
      end, AppDirs),
  ModsLoaded = xref:info(?SERVER, modules),
  %% Add/update new modules
  LoadF =
    fun({Mod, Beam}) ->
        case proplists:get_value(Mod, ModsLoaded) of
          undefined -> % New module
            case xref:add_module(?SERVER, Beam) of
              {ok, Mod} -> ok;
              {error, _ErrSrc, Err} ->
                error_logger:error_msg("xref failed to add ~p: ~p", [Mod, Err])
            end;
          Info ->
            Dir = filename:dirname(Beam),
            case proplists:get_value(directory, Info) of
              Dir     -> ok; % Same as old module
              _OldDir -> xref:replace_module(?SERVER, Mod, Beam) % Mod has moved
            end
        end
    end,
  lists:foreach(LoadF, ModsToLoad),

  %% Remove delete modules
  UnloadF =
    fun({Mod, _Info}) ->
        case lists:keymember(Mod, 1, ModsToLoad) of
          true  -> ok;
          false -> xref:remove_module(?SERVER, Mod)
        end
    end,
  lists:foreach(UnloadF, ModsLoaded).

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
  xref:add_module(?SERVER, test_module),
  xref:add_module(?SERVER, test_module2),
  File1 = filename:join(Cwd, "test_module.erl"),
  File2 = filename:join(Cwd, "test_module2.erl"),
  ?assertEqual([],
               do_check_module(test_module, File1, undefined_function_calls)),
  ?assertMatch([{error, File2, 33, Str}] when is_list(Str),
               do_check_module(test_module2, File2, undefined_function_calls)),
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
               do_check_module(test_module, File1, unused_exports)),
  ?assertMatch([{error, File2, 31, Str}] when is_list(Str),
               do_check_module(test_module2, File2, unused_exports)),
  stop(),
  code:set_path(OrigPath).

check_module_test() ->
  OrigPath = code:get_path(),
  code:set_path(mock_path()),
  start(),
  xref:add_module(?SERVER, test_module),
  xref:add_module(?SERVER, test_module2),
  Checks = [unused_exports, undefined_function_calls],
  ?assertEqual([],     check_module(test_module, Checks)),
  ?assertMatch([_, _], check_module(test_module2, Checks)),
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

