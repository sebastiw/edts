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
         reload_module/2,
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
  case whereis(?SERVER) of
    undefined ->
      ErlLibDir = code:lib_dir(),
      Paths = [D || D <- code:get_path(), filelib:is_dir(D)],
      {LibDirs, AppDirs} = lists:partition(fun(Path) ->
                                               lists:prefix(ErlLibDir, Path)
                                           end,
                                           Paths),
      xref:start(?SERVER),
      init(LibDirs, AppDirs);
    _Pid ->
      {error, already_started}
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Starts the edts xref-server with State on the local node.
%% @end
-spec start(State::term()) -> {ok , node()} | {error, already_started}.
%%------------------------------------------------------------------------------
start(State) ->
  case whereis(?SERVER) of
    undefined ->
      proc_lib:start(?MODULE, do_start, [State]),
      xref:update(?SERVER),
      {ok, node()};
    _Pid ->
      {error, already_started}
  end.

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
  xref:update(?SERVER),
  {status, _, _, [_, _, _, _, Misc]} = sys:get_status(?SERVER),
  proplists:get_value("State", lists:append([D || {data, D} <- Misc])).

%%------------------------------------------------------------------------------
%% @doc
%% Reload the updated Mod from File into the xref callgraph..
%% @end
-spec reload_module(filename:filename(), module()) -> {ok, [module()]}.
%%------------------------------------------------------------------------------
reload_module(File, Mod) ->
  case xref:replace_module(?SERVER, Mod, File) of
    {ok, Mod} -> ok;
    {error, xref_base, {no_such_module, Mod}} ->
      xref:add_module(?SERVER, File)
  end,
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
  io:format("Mod ~p, File ~p~n", [Mod0, File]),
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

init(LibDirs, AppDirs) ->
  ok = xref:set_default(?SERVER, [{verbose,false}, {warnings,false}]),
  ok = xref:set_library_path(?SERVER, LibDirs),
  lists:foreach(fun(D) ->
                    AppDir = filename:dirname(D),
                    case xref:add_application(?SERVER, AppDir) of
                      {ok, _}                               -> ok;
                      {error, _Mod, {application_clash, _}} -> ok;
                      {error, Mod, Err}                    ->
                        Fmt = "(~p) xref error not add ~p: ~p",
                        error_logger:error_msg(Fmt, [node(), Mod, Err])
                    end
                end,
                AppDirs).

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
  stop().

init_test() ->
  ?assertEqual(undefined, whereis(?SERVER)),
  xref:start(?SERVER),
  ?assertEqual(lists:sort([{verbose,  false},
                           {warnings, true},
                           {builtins, false},
                           {recurse,  false}]),
               lists:sort(xref:get_default(?SERVER))),
  {ok, Cwd} = file:get_cwd(),
  init([], []),
  ?assertEqual({ok, []}, xref:get_library_path(?SERVER)),
  xref:stop(?SERVER),
  xref:start(?SERVER),
  init([Cwd], []),
  ?assertEqual({ok, [Cwd]}, xref:get_library_path(?SERVER)),
  ?assertEqual([], xref:info(?SERVER, applications)),
  xref:set_library_path(?SERVER, []),
  ModPath = code:where_is_file(atom_to_list(?MODULE) ++ ".beam"),
  AppPath =
    filename:join((filename:dirname(filename:dirname(ModPath))), "ebin"),
  DudPath = filename:dirname(AppPath),
  init([], [DudPath]),
  ?assertMatch([], xref:info(?SERVER, applications)),
  stop().

who_calls_test() ->
  start(),
  xref:add_module(?SERVER, test_module),
  xref:add_module(?SERVER, test_module2),
  ?assertEqual([], who_calls(test_module2, bar, 1)),
  ?assertEqual(
    [{test_module, bar, 1}, {test_module, baz, 1}, {test_module2, bar, 1}],
     who_calls(test_module, bar, 1)),
  stop().

check_undefined_functions_calls_test() ->
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
  stop().

check_unused_exports_test() ->
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
  stop().

check_module_test() ->
  start(),
  xref:add_module(?SERVER, test_module),
  xref:add_module(?SERVER, test_module2),
  Checks = [unused_exports, undefined_function_calls],
  ?assertEqual([],     check_module(test_module, Checks)),
  ?assertMatch([_, _], check_module(test_module2, Checks)),
  stop().

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

