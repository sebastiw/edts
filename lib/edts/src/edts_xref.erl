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

%% xref forwarding API
-export([add_module/2,
         add_application/2,
         add_directory/2,
         q/2,
         replace_module/3,
         set_default/2,
         set_library_path/2
        ]).

%% Extended xref API
-export([start/0,
         stop/0,
         update/0
        ]).

%% API
-export([check_module/2,
         reload_module/2
        ]).

%% Internal exports.
-export([do_start/0]).

%%%_* Includes =================================================================

%%%_* Defines ==================================================================
-define(SERVER, ?MODULE).

%%%_* Types ====================================================================

%%%_* API ======================================================================

%% Forwarding to xref.
add_application(Srv, File)   -> xref:add_application(Srv, File).
add_directory(Srv, File)     -> xref:add_directory(Srv, File).
add_module(Srv, File)        -> xref:add_module(Srv, File).
q(Srv, Q)                    -> xref:q(Srv, Q).
replace_module(Srv, M, File) -> xref:replace_module(Srv, M, File).
set_default(Srv, Opts)       -> xref:set_default(Srv, Opts).
set_library_path(Srv, Opts) -> xref:set_library_path(Srv, Opts).


%%------------------------------------------------------------------------------
%% @doc
%% Starts the edts xref-server on the local node.
%% @end
-spec start() -> {ok , node()} | {error, already_started}.
%%------------------------------------------------------------------------------
start() ->
  case whereis(?SERVER) of
    undefined ->
      proc_lib:start(?MODULE, do_start, []),
      xref:update(?SERVER),
      {ok, node()};
    _Pid ->
      {error, already_started}
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Stops the edts xref-server on the local node.
%% @end
-spec stop() -> ok.
%%------------------------------------------------------------------------------
stop() ->
  case whereis(?SERVER) of
    undefined -> {error, not_started};
    _Pid      -> xref:stop(?SERVER)
  end.

update() ->
  Res = xref:update(?SERVER),
  {status, _, _, [_, _, _, _, Misc]} = sys:get_status(?SERVER),
  State = proplists:get_value("State", lists:append([D || {data, D} <- Misc])),
  file:write_file("foo", term_to_binary(State)),
  Res.

%% API
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
  update().

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
  {ok, Res} = xref:q(edts_code, QueryStr),
  FmtFun = fun({{{Mod, _, _}, {CM, CF, CA}}, [Line]}) when Mod =:= Mod0 ->
               Desc = io_lib:format("Call to undefined function ~p:~p/~p",
                                    [CM, CF, CA]),
               {error, File, Line, lists:flatten(Desc)}
           end,
  lists:map(FmtFun, Res);
do_check_module(Mod, File, unused_exports) ->
  QueryFmt  = "(Lin) ((X - XU) * (~p : Mod * X))",
  QueryStr  = lists:flatten(io_lib:format(QueryFmt, [Mod])),
  Ignores   = sets:from_list(get_xref_ignores(Mod)),
  {ok, Res} = xref:q(edts_code, QueryStr),
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

%%%_* INTERNAL functions =======================================================
do_start() ->
  case file:read_file("foo") of
    {ok, BinState} ->
      erlang:register(?SERVER, self()),
      proc_lib:init_ack({ok, self()}),
      gen_server:enter_loop(xref, [], binary_to_term(BinState));
    {error, _} ->
      ErlLibDir = code:lib_dir(),
      Paths = [D || D <- code:get_path(), filelib:is_dir(D)],
      {LibDirs, AppDirs} = lists:partition(fun(Path) ->
                                               lists:prefix(ErlLibDir, Path)
                                           end,
                                           Paths),
      init(LibDirs, AppDirs),
      proc_lib:init_ack({ok, self()}),
      xref:start(?SERVER)
  end.

init(LibDirs, AppDirs) ->
  ok = xref:set_default(?SERVER, [{verbose,false}, {warnings,false}]),
  ok = xref:set_library_path(?SERVER, LibDirs),
  lists:foreach(fun(D) ->
                    AppDir = filename:dirname(D),
                    case xref:add_application(?SERVER, AppDir) of
                      {error, _, _} -> xref:add_directory(?SERVER, D);
                      {ok, _}       -> ok
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


%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

