%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc utility library for edts.
%%% @end
%%% @author Thomas Järvstrand <tjarvstrand@gmail.com>
%%% @copyright
%%% Copyright 2013 Thomas Järvstrand <tjarvstrand@gmail.com>
%%%
%%% This file is part of EDTS.
%%%
%%% EDTS is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU Lesser General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% EDTS is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Lesser General Public License for more details.
%%%
%%% You should have received a copy of the GNU Lesser General Public License
%%% along with EDTS. If not, see <http://www.gnu.org/licenses/>.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration =======================================================
-module(edts_util).

%%%_* Includes =================================================================

-include_lib("eunit/include/eunit.hrl").

%%%_* Exports ==================================================================

-export([assoc/2,
         assoc/3,
         directory_child_p/2,
         expand_code_paths/2,
         filename_to_module/1,
         make_nodename/1,
         pid2atom/1,
         project_lib_file_p/1,
         project_lib_file_p/3,
         lib_and_app_dirs/0,
         shorten_path/1]).

%%%_* Defines ==================================================================

%%%_* Types ====================================================================

%%%_* API ======================================================================

assoc(K, AList) ->
  case lists:keyfind(K, 1, AList) of
    false  -> {error, {notfound, K}};
    {K, V} -> {ok, V}
  end.

assoc(K, AList, Default) ->
  case assoc(K, AList) of
    {error, {notfound, _}} -> Default;
    {ok, V}                -> V
  end.

expand_code_paths("", _LibDirs) -> [];
expand_code_paths(ProjectRoot, LibDirs) ->
  RootPaths = [filename:join(ProjectRoot, "ebin"),
               filename:join(ProjectRoot, "test")],
  F = fun(Dir) -> expand_code_path(ProjectRoot, Dir) end,
  LibPaths = lists:flatmap(F, LibDirs),
  RootPaths ++ LibPaths.

expand_code_path(Root, Dir) ->
  Fun = fun(F) -> [filename:join(F, "ebin"), filename:join(F, "test")] end,
  lists:flatmap(Fun, filelib:wildcard(filename:join([Root, Dir, "*"]))).

filename_to_module(Filename) ->
  list_to_atom(filename:basename(filename:basename(Filename))).

make_nodename(NameStr) ->
  case string:tokens(NameStr, "@") of
    [Name] ->
      [_Name, Host] = string:tokens(atom_to_list(node()), "@"),
      list_to_atom(Name ++ "@" ++ Host);
    [_, _] ->
      list_to_atom(NameStr)
  end.

pid2atom(Pid) ->
  PidStr0 = pid_to_list(Pid),
  list_to_atom(string:sub_string(PidStr0, 2, length(PidStr0) - 1)).

lib_and_app_dirs() ->
  ErlLibDir = code:lib_dir(),
  lists:partition(fun(Path) -> lists:prefix(ErlLibDir, Path) end,
                  code:get_path()).

%%------------------------------------------------------------------------------
%% @doc
%% Return true if File belongs to this node's EDTS project.
-spec project_lib_file_p(File :: file:filename()) -> boolean().
%%------------------------------------------------------------------------------
project_lib_file_p(File) ->
  {ok, ProjectDir} = application:get_env(edts, project_root_dir),
  {ok, LibDirs} = application:get_env(edts, project_lib_dirs),
  project_lib_file_p(ProjectDir, LibDirs, File).


%%------------------------------------------------------------------------------
%% @doc
%% Return true if File is located underneath any of LibDirs relative to
%% ProjectDir.
-spec project_lib_file_p(ProjectDir :: file:filename(),
                         LibDirs :: [file:filename()],
                         File :: file:filename()) ->
                           boolean().
%%------------------------------------------------------------------------------
project_lib_file_p(ProjectDir, LibDirs, File) ->
  lists:any(fun(LibDir) ->
                directory_child_p(filename:join(ProjectDir, LibDir), File)
            end,
            LibDirs) orelse
    lists:any(fun(Dir) ->
                  filename:join(ProjectDir, Dir) =:= filename:dirname(File)
              end,
              ["src", "test", "ebin", ""]).


project_lib_file_p_test_() ->
  [?_assertNot(project_lib_file_p("root", [], "")),
   ?_assert(project_lib_file_p("root", [], "root/a")),
   ?_assert(project_lib_file_p("root", [], "root/src/a")),
   ?_assert(project_lib_file_p("root", [], "root/ebin/a")),
   ?_assert(project_lib_file_p("root", [""], "root/")),
   ?_assert(project_lib_file_p("root", ["a"], "root/a/b")),
   ?_assertNot(project_lib_file_p("root", ["a"], "/b")),
   ?_assert(project_lib_file_p("a", ["b"], "a/b")),
   ?_assert(project_lib_file_p("a", ["b"], "a/c")),
   ?_assert(project_lib_file_p("a", ["b", "c"], "a/c")),
   ?_assertNot(project_lib_file_p("a", ["b", "c"], "a/a/c")),
   ?_assert(project_lib_file_p("a", ["b", "c"], "a/c/d"))
  ].


%%------------------------------------------------------------------------------
%% @doc
%% Return true if File is located at any level underneath Dir in the file tree.
%% File can be a directory. This function only supports tilde expansion of the
%% current user's home directory (~/...), not for specific users
%% (eg. ~myuser/...)
-spec directory_child_p(Dir :: file:filename(), File :: file:filename()) ->
                           boolean().
%%------------------------------------------------------------------------------
directory_child_p(Dir, File) ->
  lists:prefix(filename:absname(Dir), filename:absname(File)).

shorten_path("") -> "";
shorten_path(P)  ->
  case shorten_path(filename:split(P), []) of
    [Component] -> Component;
    Components  -> filename:join(Components)
  end.

shorten_path([],           [])         -> ["."];
shorten_path([],           Acc)        -> lists:reverse(Acc);
shorten_path(["."|T],      Acc)        -> shorten_path(T, Acc);
shorten_path([".."|T],     [])         -> shorten_path(T, [".."]);
shorten_path([".."|T], [".."|_] = Acc) -> shorten_path(T, [".."|Acc]);
shorten_path([".."|T],     Acc)        -> shorten_path(T, tl(Acc));
shorten_path([H|T],        Acc)        -> shorten_path(T, [H|Acc]).



%%%_* Internal functions =======================================================


%%%_* Unit tests ===============================================================

expand_code_paths_test() ->
  ?assertEqual([], expand_code_paths("", ["/foo"])),
  ?assertEqual(["/foo/ebin", "/foo/test"], expand_code_paths("/foo", [])).

expand_code_path_test() ->
  meck:new(filelib, [passthrough, unstick]),
  meck:expect(filelib, wildcard,
              fun(Path) ->
                  Dirname = filename:dirname(Path),
                  [filename:join(Dirname, "foo"), filename:join(Dirname, "bar")]
              end),
  Root = "/foo",
  Lib  = "lib",
  ?assertEqual([filename:join([Root, "lib", "foo", "ebin"]),
                filename:join([Root, "lib", "foo", "test"]),
                filename:join([Root, "lib", "bar", "ebin"]),
                filename:join([Root, "lib", "bar", "test"])],
               expand_code_path(Root, Lib)),
  meck:unload().

shorten_path_test_() ->
  [?_assertEqual("", shorten_path("")),
   ?_assertEqual(".", shorten_path(".")),
   ?_assertEqual("..", shorten_path("..")),
   ?_assertEqual("../..", shorten_path("../..")),
   ?_assertEqual("../ebin", shorten_path("../ebin")),
   ?_assertEqual("..", shorten_path("../ebin/..")),
   ?_assertEqual("..", shorten_path("../ebin/./.."))
  ].

%%%_* Test helpers =============================================================

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

