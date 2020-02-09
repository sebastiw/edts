%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Utility library for webmachine resources
%%% @end
%%% @author Thomas Järvstrand <tjarvstrand@gmail.com>
%%% @copyright
%%% Copyright 2012 Thomas Järvstrand <tjarvstrand@gmail.com>
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
-module(edts_cmd_lib).

%%%_* Exports ==================================================================

%% Application callbacks
-export([ check_exists_and_do_rpc/3,
          exists_p/2,
          validate/2]).

%%%_* Includes =================================================================

-include_lib("eunit/include/eunit.hrl").

%%%_* Defines ==================================================================

-define(a2l(A), atom_to_list(A)).

%%%_* Types ====================================================================

%%%_* API ======================================================================

check_exists_and_do_rpc(Ctx, Required, {M, F, ArgKeys}) ->
  case exists_p(Ctx, [nodename|Required]) of
    false -> {false, Ctx};
    true  ->
      Node = orddict:fetch(nodename, Ctx),
      case edts:call(Node, M, F, [orddict:fetch(Key, Ctx) || Key <- ArgKeys]) of
        {ok, Result} -> {true, orddict:store(result, Result, Ctx)};
        {error, _}   -> {false, Ctx}
      end
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Check that all elements of resource_exist.
%% @end
-spec exists_p(orddict:orddict(), [atom]) ->
                  {boolean(), orddict:orddict()}.
%%------------------------------------------------------------------------------
exists_p(Ctx, Keys) ->
  F = fun(Key) -> (atom_to_exists_p(Key))(Ctx) end,
  case lists:partition(F, Keys) of
    {_, []}       -> true;
    {_, NoExists} ->
      edts_log:debug("resource_exists failed: ~p", [NoExists]),
      false
  end.


%%------------------------------------------------------------------------------
%% @doc
%% Validate Ctx0 and convert values to internal representation.
%% @end
-spec validate(orddict:orddict(), [atom]) ->
                  {boolean(), orddict:orddict()}.
%%------------------------------------------------------------------------------
validate(Ctx0, Keys) ->
  F = fun(Key, Ctx) ->
          Name = case Key of
                   Key when is_atom(Key) -> Key;
                   {_Type, Props}        ->
                     {ok, N} = edts_util:assoc(name, Props),
                     N
                 end,
          case (term_to_validate(Key))(Ctx) of
            {ok, Value} ->
              orddict:store(Name, Value, Ctx);
            {error, Rsn} ->
              edts_log:error("API input validation failed. Key ~p, Rsn: ~p",
                             [Key, Rsn]),
              throw({error, Key})
          end
      end,
  try
    {ok, lists:foldl(F, Ctx0, Keys)}
  catch
    throw:{error, Key0} = E ->
      Key = case Key0 of
              Key1          when is_atom(Key1) -> atom_to_list(Key1);
              {_Type, Key1} when is_atom(Key1) -> Key1;
              {_Type, Props} ->
                {ok, N} = edts_util:assoc(name, Props),
                N
            end,
      Value = orddict:fetch(Key, Ctx0),
      edts_log:debug("Invalid Request, ~nKey: ~p~nValue: ~p", [Key, Value]),
      E
  end.

%%%_* Internal functions =======================================================
atom_to_exists_p(nodename) -> fun nodename_exists_p/1;
atom_to_exists_p(module)   -> fun module_exists_p/1;
atom_to_exists_p(modules)  -> fun modules_exists_p/1.


term_to_validate(app_include_dirs) ->
  fun(Ctx) -> dirs_validate(Ctx, app_include_dirs) end;
term_to_validate(arity)                -> fun arity_validate/1;
term_to_validate(code)                 ->
  fun(Ctx) -> string_validate(Ctx, code) end;
term_to_validate(string)                 ->
  fun(Ctx) -> string_validate(Ctx, string) end;
term_to_validate(indent) ->
  fun(Ctx) -> non_neg_integer_validate(Ctx, indent) end;
term_to_validate(max_column) ->
  fun(Ctx) -> non_neg_integer_validate(Ctx, max_column) end;
term_to_validate({enum_list, Props})   ->
  fun(Ctx) -> enum_list_validate(Ctx, Props) end;
term_to_validate({enum, Props})        ->
  fun(Ctx) -> enum_validate(Ctx, Props) end;
term_to_validate(erlang_cookie) ->
  fun(Ctx) -> string_validate(Ctx, erlang_cookie, undefined) end;
term_to_validate(exported)             ->
  fun(Ctx) ->
      enum_validate(Ctx, [{name,    exported},
                          {allowed, [true, false, all]},
                          {default, all},
                          {required, false}])
  end;
term_to_validate(expressions)          ->
  fun(Ctx) -> strings_validate(Ctx, expressions) end;
term_to_validate(file)                 -> fun file_validate/1;
term_to_validate({file, Key})          ->
  fun(Ctx) -> file_validate(Ctx, ?a2l(Key)) end;
term_to_validate(files)                -> fun files_validate/1;
term_to_validate(function)             -> fun function_validate/1;
term_to_validate(info_level)           ->
  fun(Ctx) ->
      enum_validate(Ctx, [{name,     info_level},
                          {allowed,  [basic, detailed]},
                          {default,  basic},
                          {required, false}])
  end;
term_to_validate(line) ->
  fun(Ctx) -> non_neg_integer_validate(Ctx, line) end;
term_to_validate(module)               -> fun module_validate/1;
term_to_validate(modules)              -> fun modules_validate/1;
term_to_validate(nodename)             -> fun nodename_validate/1;
term_to_validate(process)              -> fun process_validate/1;
term_to_validate(project_name)         ->
  fun(Ctx) -> string_validate(Ctx, project_name, "") end;
term_to_validate(project_root)         -> fun project_root_validate/1;
term_to_validate(project_include_dirs) ->
  fun(Ctx) -> dirs_validate(Ctx, project_include_dirs) end;
term_to_validate(project_lib_dirs)     ->
  fun(Ctx) -> dirs_validate(Ctx, project_lib_dirs) end.

%%------------------------------------------------------------------------------
%% @doc
%% Validate arity
%% @end
-spec arity_validate(orddict:orddict()) -> {ok, non_neg_integer()} | error.
%%------------------------------------------------------------------------------
arity_validate(Ctx) ->
  Str = orddict:fetch(arity, Ctx),
  try
    case list_to_integer(Str) of
      Arity when Arity >= 0 -> {ok, Arity};
      _ -> error
    end
  catch error:badarg -> {error, {badarg, Str}}
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Validate integer
%% @end
-spec integer_validate(orddict:orddict(), string()) -> {ok, integer()} | error.
%%------------------------------------------------------------------------------
integer_validate(Ctx, Key) ->
  Str = orddict:fetch(Key, Ctx),
  try {ok, list_to_integer(Str)}
  catch error:badarg -> {error, {badarg, Str}}
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Validate a non-negative integer
%% @end
-spec non_neg_integer_validate(orddict:orddict(), string()) ->
                                  {ok, integer()} | error.
%%------------------------------------------------------------------------------
non_neg_integer_validate(Ctx, Key) ->
  case integer_validate(Ctx, Key) of
    {ok, Int} when Int >= 0 -> {ok, Int};
    {ok, Int}               -> {error, {illegal, integer_to_list(Int)}};
    Err                     -> Err
  end.


%%------------------------------------------------------------------------------
%% @doc
%% Validate and convert a list of directory names.
%% @end
-spec dirs_validate(orddict:orddict(), string()) -> {ok, file:filename()}.
%%------------------------------------------------------------------------------
dirs_validate(Ctx, QsKey) ->
  case orddict:find(QsKey, Ctx) of
    error                     -> {ok, []};
    {ok, [[_|_]|_] = Strings} -> {ok, Strings}
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Validate a list where every element is part of an enumeration.
%% @end
-spec enum_list_validate(orddict:orddict(), [{atom(), term()}]) ->
                            {ok,  atom()} |
                            {error, {illegal, string()}} |
                            {error, {not_found, atom()}}.
%%------------------------------------------------------------------------------
enum_list_validate(Ctx, Props) ->
  {ok, Name}   = edts_util:assoc(name,     Props),
  {ok, Allowed} = edts_util:assoc(allowed,  Props),
  Required      = edts_util:assoc(required, Props, false),
  case orddict:find(Name, Ctx) of
    error when Required -> {error, {not_found, Name}};
    error               -> {ok, edts_util:assoc(default, Props, [])};
    {ok, Strs}          ->
      Vals = [list_to_atom(string:strip(Mod, both)) || Mod <- Strs],
      Filter = fun(Val) -> lists:member(Val, Allowed) end,
      case lists:partition(Filter, Vals) of
        {_, []} -> {ok, Vals};
        {_, Invalid} ->
          edts_log:debug("resource_exists failed: ~p", [Invalid]),
          {error, {illegal, Invalid}}
      end
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Validate an enumeration
%% @end
-spec enum_validate(orddict:orddict(),
                    [{atom(), term()}]) ->
                       {ok,  atom()} |
                       {error, {illegal, string()}} |
                       {error, {not_found, atom()}}.
%%------------------------------------------------------------------------------
enum_validate(Ctx, Props) ->
  {ok, Name}   = edts_util:assoc(name,     Props),
  {ok, Allowed} = edts_util:assoc(allowed,  Props),
  Required      = edts_util:assoc(required, Props, false),
  case orddict:find(Name, Ctx) of
    error when Required -> {error, {not_found, Name}};
    error               ->
      {ok, edts_util:assoc(default, Props, undefined)};
    {ok, V}                       ->
      Atom = list_to_atom(V),
      case lists:member(Atom, Allowed) of
        true  -> {ok, Atom};
        false -> {error, {illegal, V}}
      end
  end.


%%------------------------------------------------------------------------------
%% @doc
%% Validate path to a file.
%% @end
-spec file_validate(orddict:orddict()) ->
                       {ok, filename:filename()} |
                       {error, {no_exists, string()}}.
%%------------------------------------------------------------------------------
file_validate(Ctx) ->
  file_validate(Ctx, file).

file_validate(Ctx, Key) ->
  File = orddict:fetch(Key, Ctx),
  do_file_validate(File).

%%------------------------------------------------------------------------------
%% @doc
%% Validate a list of paths.
%% @end
-spec files_validate(orddict:orddict()) ->
                        {ok, filename:filename()} |
                        {error, {no_exists, string()}}.
%%------------------------------------------------------------------------------
files_validate(Ctx) ->
  case orddict:find(files, Ctx) of
    error                               -> {ok, all};
    {ok, "all"}                         -> {ok, all};
    {ok, [F|_] = Files} when is_list(F) ->
      NotExistsP =
        fun(File) ->
            %% Filter out the files that don't exist.
            case do_file_validate(File) of
              {error, _} -> true;
              {ok, _}    -> false
            end
        end,
      case lists:filter(NotExistsP, Files) of
        []                  -> {ok, Files};
        [_|_] = NonExisting -> {error ,{no_exists, NonExisting}}
      end
  end.


do_file_validate(File) ->
  case filelib:is_file(File) of
    true  -> {ok, File};
    false -> {error, {no_exists, File}}
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Validate function
%% @end
-spec function_validate(orddict:orddict()) -> {ok, atom()}.
%%------------------------------------------------------------------------------
function_validate(Ctx) ->
  {ok, list_to_atom(orddict:fetch(function, Ctx))}.


%%------------------------------------------------------------------------------
%% @doc
%% Validate module
%% @end
-spec module_validate(orddict:orddict()) -> {ok, module()}.
%%------------------------------------------------------------------------------
module_validate(Ctx) ->
  {ok, list_to_atom(orddict:fetch(module, Ctx))}.


%%------------------------------------------------------------------------------
%% @doc
%% Validate a list of modules.
%% @end
-spec modules_validate(orddict:orddict()) ->
                          {ok, filename:filename()} |
                          {error, {no_exists, string()}}.
%%------------------------------------------------------------------------------
modules_validate(Ctx) ->
  case orddict:find(modules, Ctx) of
    error       -> {ok, all};
    {ok, "all"} -> {ok, all};
    {ok, Val}   -> {ok, [list_to_atom(Mod) || Mod <- Val]}
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Check that module exists on the relevant node.
%% @end
-spec module_exists_p(orddict:orddict()) -> boolean().
%%------------------------------------------------------------------------------
module_exists_p(Ctx) ->
  Nodename = orddict:fetch(nodename, Ctx),
  case edts_dist:call(Nodename, code, which, [orddict:fetch(module, Ctx)]) of
    non_existing            -> false;
    File when is_list(File) -> true
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Check that all of a list of modules exist on the relevant node.
%% @end
-spec modules_exists_p(orddict:orddict()) -> boolean().
%%------------------------------------------------------------------------------
modules_exists_p(Ctx) ->
  case orddict:fetch(modules, Ctx) of
    all     -> true;
    Modules ->
      Nodename = orddict:fetch(nodename, Ctx),
      Loaded = edts_dist:call(Nodename, code, all_loaded, []),
      lists:all(fun(Mod) -> lists:keymember(Mod, 1, Loaded) end, Modules)
  end.


%%------------------------------------------------------------------------------
%% @doc
%% Validate nodename
%% @end
-spec nodename_validate(orddict:orddict()) -> {ok, node()}.
%%------------------------------------------------------------------------------
nodename_validate(Ctx) ->
  {ok, edts_util:make_nodename(orddict:fetch(nodename, Ctx))}.

%%------------------------------------------------------------------------------
%% @doc
%% Validate nodename
%% @end
-spec process_validate(orddict:orddict()) -> {ok, pid()}.
%%------------------------------------------------------------------------------
process_validate(Ctx) ->
  ProcessStr = orddict:fetch(process, Ctx),
  case catch(erlang:list_to_pid("<" ++ ProcessStr ++ ">")) of
    {'EXIT', {badarg, _}} -> {error, {badarg, ProcessStr}};
    Pid when is_pid(Pid)  -> {ok, Pid}
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Validate nodename
%% @end
-spec nodename_exists_p(orddict:orddict()) -> boolean().
%%------------------------------------------------------------------------------
nodename_exists_p(Ctx) ->
  edts:node_reachable(orddict:fetch(nodename, Ctx)).

%%------------------------------------------------------------------------------
%% @doc
%% Validate path to a project root directory
%% @end
-spec project_root_validate(orddict:orddict()) ->
                               {ok, file:filename()} |
                               {error, {not_dir, string()}}.
%%------------------------------------------------------------------------------
project_root_validate(Ctx) ->
  case orddict:find(project_root, Ctx) of
    error ->
      {ok, ""};
    {ok, Root} ->
      case filelib:is_dir(Root) of
        true  -> {ok, Root};
        false -> {error, {not_dir, Root}}
      end
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Validate a string
%% @end
-spec string_validate(orddict:orddict(), string()) -> {ok, string()}.
%%------------------------------------------------------------------------------
string_validate(Ctx, Key, Default) ->
  case string_validate(Ctx, Key) of
    {ok, String}      -> {ok, String};
    {error, notfound} -> {ok, Default}
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Validate a string
%% @end
-spec string_validate(orddict:orddict(), string(), term()) -> term().
%%------------------------------------------------------------------------------
string_validate(Ctx, Key) ->
  case orddict:find(Key, Ctx) of
    error        -> {error, notfound};
    {ok, String} -> {ok, String}
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Validate a list of strings
%% @end
-spec strings_validate(orddict:orddict(), string()) -> {ok, string()}.
%%------------------------------------------------------------------------------
strings_validate(Ctx, Key) ->
  case orddict:find(Key, Ctx) of
    {ok, Strings} when is_list(Strings) ->
      case lists:filter(fun(S) -> not is_list(S) end, Strings) of
        []         -> {ok, Strings};
        NotStrings -> {error, {not_strings, NotStrings}}
      end;
    {ok, Value} ->
      {error, {not_list, Value}};
    error ->
      {error, notfound}
  end.

%%%_* Unit tests ===============================================================

arity_validate_test() ->
  CtxF = fun(A) -> orddict:from_list([{arity, A}]) end,
  ?assertEqual({ok, 0}, arity_validate(CtxF("0"))),
  ?assertEqual({ok, 1}, arity_validate(CtxF("1"))),
  ?assertEqual(error, arity_validate(CtxF("-1"))),
  ?assertEqual({error, {badarg, "a"}}, arity_validate(CtxF("a"))).

string_validate_test() ->
  ?assertEqual({error, notfound}, string_validate(orddict:new(), s)),
  Ctx = orddict:from_list([{s, "a_string"}]),
  ?assertEqual({ok, "a_string"}, string_validate(Ctx, s)).

integer_validate_test() ->
  CtxF = fun(A) -> orddict:from_list([{integer, A}]) end,
  ?assertEqual({ok, 0}, integer_validate(CtxF("0"), integer)),
  ?assertEqual({ok, 1}, integer_validate(CtxF("1"), integer)),
  ?assertEqual({ok, -1}, integer_validate(CtxF("-1"), integer)),
  ?assertEqual({error, {badarg, "a"}}, integer_validate(CtxF("a"), integer)).

non_neg_integer_validate_test() ->
  CtxF = fun(A) -> orddict:from_list([{integer, A}]) end,
  ?assertEqual({ok, 0}, non_neg_integer_validate(CtxF("0"), integer)),
  ?assertEqual({ok, 1}, non_neg_integer_validate(CtxF("1"), integer)),
  ?assertEqual({error, {illegal, "-1"}},
               non_neg_integer_validate(CtxF("-1"), integer)),
  ?assertEqual({error, {badarg, "a"}},
               non_neg_integer_validate(CtxF("a"), integer)).

file_validate_test() ->
  CtxF = fun(A) -> orddict:from_list([{file, A}]) end,
  {ok, Cwd} = file:get_cwd(),
  ?assertEqual({ok, Cwd}, file_validate(CtxF(Cwd))),
  Filename = filename:join(Cwd, "asotehu"),
  ?assertEqual({error, {no_exists, Filename}}, file_validate(CtxF(Filename))),

  ?assertError(function_clause, file_validate(CtxF(Filename), f)),
  ?assertEqual({ok, Cwd}, file_validate(orddict:from_list([{f, Cwd}]), f)).

files_validate_test() ->
  {ok, Cwd} = file:get_cwd(),
  CtxF = fun(A) -> orddict:from_list([{files, [A, A]}]) end,
  ?assertEqual({ok, [Cwd, Cwd]}, files_validate(CtxF(Cwd))),
  Filename = filename:join(Cwd, "asotehu"),
  ?assertEqual({error, {no_exists, [Filename, Filename]}},
               files_validate(CtxF(Filename))).

function_validate_test() ->
  ?assertEqual({ok, foo}, function_validate([{function, "foo"}])).

dirs_validate_validate_test() ->
  {ok, Cwd} = file:get_cwd(),
  LibDir = filename:basename(Cwd),
  Dirs = orddict:from_list([{project_lib_dirs, [LibDir, LibDir]}]),
  ?assertEqual({ok, [LibDir, LibDir]},
               dirs_validate(Dirs,
                             project_lib_dirs)),
  ?assertEqual({ok, []}, dirs_validate([], project_lib_dirs)).

module_validate_test() ->
  ?assertEqual({ok, foo},
               module_validate(orddict:from_list([{module, "foo"}]))).

modules_validate_test() ->
  Ctx = orddict:from_list([{modules, ["foo","bar"]}]),
  ?assertEqual({ok, [foo, bar]}, modules_validate(Ctx)),
  ?assertEqual({ok, all}, modules_validate(orddict:new())).

nodename_validate_test() ->
  [_Name, Hostname] = string:tokens(atom_to_list(node()), "@"),
  Ctx = orddict:from_list([{nodename, "foo"}]),
  ?assertEqual( {ok, list_to_atom("foo@" ++ Hostname)}
              , nodename_validate(Ctx)).

project_root_validate_test() ->
  {ok, Cwd} = file:get_cwd(),

  Ctx1 = orddict:from_list([{project_root, Cwd}]),
  ?assertEqual({ok, Cwd}, project_root_validate(Ctx1)),

  Path = filename:join(Cwd, "asotehu"),
  Ctx2 = orddict:from_list([{project_root, Path}]),
  ?assertEqual({error, {not_dir, Path}},
               project_root_validate(Ctx2)).

enum_validate_test() ->
  %% Value found
  Ctx0 = orddict:from_list([{a, "a"}]),
  ?assertEqual({ok, a}, enum_validate(Ctx0, [{name,     a},
                                             {allowed,  [a, b]},
                                             {default,  a},
                                             {required, false}])),

  %% Value not found, expect default
  Ctx1 = orddict:new(),
  ?assertEqual({ok, b}, enum_validate(Ctx1, [{name,     a},
                                             {allowed,  [a, b]},
                                             {default,  b},
                                             {required, false}])),

  %% Required value not found
  ?assertEqual({error, {not_found, a}},
               enum_validate(Ctx1, [{name,     a},
                                    {allowed,  [a, b]},
                                    {default,  b},
                                    {required, true}])),

  %% Illegal value
  Ctx2 = orddict:from_list([{a, "c"}]),
  ?assertEqual({error, {illegal, "c"}},
               enum_validate(Ctx2, [{name,     a},
                                    {allowed,  [a, b]},
                                    {default,  b},
                                    {required, true}])).

enum_list_validate_test() ->
  %% Value found
  Ctx0 = orddict:from_list([{valid, ["valid_1"]}]),
  ?assertEqual({ok, [valid_1]},
               enum_list_validate(Ctx0,
                                  [{name,     valid},
                                   {allowed,  [valid_1, valid_2]},
                                   {default,  [valid_2]},
                                   {required, false}])),
  %% Value not found, expect default
  Ctx1 = orddict:new(),
  ?assertEqual({ok, [valid_2]},
               enum_list_validate(Ctx1,
                                  [{name,     valid},
                                   {allowed,  [valid_1, valid_2]},
                                   {default,  [valid_2]},
                                   {required, false}])),

  %% Value not found and no default given, expect []
  ?assertEqual({ok, []}, enum_list_validate(Ctx1,
                                            [{name,     undefined},
                                             {allowed,  [valid_1, valid_2]},
                                             {required, false}])),

  %% Required value not found
  ?assertEqual({error, {not_found, valid}},
               enum_list_validate(Ctx1, [{name,     valid},
                                         {allowed,  [valid_1, valid_2]},
                                         {required, true}])),

  %% One illegal value
  Ctx2 = orddict:from_list([{one_invalid, ["valid_1","invalid"]}]),
  ?assertEqual({error, {illegal, [invalid]}},
               enum_list_validate(Ctx2,
                                  [{name,     one_invalid},
                                   {allowed,  [valid_1, valid_2]},
                                   {default,  [valid_1]},
                                   {required, true}])).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
