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
-module(edts_resource_lib).

%%%_* Exports ==================================================================

%% Application callbacks
-export([ check_exists_and_do_rpc/4,
          exists_p/3,
          validate/3]).

%%%_* Includes =================================================================

-include_lib("eunit/include/eunit.hrl").

%%%_* Defines ==================================================================

-define(a2l(A), atom_to_list(A)).

%%%_* Types ====================================================================

%%%_* API ======================================================================

check_exists_and_do_rpc(ReqData, Ctx, Required, {M, F, ArgKeys}) ->
  case exists_p(ReqData, Ctx, [nodename|Required]) of
    false -> {false, ReqData, Ctx};
    true  ->
      Node = orddict:fetch(nodename, Ctx),
      case edts:call(Node, M, F, [orddict:fetch(Key, Ctx) || Key <- ArgKeys]) of
        {ok, Result} -> {true, ReqData, orddict:store(result, Result, Ctx)};
        {error, _}   -> {false, ReqData, Ctx}
      end
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Check that all elements of resource_exist.
%% @end
-spec exists_p(wrq:req_data(), orddict:orddict(), [atom]) ->
               {boolean(), wrq:req_data(), orddict:orddict()}.
%%------------------------------------------------------------------------------
exists_p(ReqData, Ctx, Keys) ->
  F = fun(Key) -> (atom_to_exists_p(Key))(ReqData, Ctx) end,
  case lists:partition(F, Keys) of
    {_, []}       -> true;
    {_, NoExists} ->
      edts_log:debug("resource_exists failed: ~p", [NoExists]),
      false
  end.


%%------------------------------------------------------------------------------
%% @doc
%% Validate ReqData and convert values to internal representation.
%% @end
-spec validate(wrq:req_data(), orddict:orddict(), [atom]) ->
               {boolean(), wrq:req_data(), orddict:orddict()}.
%%------------------------------------------------------------------------------
validate(ReqData0, Ctx0, Keys) ->
  F = fun(Key, {ReqData, Ctx}) ->
          Name = case Key of
                   Key when is_atom(Key) -> Key;
                   {_Type, Props}        ->
                     {ok, N} = edts_util:assoc(name, Props),
                     N
                 end,
          case (term_to_validate(Key))(ReqData, Ctx) of
            {ok, Value} ->
              {ReqData, orddict:store(Name, Value, Ctx)};
            {error, Rsn} ->
              edts_log:error("API input validation failed. Key ~p, Rsn: ~p",
                             [Key, Rsn]),
              throw({error, Key})
          end
      end,
  try
    {ReqData, Ctx} = lists:foldl(F, {ReqData0, Ctx0}, Keys),
    {false, ReqData, Ctx}
  catch throw:{error, Key0} = E ->
      Key = case Key0 of
              Key1          when is_atom(Key1) -> atom_to_list(Key1);
              {_Type, Key1} when is_atom(Key1) -> Key1;
              {_Type, Props} ->
                {ok, N} = edts_util:assoc(name, Props),
                N
            end,
      Value = wrq:get_qs_value(atom_to_list(Key), ReqData0),
      edts_log:debug("Invalid Request, ~nKey: ~p~nValue: ~p", [Key, Value]),
      {true, ReqData0, orddict:store(error, E, Ctx0)}
  end.

%%%_* Internal functions =======================================================
atom_to_exists_p(nodename) -> fun nodename_exists_p/2;
atom_to_exists_p(module)   -> fun module_exists_p/2;
atom_to_exists_p(modules)  -> fun modules_exists_p/2.


term_to_validate(app_include_dirs) ->
  fun(RD, _Ctx) -> dirs_validate(RD, "app_include_dirs") end;
term_to_validate(arity)                -> fun arity_validate/2;
term_to_validate({enum_list, Props})   ->
  fun(RD, _Ctx) -> enum_list_validate(RD, Props) end;
term_to_validate({enum, Props})        ->
  fun(RD, _Ctx) -> enum_validate(RD, Props) end;
term_to_validate(erlang_cookie) ->
  fun(RD, _Ctx) -> string_validate(RD, "erlang_cookie", undefined) end;
term_to_validate(exported)             ->
  fun(ReqData, _Ctx) ->
      enum_validate(ReqData, [{name,    exported},
                              {allowed, [true, false, all]},
                              {default, all},
                              {required, false}])
  end;
term_to_validate(file)                 -> fun file_validate/2;
term_to_validate({file, Key})          ->
  fun(ReqData, Ctx) -> file_validate(ReqData, Ctx, ?a2l(Key)) end;
term_to_validate(files)                -> fun files_validate/2;
term_to_validate(function)             -> fun function_validate/2;
term_to_validate(info_level)           ->
  fun(ReqData, _Ctx) ->
      enum_validate(ReqData, [{name,     info_level},
                              {allowed,  [basic, detailed]},
                              {default,  basic},
                              {required, false}])
  end;
term_to_validate(line) ->
  fun(RD, _Ctx) -> non_neg_integer_validate(RD, line) end;
term_to_validate(module)               -> fun module_validate/2;
term_to_validate(modules)              -> fun modules_validate/2;
term_to_validate(nodename)             -> fun nodename_validate/2;
term_to_validate(process)              -> fun process_validate/2;
term_to_validate(project_name)         ->
  fun(RD, _Ctx) -> string_validate(RD, "project_name", "") end;
term_to_validate(project_root)         -> fun project_root_validate/2;
term_to_validate(project_include_dirs) ->
  fun(RD, _Ctx) -> dirs_validate(RD, "project_include_dirs") end;
term_to_validate(project_lib_dirs)     ->
  fun(RD, _Ctx) -> dirs_validate(RD, "project_lib_dirs") end.

%%------------------------------------------------------------------------------
%% @doc
%% Validate arity
%% @end
-spec arity_validate(wrq:req_data(), orddict:orddict()) ->
               {ok, non_neg_integer()} | error.
%%------------------------------------------------------------------------------
arity_validate(ReqData, _Ctx) ->
  Str = wrq:path_info(arity, ReqData),
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
-spec integer_validate(wrq:req_data(), string()) ->
               {ok, integer()} | error.
%%------------------------------------------------------------------------------
integer_validate(ReqData, Key) ->
  Str = wrq:path_info(Key, ReqData),
  try {ok, list_to_integer(Str)}
  catch error:badarg -> {error, {badarg, Str}}
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Validate a non-negative integer
%% @end
-spec non_neg_integer_validate(wrq:req_data(), string()) ->
               {ok, integer()} | error.
%%------------------------------------------------------------------------------
non_neg_integer_validate(ReqData, Key) ->
  case integer_validate(ReqData, Key) of
    {ok, Int} when Int >= 0 -> {ok, Int};
    {ok, Int}               -> {error, {illegal, integer_to_list(Int)}};
    Err                     -> Err
  end.


%%------------------------------------------------------------------------------
%% @doc
%% Validate and convert a list of directory names.
%% @end
-spec dirs_validate(wrq:req_data(), string()) -> {ok, file:filename()}.
%%------------------------------------------------------------------------------
dirs_validate(ReqData, QsKey) ->
  case wrq:get_qs_value(QsKey, ReqData) of
    undefined -> {ok, []};
    Str       -> {ok, string:tokens(Str, ",")}
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Validate a list where every element is part of an enumeration.
%% @end
-spec enum_list_validate(wrq:req_data(),
                         [{atom(), term()}]) ->
                          {ok,  atom()} |
                          {error, {illegal, string()}} |
                          {error, {not_found, atom()}}.
%%------------------------------------------------------------------------------
enum_list_validate(ReqData, Props) ->
  {ok, Name}   = edts_util:assoc(name,     Props),
  QsKey = atom_to_list(Name),
  {ok, Allowed} = edts_util:assoc(allowed,  Props),
  Required      = edts_util:assoc(required, Props, false),
  case wrq:get_qs_value(QsKey, ReqData) of
    undefined when Required -> {error, {not_found, QsKey}};
    undefined               -> {ok, edts_util:assoc(default, Props, [])};
    Str                     ->
      Tokens = string:tokens(Str, ","),
      Vals = [list_to_atom(string:strip(Mod, both)) || Mod <- Tokens],
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
-spec enum_validate(wrq:req_data(),
                    [{atom(), term()}]) ->
                    {ok,  atom()} |
                       {error, {illegal, string()}} |
                       {error, {not_found, atom()}}.
%%------------------------------------------------------------------------------
enum_validate(ReqData, Props) ->
  {ok, Name}   = edts_util:assoc(name,     Props),
  QsKey = atom_to_list(Name),
  {ok, Allowed} = edts_util:assoc(allowed,  Props),
  Required      = edts_util:assoc(required, Props, false),
  case wrq:get_qs_value(QsKey, ReqData) of
    undefined when Required -> {error, {not_found, QsKey}};
    undefined               ->
      {ok, edts_util:assoc(default, Props, undefined)};
    V                       ->
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
-spec file_validate(wrq:req_data(), orddict:orddict()) ->
                       {ok, filename:filename()} |
                       {error, {no_exists, string()}}.
%%------------------------------------------------------------------------------
file_validate(ReqData, Ctx) ->
  file_validate(ReqData, Ctx, "file").

file_validate(ReqData, _Ctx, Key) ->
  File = wrq:get_qs_value(Key, ReqData),
  do_file_validate(File).

%%------------------------------------------------------------------------------
%% @doc
%% Validate a list of paths.
%% @end
-spec files_validate(wrq:req_data(), orddict:orddict()) ->
                       {ok, filename:filename()} |
                       {error, {no_exists, string()}}.
%%------------------------------------------------------------------------------
files_validate(ReqData, _Ctx) ->
  case wrq:get_qs_value("files", ReqData) of
    undefined                     -> {ok, all};
    "all"                         -> {ok, all};
    [F|_] = Files when is_list(F) ->
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
-spec function_validate(wrq:req_data(), orddict:orddict()) -> {ok, module()}.
%%------------------------------------------------------------------------------
function_validate(ReqData, _Ctx) ->
  {ok, list_to_atom(wrq:path_info(function, ReqData))}.



%%------------------------------------------------------------------------------
%% @doc
%% Validate module
%% @end
-spec module_validate(wrq:req_data(), orddict:orddict()) -> {ok, module()}.
%%------------------------------------------------------------------------------
module_validate(ReqData, _Ctx) ->
  {ok, list_to_atom(wrq:path_info(module, ReqData))}.


%%------------------------------------------------------------------------------
%% @doc
%% Validate a list of modules.
%% @end
-spec modules_validate(wrq:req_data(), orddict:orddict()) ->
                       {ok, filename:filename()} |
                       {error, {no_exists, string()}}.
%%------------------------------------------------------------------------------
modules_validate(ReqData, _Ctx) ->
  case wrq:get_qs_value("modules", ReqData) of
    undefined -> {ok, all};
    "all"     -> {ok, all};
    Val        -> {ok, [list_to_atom(Mod) || Mod <- string:tokens(Val, ",")]}
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Check that module exists on the relevant node.
%% @end
-spec module_exists_p(wrq:req_data(), orddict:orddict()) -> boolean().
%%------------------------------------------------------------------------------
module_exists_p(_ReqData, Ctx) ->
  Nodename = orddict:fetch(nodename, Ctx),
  case edts_dist:call(Nodename, code, which, [orddict:fetch(module, Ctx)]) of
    non_existing            -> false;
    File when is_list(File) -> true
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Check that all of a list of modules exist on the relevant node.
%% @end
-spec modules_exists_p(wrq:req_data(), orddict:orddict()) -> boolean().
%%------------------------------------------------------------------------------
modules_exists_p(_ReqData, Ctx) ->
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
-spec nodename_validate(wrq:req_data(), orddict:orddict()) -> {ok, node()}.
%%------------------------------------------------------------------------------
nodename_validate(ReqData, _Ctx) ->
  {ok, edts_util:make_nodename(wrq:path_info(nodename, ReqData))}.

%%------------------------------------------------------------------------------
%% @doc
%% Validate nodename
%% @end
-spec process_validate(wrq:req_data(), orddict:orddict()) -> {ok, pid()}.
%%------------------------------------------------------------------------------
process_validate(ReqData, _Ctx) ->
  ProcessStr = wrq:path_info(process, ReqData),
  case catch(erlang:list_to_pid("<" ++ ProcessStr ++ ">")) of
    {'EXIT', {badarg, _}}    -> {error, {badarg, ProcessStr}};
    Pid when is_pid(Pid) -> {ok, Pid}
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Validate nodename
%% @end
-spec nodename_exists_p(wrq:req_data(), orddict:orddict()) -> boolean().
%%------------------------------------------------------------------------------
nodename_exists_p(_ReqData, Ctx) ->
  edts:node_reachable(orddict:fetch(nodename, Ctx)).

%%------------------------------------------------------------------------------
%% @doc
%% Validate path to a project root directory
%% @end
-spec project_root_validate(wrq:req_data(), orddict:orddict()) ->
                               {ok, file:filename()} |
                               {error, {not_dir, string()}}.
%%------------------------------------------------------------------------------
project_root_validate(ReqData, _Ctx) ->
  case wrq:get_qs_value("project_root", ReqData) of
    undefined -> {ok, ""};
    Root      ->
      case filelib:is_dir(Root) of
        true  -> {ok, Root};
        false -> {error, {not_dir, Root}}
      end
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Validate a string
%% @end
-spec string_validate(wrq:req_data(), string()) -> {ok, string()}.
%%------------------------------------------------------------------------------
string_validate(ReqData, Key, Default) ->
  case string_validate(ReqData, Key) of
    {ok, String}      -> {ok, String};
    {error, notfound} -> {ok, Default}
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Validate a string
%% @end
-spec string_validate(wrq:req_data(), string(), term()) -> term().
%%------------------------------------------------------------------------------
string_validate(ReqData, Key) ->
  case wrq:get_qs_value(Key, ReqData) of
    undefined -> {error, notfound};
    String    -> {ok, String}
  end.

%%%_* Unit tests ===============================================================

arity_validate_test() ->
  meck:unload(),
  meck:new(wrq),
  meck:expect(wrq, path_info, fun(arity, _) -> "0" end),
  ?assertEqual({ok, 0}, arity_validate(foo, bar)),
  meck:expect(wrq, path_info, fun(arity, _) -> "1" end),
  ?assertEqual({ok, 1}, arity_validate(foo, bar)),
  meck:expect(wrq, path_info, fun(arity, _) -> "-1" end),
  ?assertEqual(error, arity_validate(foo, bar)),
  meck:expect(wrq, path_info, fun(arity, _) -> "a" end),
  ?assertEqual({error, {badarg, "a"}}, arity_validate(foo, bar)),
  meck:unload().

string_validate_test() ->
  meck:unload(),
  meck:new(wrq),
  meck:expect(wrq, get_qs_value, fun(bar, _) -> undefined end),
  ?assertEqual({error, notfound}, string_validate(foo, bar)),
  meck:expect(wrq, get_qs_value, fun(bar, _) -> "a_string" end),
  ?assertEqual({ok, "a_string"}, string_validate(foo, bar)),
  meck:unload().


integer_validate_test() ->
  meck:unload(),
  meck:new(wrq),
  meck:expect(wrq, path_info, fun(bar, _) -> "0" end),
  ?assertEqual({ok, 0}, integer_validate(foo, bar)),
  meck:expect(wrq, path_info, fun(bar, _) -> "1" end),
  ?assertEqual({ok, 1}, integer_validate(foo, bar)),
  meck:expect(wrq, path_info, fun(bar, _) -> "-1" end),
  ?assertEqual({ok, -1}, integer_validate(foo, bar)),
  meck:expect(wrq, path_info, fun(bar, _) -> "a" end),
  ?assertEqual({error, {badarg, "a"}}, integer_validate(foo, bar)),
  meck:unload().

non_neg_integer_validate_test() ->
  meck:unload(),
  meck:new(wrq),
  meck:expect(wrq, path_info, fun(bar, _) -> "0" end),
  ?assertEqual({ok, 0}, non_neg_integer_validate(foo, bar)),
  meck:expect(wrq, path_info, fun(bar, _) -> "1" end),
  ?assertEqual({ok, 1}, non_neg_integer_validate(foo, bar)),
  meck:expect(wrq, path_info, fun(bar, _) -> "-1" end),
  ?assertEqual({error, {illegal, "-1"}}, non_neg_integer_validate(foo, bar)),
  meck:expect(wrq, path_info, fun(bar, _) -> "a" end),
  ?assertEqual({error, {badarg, "a"}}, non_neg_integer_validate(foo, bar)),
  meck:unload().

file_validate_test() ->
  meck:unload(),
  meck:new(wrq),
  {ok, Cwd} = file:get_cwd(),
  meck:expect(wrq, get_qs_value, fun("file", _) -> Cwd end),
  ?assertEqual({ok, Cwd}, file_validate(foo, bar)),
  Filename = filename:join(Cwd, "asotehu"),
  meck:expect(wrq, get_qs_value,
              fun("file", _) ->  Filename end),
  ?assertEqual({error, {no_exists, Filename}}, file_validate(foo, bar)),

  ?assertError(function_clause, file_validate(foo, bar, "f")),
  meck:expect(wrq, get_qs_value, fun("f", _) -> Cwd end),
  ?assertEqual({ok, Cwd}, file_validate(foo, bar, "f")),
  meck:unload().

files_validate_test() ->
  meck:unload(),
  meck:new(wrq),
  {ok, Cwd} = file:get_cwd(),
  meck:expect(wrq, get_qs_value, fun("files", _) -> [Cwd, Cwd] end),
  ?assertEqual({ok, [Cwd, Cwd]}, files_validate(foo, bar)),
  Filename = filename:join(Cwd, "asotehu"),
  meck:expect(wrq, get_qs_value,
              fun("files", _) ->  [Cwd, Filename] end),
  ?assertEqual({error, {no_exists, [Filename]}}, files_validate(foo, bar)),
  meck:unload().

function_validate_test() ->
  meck:unload(),
  meck:new(wrq),
  meck:expect(wrq, path_info, fun(function, _) -> "foo" end),
  ?assertEqual({ok, foo}, function_validate(foo, bar)),
  meck:unload().

dirs_validate_validate_test() ->
  meck:unload(),
  meck:new(wrq),
  {ok, Cwd} = file:get_cwd(),
  LibDir = filename:basename(Cwd),
  meck:expect(wrq, get_qs_value,
              fun("project_lib_dirs", _) -> LibDir ++ "," ++ LibDir end),
  ?assertEqual({ok, [LibDir, LibDir]}, dirs_validate(foo, "project_lib_dirs")),
  meck:expect(wrq, get_qs_value, fun("project_lib_dirs", _) -> undefined end),
  ?assertEqual({ok, []}, dirs_validate(foo, "project_lib_dirs")),
  meck:unload().

module_validate_test() ->
  meck:unload(),
  meck:new(wrq),
  meck:expect(wrq, path_info, fun(module, _) -> "foo" end),
  ?assertEqual({ok, foo}, module_validate(foo, bar)),
  meck:unload().

modules_validate_test() ->
  meck:unload(),
  meck:new(wrq),
  meck:expect(wrq, get_qs_value, fun("modules", _) -> "foo,bar" end),
  ?assertEqual({ok, [foo, bar]}, modules_validate(foo, bar)),
  meck:unload().

nodename_validate_test() ->
  meck:unload(),
  meck:new(wrq),
  meck:expect(wrq, path_info, fun(nodename, _) -> "foo" end),
  [_Name, Hostname] = string:tokens(atom_to_list(node()), "@"),
  ?assertEqual( {ok, list_to_atom("foo@" ++ Hostname)}
              , nodename_validate(foo, bar)),
  meck:unload().

root_validate_test() ->
  meck:unload(),
  meck:new(wrq),
  {ok, Cwd} = file:get_cwd(),
  meck:expect(wrq, get_qs_value, fun("project_root", _) -> Cwd end),
  ?assertEqual({ok, Cwd}, project_root_validate(foo, bar)),
  Path = filename:join(Cwd, "asotehu"),
  meck:expect(wrq, get_qs_value,
              fun("project_root", _) -> Path end),
  ?assertEqual({error, {not_dir, Path}}, project_root_validate(foo, bar)),
  meck:unload().

enum_validate_test_() ->
  {setup,
   fun() ->
       meck:unload(),
       meck:new(wrq),
       meck:expect(wrq, get_qs_value, fun("undefined", _) -> undefined;
                                         (Name, _) -> Name
                                      end)
   end,
   fun(_) ->
       meck:unload()
   end,
  [%% Value found
   ?_assertEqual({ok, a}, enum_validate(fake_req, [{name,     a},
                                                   {allowed,  [a, b]},
                                                   {default,  a},
                                                   {required, false}])),
   %% Value not found, expect default
   ?_assertEqual({ok, b}, enum_validate(fake_req, [{name,     undefined},
                                                   {allowed,  [a, b]},
                                                   {default,  b},
                                                   {required, false}])),
   %% Required value not found
   ?_assertEqual({error, {not_found, "undefined"}},
                  enum_validate(fake_req, [{name,     undefined},
                                           {allowed,  [a, b]},
                                           {default,  b},
                                           {required, true}])),
   %% Illegal value
   ?_assertEqual({error, {illegal, "c"}},
                 enum_validate(fake_req, [{name,     c},
                                          {allowed,  [a, b]},
                                          {default,  b},
                                          {required, true}]))
  ]}.

enum_list_validate_test_() ->
  {setup,
   fun() ->
       meck:unload(),
       meck:new(wrq),
       meck:expect(wrq, get_qs_value,
                   fun("undefined", _)     -> undefined;
                      ("one_invalid", _)   -> "valid_1, invalid";
                      ("invalid_value", _) -> "invalid";
                      ("valid", _)         -> "valid_1"
                                           end)
   end,
   fun(_) ->
       meck:unload()
   end,
  [%% Value found
   ?_assertEqual({ok, [valid_1]}, enum_list_validate(fake_req,
                                        [{name,     valid},
                                         {allowed,  [valid_1, valid_2]},
                                         {default,  [valid_2]},
                                         {required, false}])),
   %% Value not found, expect default
   ?_assertEqual({ok, [valid_2]}, enum_list_validate(fake_req,
                                             [{name,     undefined},
                                              {allowed,  [valid_1, valid_2]},
                                              {default,  [valid_2]},
                                              {required, false}])),
   %% Value not found and not default given, expect []
   ?_assertEqual({ok, []}, enum_list_validate(fake_req,
                                              [{name,     undefined},
                                               {allowed,  [valid_1, valid_2]},
                                               {required, false}])),
   %% Required value not found
   ?_assertEqual({error, {not_found, "undefined"}},
                 enum_list_validate(fake_req, [{name,     undefined},
                                               {allowed,  [valid_1, valid_2]},
                                               {required, true}])),
   %% One illegal value
   ?_assertEqual({error, {illegal, [invalid]}},
                 enum_list_validate(fake_req, [{name,     one_invalid},
                                               {allowed,  [valid_1, valid_2]},
                                               {default,  [valid_1]},
                                               {required, true}]))
  ]}.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
