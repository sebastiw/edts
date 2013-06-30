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
-export([ exists_p/3
        , encode_debugger_info/1
        , make_nodename/1
        , validate/3]).

%%%_* Includes =================================================================
-include_lib("tulib/include/prelude.hrl").

-include_lib("eunit/include/eunit.hrl").

%%%_* Defines ==================================================================

%%%_* Types ====================================================================

%%%_* API ======================================================================


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
%% Fixme, should not be _p.
%% @end
-spec validate(wrq:req_data(), orddict:orddict(), [atom]) ->
               {boolean(), wrq:req_data(), orddict:orddict()}.
%%------------------------------------------------------------------------------
validate(ReqData0, Ctx0, Keys) ->
  F = fun(Key, {ReqData, Ctx}) ->
          case (term_to_validate(Key))(ReqData, Ctx) of
            {ok, Value} ->
              {ReqData, orddict:store(Key, Value, Ctx)};
            {error, Rsn} ->
              edts_log:error("API input validation failed. Key ~p, Rsn: ~p",
                             [Key, Rsn]),
              throw({error, Key})
          end
      end,
  try
    {ReqData, Ctx} = lists:foldl(F, {ReqData0, Ctx0}, Keys),
    {false, ReqData, Ctx}
  catch throw:{error, Key} = E ->
      KeyString = case Key of
                    {_Type, KeyAtom} -> atom_to_list(KeyAtom);
                    KeyAtom          -> atom_to_list(KeyAtom)
                  end,
      edts_log:debug("Invalid Request, ~nKey: ~p~nValue: ~p",
                     [Key, wrq:get_qs_value(KeyString, ReqData0)]),
      {true, ReqData0, orddict:store(error, E, Ctx0)}
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Try to construct a node sname from a string.
%% @end
-spec make_nodename(string()) -> node().
%%------------------------------------------------------------------------------
make_nodename(NameStr) ->
  [_Name, Host] = string:tokens(atom_to_list(node()), "@"),
  list_to_atom(hd(string:tokens(NameStr, "@")) ++ "@" ++ Host).

%%%_* Internal functions =======================================================
atom_to_exists_p(nodename) -> fun nodename_exists_p/2;
atom_to_exists_p(module)   -> fun module_exists_p/2;
atom_to_exists_p(modules)  -> fun modules_exists_p/2.


term_to_validate(app_include_dirs) ->
  fun(RD, _Ctx) -> dirs_validate(RD, "app_include_dirs") end;
term_to_validate(arity)                -> fun arity_validate/2;
term_to_validate(cmd)                  -> fun cmd_validate/2;
term_to_validate(exported)             -> fun exported_validate/2;
term_to_validate(file)                 -> fun file_validate/2;
term_to_validate({file, Key})          ->
  fun(ReqData, Ctx) -> file_validate(ReqData, Ctx, ?a2l(Key)) end;
term_to_validate(files)                -> fun files_validate/2;
term_to_validate(function)             -> fun function_validate/2;
term_to_validate(info_level)           -> fun info_level_validate/2;
term_to_validate(interpret)            ->
  fun(RD, _Ctx) -> boolean_validate(RD, "interpret") end;
term_to_validate(line) ->
  fun(RD, _Ctx) -> non_neg_integer_validate(RD, "line") end;
term_to_validate(module)               -> fun module_validate/2;
term_to_validate(modules)              -> fun modules_validate/2;
term_to_validate(nodename)             -> fun nodename_validate/2;
term_to_validate(project_name)         ->
  fun(RD, _Ctx) -> string_validate(RD, "project_name") end;
term_to_validate(project_root)         -> fun project_root_validate/2;
term_to_validate(project_include_dirs) ->
  fun(RD, _Ctx) -> dirs_validate(RD, "project_include_dirs") end;
term_to_validate(project_lib_dirs)     ->
  fun(RD, _Ctx) -> dirs_validate(RD, "project_lib_dirs") end;
term_to_validate(xref_checks)          -> fun xref_checks_validate/2.


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
%%------------------------------------------------------------------------------n
integer_validate(ReqData, Key) ->
  Str = wrq:get_qs_value(Key, ReqData),
  try {ok, list_to_integer(Str)}
  catch error:badarg -> {error, {badarg, Str}}
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Validate a non-negative integer
%% @end
-spec non_neg_integer_validate(wrq:req_data(), string()) ->
               {ok, integer()} | error.
%%------------------------------------------------------------------------------n
non_neg_integer_validate(ReqData, Key) ->
  case integer_validate(ReqData, Key) of
    {ok, Int} when Int =< 0 -> {ok, Int};
    {ok, Int}               -> {error, {illegal, integer_to_list(Int)}};
    Err                     -> Err
  end.


%%------------------------------------------------------------------------------
%% @doc
%% Validate a boolean parameter
%% @end
-spec boolean_validate(wrq:req_data(), string()) ->
                    {ok,  boolean()} | {error, {illegal, string()}}.
%%------------------------------------------------------------------------------
boolean_validate(ReqData, Key) ->
  case wrq:get_qs_value(Key, ReqData) of
    undefined -> {ok, false};
    "false"   -> {ok, false};
    "true"    -> {ok, true};
    V         -> {error, {illegal, V}}
  end.


%%------------------------------------------------------------------------------
%% @doc
%% Validate debugger command
%% @end
-spec cmd_validate(wrq:req_data(), orddict:orddict()) ->
                      {ok, atom()} | error.
%%------------------------------------------------------------------------------
cmd_validate(ReqData, _Ctx) ->
  case wrq:get_qs_value("cmd", ReqData) of
    undefined         -> error;
    L when is_list(L) -> {ok, list_to_atom(L)}
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
%% Validate export parameter
%% @end
-spec exported_validate(wrq:req_data(), orddict:orddict()) ->
                    {ok,  boolean() | all} | {error, {illegal, string()}}.
%%------------------------------------------------------------------------------
exported_validate(ReqData, _Ctx) ->
  case wrq:get_qs_value("exported", ReqData) of
    undefined -> {ok, all};
    "all"     -> {ok, all};
    "true"    -> {ok, true};
    "false"   -> {ok, false};
    V         -> {error, {illegal, V}}
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
%% Validate info-level
%% @end
-spec info_level_validate(wrq:req_data(), orddict:orddict()) ->
                    {ok, basic | detailed} | {error, {illegal, string()}}.
%%------------------------------------------------------------------------------
info_level_validate(ReqData, _Ctx) ->
  case wrq:get_qs_value("info_level", ReqData) of
    undefined  -> {ok, basic};
    "basic"    -> {ok, basic};
    "detailed" -> {ok, detailed};
    V          -> {error, {illegal, V}}
  end.


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
  Module   = orddict:fetch(module, Ctx),
  case edts_dist:call(Nodename, Module, module_info, []) of
    {badrpc, _} -> false;
    _ -> true
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
  {ok, make_nodename(wrq:path_info(nodename, ReqData))}.

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
string_validate(ReqData, Key) ->
  case wrq:get_qs_value(Key, ReqData) of
    undefined -> {ok, ""};
    String    -> {ok, String}
  end.


%%------------------------------------------------------------------------------
%% @doc
%% Validate xref_checks
%% @end
-spec xref_checks_validate(wrq:req_data(), orddict:orddict()) ->
                              {ok, [edts_xref:xref_check()]} |
                              {error, {illegal, [atom()]}}.
%%------------------------------------------------------------------------------
xref_checks_validate(ReqData, _Ctx) ->
  case wrq:get_qs_value("xref_checks", ReqData) of
    undefined  -> {ok, [undefined_function_calls]};
    Val        ->
      Checks   = [list_to_atom(Check) || Check <- string:tokens(Val, ",")],
      IsLegalF = fun(Check) ->
                     lists:member(Check, edts_xref:allowed_checks())
                 end,
      case lists:partition(IsLegalF, Checks) of
        {Legal, []}       -> {ok, Legal};
        {_Legal, Illegal} -> {error, {illegal, Illegal}}
      end
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Encodes debugger replies into the appropriate json structure
%% @end
-spec encode_debugger_info({ok, Info :: term()}) -> term().
%%------------------------------------------------------------------------------
encode_debugger_info({ok, Info}) ->
  {struct, do_encode_debugger_info(Info)};
encode_debugger_info({error, Error}) ->
  {struct, [{state, error}, {message, Error}]}.

do_encode_debugger_info({break, File, {Module, Line}, VarBindings}) ->
  [{state, break}, {file, list_to_binary(File)},{module, Module}, {line, Line},
   {var_bindings,
    {struct, encode(VarBindings)}}];
do_encode_debugger_info([{module, _} | _] = Interpreted) ->
  [{interpreted, {array, Interpreted}}];
do_encode_debugger_info(State) ->
  [{state, State}].

encode(VarBindings) ->
  [{Key, list_to_binary(io_lib:format("~p", [Value]))}
   || {Key, Value} <- VarBindings].


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

cmd_validate_test() ->
  meck:unload(),
  meck:new(wrq),
  meck:expect(wrq, get_qs_value, fun("cmd", _) -> undefined end),
  ?assertEqual(error, cmd_validate(foo, bar)),
  meck:expect(wrq, get_qs_value, fun("cmd", _) -> "foo" end),
  ?assertEqual({ok, foo}, cmd_validate(foo, bar)),
  meck:unload().

exported_validate_test() ->
  meck:unload(),
  meck:new(wrq),
  meck:expect(wrq, get_qs_value, fun("exported", _) -> undefined end),
  ?assertEqual({ok, all}, exported_validate(foo, bar)),
  meck:expect(wrq, get_qs_value, fun("exported", _) -> "all" end),
  ?assertEqual({ok, all}, exported_validate(foo, bar)),
  meck:expect(wrq, get_qs_value, fun("exported", _) -> "true" end),
  ?assertEqual({ok, true}, exported_validate(foo, bar)),
  meck:expect(wrq, get_qs_value, fun("exported", _) -> "false" end),
  ?assertEqual({ok, false}, exported_validate(foo, bar)),
  meck:expect(wrq, get_qs_value, fun("exported", _) -> true end),
  ?assertEqual({error, {illegal, true}}, exported_validate(foo, bar)),
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

info_level_validate_test() ->
  meck:unload(),
  meck:new(wrq),
  meck:expect(wrq, get_qs_value, fun("info_level", _) -> undefined end),
  ?assertEqual({ok, basic}, info_level_validate(foo, bar)),
  meck:expect(wrq, get_qs_value, fun("info_level", _) -> "basic" end),
  ?assertEqual({ok, basic}, info_level_validate(foo, bar)),
  meck:expect(wrq, get_qs_value, fun("info_level", _) -> "detailed" end),
  ?assertEqual({ok, detailed}, info_level_validate(foo, bar)),
  meck:expect(wrq, get_qs_value, fun("info_level", _) -> true end),
  ?assertEqual({error, {illegal, true}}, info_level_validate(foo, bar)),
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

xref_checks_validate_test() ->
  meck:unload(),
  meck:new(wrq),
  meck:expect(wrq, get_qs_value, fun("xref_checks", _) -> undefined end),
  ?assertEqual({ok, [undefined_function_calls]},
               xref_checks_validate(foo, bar)),
  meck:expect(wrq, get_qs_value,
              fun("xref_checks", _) -> "undefined_function_calls" end),
  ?assertEqual({ok, [undefined_function_calls]},
               xref_checks_validate(foo, bar)),
  meck:expect(wrq, get_qs_value, fun("xref_checks", _) -> "something" end),
  ?assertEqual({error, {illegal, [something]}}, xref_checks_validate(foo, bar)),
  meck:expect(wrq, get_qs_value, fun("xref_checks", _) ->
                                     "something,undefined_function_calls" end),
  ?assertEqual({error, {illegal, [something]}}, xref_checks_validate(foo, bar)),
  meck:expect(wrq, get_qs_value,
              fun("xref_checks", _) ->
                  "undefined_function_calls,unused_exports"
              end),
  ?assertEqual({ok, [undefined_function_calls,unused_exports]},
               xref_checks_validate(foo, bar)),
  meck:unload().

encode_debugger_info_test() ->
  ?assertEqual({struct, [{state, error}, {message, foo}]},
               encode_debugger_info({error, foo})),
  ?assertEqual({struct, [ {state, break}
                        , {file, <<"/awsum/foo.erl">>}
                        , {module, foo}
                        , {line, 42}
                        , {var_bindings, {struct, []}}]},
               encode_debugger_info({ok, {break, "/awsum/foo.erl", {foo, 42},
                                          []}})),
  ?assertEqual({struct, [ {state, break}
                        , {file, <<"/awsum/bar.erl">>}
                        , {module, bar}
                        , {line, 123}
                        , {var_bindings, {struct, [{'A', <<"3.14">>}]}}]},
               encode_debugger_info({ok, {break, "/awsum/bar.erl", {bar, 123},
                                          [{'A', 3.14}]}})).

encode_test() ->
  ?assertEqual([{'A', <<"\"foo\"">>}], encode([{'A', "foo"}])),
  ?assertEqual([{"bar", <<"\"BAZ\"">>}], encode([{"bar", [$B, $A, $Z]}])),
  ?assertEqual([{'foo', <<"bar">>}, {"pi", <<"3.14">>}],
               encode([{'foo', bar}, {"pi", 3.14}])),
  ?assertEqual([{a_tuple, <<"{with,3,\"fields\"}">>}],
               encode([{a_tuple, {with, 3, "fields"}}])).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
