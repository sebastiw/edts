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

-include("logger.hrl").

%%%_* Defines ==================================================================

-record(enum_value,
        {name :: atom(),
         allowed_values = [] :: [atom()],
         default_value :: atom()}).

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
-spec exists_p(orddict:orddict(), [atom()]) -> boolean().
%%------------------------------------------------------------------------------
exists_p(Ctx, Keys) ->
  F = fun(Key) -> (atom_to_exists_p(Key))(Ctx) end,
  case lists:partition(F, Keys) of
    {_, []}       -> true;
    {_, NoExists} ->
      ?LOG_DEBUG("resource_exists failed: ~p", [NoExists]),
      false
  end.


%%------------------------------------------------------------------------------
%% @doc
%% Validate Ctx0 and convert values to internal representation.
%% @end
-spec validate(orddict:orddict(), [atom()]) ->
        {ok, orddict:orddict()} | {error, term()}.
%%------------------------------------------------------------------------------
validate(Ctx0, Keys) ->
  try
    {ok, lists:foldl(fun validate_key/2, Ctx0, Keys)}
  catch
    throw:{error, Key0, Rsn} ->
      Key = case Key0 of
              Key1          when is_atom(Key1) -> atom_to_list(Key1);
              {_Type, Key1} when is_atom(Key1) -> Key1;
              {_Type, Props} ->
                {ok, N} = edts_util:assoc(name, Props),
                N
            end,
      Value = orddict:find(Key, Ctx0),
      ?LOG_DEBUG("Invalid Request, ~nKey: ~p~nValue: ~p", [Key, Value]),
      {error, {Key0, Rsn}}
  end.

validate_key(Key, Ctx) ->
  case (term_to_validate(Key))(Ctx) of
    {ok, Value} ->
      orddict:store(Key, Value, Ctx);
    {error, Rsn} ->
      ?LOG_ERROR("API input validation failed. Key ~p, Rsn: ~p",
                 [Key, Rsn]),
      throw({error, Key, Rsn})
  end.

%%%_* Internal functions =======================================================
atom_to_exists_p(nodename) -> fun nodename_exists_p/1;
atom_to_exists_p(module)   -> fun module_exists_p/1;
atom_to_exists_p(modules)  -> fun modules_exists_p/1.


term_to_validate(app_include_dirs) ->
  fun(Ctx) -> dirs_validate(Ctx, app_include_dirs) end;
term_to_validate(arity) ->
  fun arity_validate/1;
term_to_validate(code) ->
  fun(Ctx) -> string_validate(Ctx, code) end;
term_to_validate(string) ->
  fun(Ctx) -> string_validate(Ctx, string) end;
term_to_validate(indent) ->
  fun(Ctx) -> non_neg_integer_validate(Ctx, indent) end;
term_to_validate(max_column) ->
  fun(Ctx) -> non_neg_integer_validate(Ctx, max_column) end;
term_to_validate(erlang_cookie) ->
  fun(Ctx) -> string_validate(Ctx, erlang_cookie, undefined) end;
term_to_validate(expressions) ->
  fun(Ctx) -> strings_validate(Ctx, expressions) end;
term_to_validate(file) ->
  fun file_validate/1;
term_to_validate(function) ->
  fun function_validate/1;
term_to_validate(info_level) ->
  fun(Ctx) ->
      enum_validate(Ctx, #enum_value{name = info_level,
                                     allowed_values = [basic, detailed],
                                     default_value = basic})
  end;
term_to_validate(module) ->
  fun module_validate/1;
term_to_validate(nodename) ->
  fun nodename_validate/1;
term_to_validate(project_name) ->
  fun(Ctx) -> string_validate(Ctx, project_name, "") end;
term_to_validate(project_root) ->
  fun project_root_validate/1;
term_to_validate(project_include_dirs) ->
  fun(Ctx) -> dirs_validate(Ctx, project_include_dirs) end;
term_to_validate(project_lib_dirs) ->
  fun(Ctx) -> dirs_validate(Ctx, project_lib_dirs) end.

%%------------------------------------------------------------------------------
%% @doc
%% Validate arity
%% @end
-spec arity_validate(orddict:orddict()) ->
        {ok, non_neg_integer()} |
        {error, {badarg, string()}} |
        {error, notfound}.
%%------------------------------------------------------------------------------
arity_validate(Ctx) ->
  case orddict:find(arity, Ctx) of
    error ->
      {error, notfound};
    {ok, Str} ->
      try list_to_integer(Str) of
        Arity when Arity >= 0 ->
          {ok, Arity};
        _ ->
          {error, {badarg, Str}}
      catch error:badarg ->
          {error, {badarg, Str}}
      end
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Validate integer
%% @end
-spec integer_validate(orddict:orddict(), atom()) ->
        {ok, integer()} |
        {error, {badarg, string()}}.
%%------------------------------------------------------------------------------
integer_validate(Ctx, Key) ->
  case orddict:find(Key, Ctx) of
    error ->
      {error, notfound};
    {ok, Str} ->
      try {ok, list_to_integer(Str)}
      catch error:badarg -> {error, {badarg, Str}}
      end
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Validate a non-negative integer
%% @end
-spec non_neg_integer_validate(orddict:orddict(), atom()) ->
                                  {ok, integer()} | {error, term()}.
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
-spec dirs_validate(orddict:orddict(), atom()) -> {ok, [file:filename()]}.
%%------------------------------------------------------------------------------
dirs_validate(Ctx, QsKey) ->
  case orddict:find(QsKey, Ctx) of
    error                     -> {ok, []};
    {ok, [[_|_]|_] = Strings} -> {ok, Strings}
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Validate an enumeration
%% @end
-spec enum_validate(orddict:orddict(),
                    #enum_value{}) ->
                       {ok,  atom()} |
                       {error, {illegal, string()}} |
                       {error, {not_found, atom()}}.
%%------------------------------------------------------------------------------
enum_validate(Ctx, Props) ->
  Name = Props#enum_value.name,
  Allowed = Props#enum_value.allowed_values,
  case orddict:find(Name, Ctx) of
    error ->
      {ok, Props#enum_value.default_value};
    {ok, V} ->
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
        {ok, file:filename()} |
        {error, {no_exists, string()}} |
        {error, notfound}.
%%------------------------------------------------------------------------------
file_validate(Ctx) ->
  case orddict:find(file, Ctx) of
    error -> {error, notfound};
    {ok, File} -> do_file_validate(File)
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
    case orddict:find(function, Ctx) of
        error ->
            {error, notfound};
        {ok, F} ->
            {ok, list_to_atom(F)}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Validate module
%% @end
-spec module_validate(orddict:orddict()) -> {ok, module()}.
%%------------------------------------------------------------------------------
module_validate(Ctx) ->
    case orddict:find(module, Ctx) of
        error ->
            {error, notfound};
        {ok, M} ->
            {ok, list_to_atom(M)}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Check that module exists on the relevant node.
%% @end
-spec module_exists_p(orddict:orddict()) -> boolean().
%%------------------------------------------------------------------------------
module_exists_p(Ctx) ->
    case orddict:find(nodename, Ctx) of
        error ->
            false;
        {ok, Nodename} ->
            case edts_dist:call(Nodename, code, which, [orddict:fetch(module, Ctx)]) of
                non_existing            -> false;
                File when is_list(File) -> true
            end
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Check that all of a list of modules exist on the relevant node.
%% @end
-spec modules_exists_p(orddict:orddict()) -> boolean().
%%------------------------------------------------------------------------------
modules_exists_p(Ctx) ->
  case orddict:find(modules, Ctx) of
      error -> false;
      {ok, all} -> true;
      {ok, Modules} ->
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
%% Validate a string with default
%% @end
-spec string_validate(orddict:orddict(), atom(), term()) -> {ok, term()}.
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
-spec string_validate(orddict:orddict(), atom()) ->
        {error, notfound} |
        {ok, string()}.
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
-spec strings_validate(orddict:orddict(), atom()) ->
        {ok, string()} |
        {error, {not_strings, list(term())}} |
        {error, {not_list, term()}} |
        {error, notfound}.
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

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
