%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% code-analysis library for edts. This module is loaded on the project
%%% node.
%%% @end
%%%
%%% @author Thomas Järvstrand <tjarvstrand@gmail.com>
%%% @copyright
%%% Copyright 2012-2013 Thomas Järvstrand <tjarvstrand@gmail.com>
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
-module(edts_code).

%%%_* Includes =================================================================
-include_lib("kernel/include/file.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%_* Exports ==================================================================

-export([add_paths/1,
         compile_and_load/1,
         ensure_module_loaded/2,
         free_vars/1,
         free_vars/2,
         get_function_info/3,
         get_module_info/2,
         get_module_source/1,
         init/1,
         modules/0,
         parse_expressions/1,
         project_data_dir/0,
         project_name/0,
         project_root_dir/0,
         project_specific_data_file/1,
         start/0,
         started_p/0,
         string_to_mfa/1]).

%%%_* Defines ==================================================================
-define(SERVER, ?MODULE).

%%%_* Types ====================================================================
-export_type([issue/0]).

-type issue() :: { Type        :: atom()
                 , File        :: string()
                 , Line        :: non_neg_integer()
                 , Description :: string()}.

%%%_* API ======================================================================


%%------------------------------------------------------------------------------
%% @doc
%% Add a new path to the code-path. Uniqueness is determined after shortening
%% the path using edts_util:shorten_path/1, which means symbolic links could
%% cause duplicate paths to be added.
%% @end
-spec add_path(filename:filename()) -> code:add_path_ret().
%%------------------------------------------------------------------------------
add_path(Path) -> code:add_patha(edts_util:shorten_path(Path)).


%%------------------------------------------------------------------------------
%% @doc
%% Call add_path/1 for each path in Paths.
%% @end
-spec add_paths([filename:filename()]) -> ok.
%%------------------------------------------------------------------------------
add_paths(Paths) -> lists:foreach(fun add_path/1, Paths).


%%------------------------------------------------------------------------------
%% @doc
%% Equivalent to compile_and_load(Module, []).
%% @end
-spec compile_and_load(Module::file:filename() | module()) ->
        {ok, {[], [issue()]}} |
        {error, {[issue()], [issue()]}} |
        {error, term()}.
%%------------------------------------------------------------------------------
compile_and_load(Module) ->
  update_paths(),
  compile_and_load(Module, []).


%%------------------------------------------------------------------------------
%% @doc
%% Compiles Module with Options and returns a list of any errors and warnings.
%% If there are no errors, the module will be loaded. Compilation options
%% always include 'return', and 'debug_info' (and also 'binary', but
%% the returned binary will be written to file by EDTS).
%% Any other options passed in will be appended to the above.
%% @end
-spec compile_and_load(Module::file:filename()| module(), [compile:option()]) ->
        {ok, {[], [issue()]}} |
        {error, {[issue()], [issue()]}} |
        {error, term()}.
%%------------------------------------------------------------------------------
compile_and_load(Module, Opts) when is_atom(Module)->
  File = proplists:get_value(source, Module:module_info(compile)),
  compile_and_load(File, Opts);
compile_and_load(File0, Opts) ->
  {ok, Cwd}   = file:get_cwd(),
  File = case lists:prefix(Cwd, File0) of
           true  -> lists:nthtail(length(Cwd) + 1, File0);
           false -> File0
         end,
  OutDir  = get_compile_outdir(File0),
  reload_if_modified(File0, OutDir),
  OldOpts = extract_compile_opts(File),

  AdditionalIncludes = get_additional_includes(filename:dirname(File), OldOpts),
  CompileOpts = [{cwd, Cwd},
                 {outdir, OutDir},
                 binary,
                 debug_info,
                 return|Opts] ++ OldOpts ++ AdditionalIncludes,
  %% Only compile to a binary to begin with since compile-options resulting in
  %% an output-file will cause the compile module to remove the existing beam-
  %% file even if compilation fails, in which case we end up with no module
  %% at all for other analyses (xref etc.).
  case compile:file(File, CompileOpts) of
    {ok, Mod, Bin, Warnings} ->
      OutFile = filename:join(OutDir, atom_to_list(Mod)),
      case file:write_file(OutFile ++ ".beam", Bin) of
        ok ->
          code:purge(Mod),
          {module, Mod} = code:load_abs(OutFile),
          add_path(OutDir),
          {ok, {[], format_errors(warning, Warnings)}};
        {error, _} = Err ->
          error_logger:error_msg("(~p) Failed to write ~p: ~p",
                                 [node(), OutFile, Err]),
          Err
      end;
    {error, Errors, Warnings} ->
      {error, {format_errors(error, Errors), format_errors(warning, Warnings)}}
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Returns information about Function as defined in Module.
%% @end
-spec get_function_info(M::module(), F0::atom(), A0::non_neg_integer()) ->
                           [{atom(), term()}].
%%------------------------------------------------------------------------------
get_function_info(M, F, A) ->
  ensure_module_loaded(true, M),
  {M, Bin, _File}                   = code:get_object_code(M),
  {ok, {M, Chunks}}                 = beam_lib:chunks(Bin, [abstract_code]),
  {abstract_code, {_Vsn, Abst}} = lists:keyfind(abstract_code, 1, Chunks),
  Abstract = lists:map(fun erl_parse:anno_to_term/1, Abst),
  ExportedP = lists:member({F, A}, M:module_info(exports)),
  {ok, ModSrc} = get_module_source(M, M:module_info()),
  case get_file_and_line(M, F, A, ModSrc, Abstract) of
    {error, _} = Err   ->
      case ExportedP of
        false -> Err;
        true  -> make_function_info(M, F, A, ExportedP, ModSrc, is_bif, ModSrc)
      end;
    {ok, {FunSrc, Line}} ->
      make_function_info(M, F, A, ExportedP, FunSrc, Line, ModSrc)
  end.


make_function_info(M, F, A, ExportedP, FunSrc, Line, ModSrc) ->
  %% Get rid of any local paths, in case function was defined in a
  %% file include with a relative path.
  Source =
    case filename:pathtype(FunSrc) of
      absolute -> FunSrc;
      relative ->
        % Deals with File = "./src". Must be a better way to do this.
        case lists:suffix(FunSrc, ModSrc) of
          true  -> ModSrc;
          false -> filename:join(filename:dirname(ModSrc), FunSrc)
        end
    end,
  [ {module,   M}
  , {function, F}
  , {arity,    A}
  , {exported, ExportedP}
  , {source,   Source}
  , {line,     Line}].


%%------------------------------------------------------------------------------
%% @doc
%% Equivalent to free_vars(Snippet, 1).
%% @end
-spec free_vars(Text::string()) -> {ok, FreeVars::[atom()]} |
                                   {error, [issue()]}.
%% @equiv free_vars(Text, 1)
%%------------------------------------------------------------------------------
free_vars(Snippet) -> free_vars(Snippet, 1).


%%------------------------------------------------------------------------------
%% @doc
%% Equivalent to free_vars(Snippet, 1).
%% @end
-spec free_vars(Text::string(), pos_integer()) ->
        {ok, FreeVars::[atom()]} |
        {error, [issue()]}.
%% @equiv free_vars(Text, 1)
%%------------------------------------------------------------------------------
free_vars(Text, StartLine) ->
  case edts_syntax:free_vars(Text, StartLine) of
    {ok, _} = Res    -> Res;
    {error, Errs} -> {error, format_errors(error, [{"Snippet", [Errs]}])}
  end.

string_to_mfa(String0) ->
  String = prepare_string(String0),
  case parse_expressions(String) of
    {error, _} = Err -> Err;
    [_, _|_]         -> {error, more_than_one_expr};
    [Expr]           ->
      case form_to_mfa(Expr) of
        {F, A}    -> {ok, [{function, F}, {arity, A}]};
        {M, F, A} -> {ok, [{module, M}, {function, F}, {arity, A}]}
      end
  end.

prepare_string(String0) ->
  %% Ugly hack to get rid of macros.
  String = re:replace(String0, "\\?", "", [{return, list}, global]),
  case lists:reverse(String) of
    [$.|_] -> String;
    _      -> String ++ "."
  end.

%% First two clauses are workarounds for "fun-less" function names, such as
%% those in a list of exports.
form_to_mfa({atom,  _, F})                     -> {F, 0};
form_to_mfa({var,   _, F})                     -> {F, 0};
form_to_mfa({op,    _, '/', {atom, _, F},
                             {integer, _, A}}) -> {F, A};
form_to_mfa({op,    _, '/', {remote, _,
                             {atom, _, M},
                             {atom, _, F}},
                             {integer, _, A}}) -> {M, F, A};
form_to_mfa({'fun', _, {function, F, A}})      -> {F, A};
form_to_mfa({'fun', _, {function,
                        {atom, _, M},
                        {atom, _, F},
                        {integer, _, A}}})     -> {M, F, A};
form_to_mfa({'fun', _, {function, M, F, A}})   -> {M, F, A};

form_to_mfa({call,  _, {var,  _, F}, Args})    -> {F, length(Args)};
form_to_mfa({call,  _, {atom, _, F}, Args})    -> {F, length(Args)};
form_to_mfa({call,  _, {remote,
                        _,
                        {atom, _, M},
                        {atom, _, F}}, Args})  -> {M, F, length(Args)}.



%%------------------------------------------------------------------------------
%% @doc
%% Tokenize and parse String as a sequence of expressions.
%% @end
-spec parse_expressions(string()) ->
        Forms::[erl_parse:abstract_expr()] | {error, term()}.
%%------------------------------------------------------------------------------
parse_expressions(String) ->
  case edts_syntax:parse_expressions(String) of
    {ok, Forms}  -> Forms;
    {error, Err} -> {error, format_errors(error, [{"Snippet", [Err]}])};
    {error, Info, _Loc} -> {error, format_errors(error, [{"Snippet", [Info]}])}
  end.


%%------------------------------------------------------------------------------
%% @doc
%% Returns information on M.
%% @end
-spec get_module_info(M::module(), Level::basic | detailed) ->
        {ok, [{atom(), term()}]} |
        {error, {not_found, module()}}.
%%------------------------------------------------------------------------------
get_module_info(M, Level) ->
  ensure_module_loaded(true, M),
  do_get_module_info(M, Level).

do_get_module_info(M, basic) ->
  try erlang:get_module_info(M) of
    Info ->
      {exports, Exports}           = lists:keyfind(exports, 1, Info),
      {ok, ModSrc}                 = get_module_source(M, Info),
      {ok, [ {module, M}
           , {exports, [[{function, F}, {arity, A}] || {F, A} <- Exports]}
           , {source, ModSrc}]}
  catch
    error:badarg ->
      {error, {not_found, M}}
  end;
do_get_module_info(M, detailed) ->
  case code:get_object_code(M) of
    {M, Bin, _File} ->
      {ok, {M, Chunks}} = beam_lib:chunks(Bin, [abstract_code]),
      {abstract_code, {_, A}} = lists:keyfind(abstract_code, 1, Chunks),
      Abstract = lists:map(fun erl_parse:anno_to_term/1, A),
      {ok, Basic} = do_get_module_info(M, basic),

      {source, Source} = lists:keyfind(source, 1, Basic),

      Acc0 = orddict:from_list([ {cur_file,    Source}
                               , {compile_cwd, get_compile_cwd(M, Abstract)}
                               , {exports,     M:module_info(exports)}
                               , {imports,     []}
                               , {includes,    []}
                               , {functions,   []}
                               , {records,     []}]
                               ++ Basic),
      Dict0 = lists:foldl(fun parse_abstract/2, Acc0, Abstract),
      Dict1 = orddict:update(imports,  fun(I) -> lists:usort(I) end, Dict0),
      Dict2 = orddict:update(includes, fun(I) -> lists:usort(I) end, Dict1),
      Dict = orddict:erase(cur_file,  Dict2),
      {ok, orddict:to_list(Dict)};
    error ->
      {error, {not_found, M}}
  end.

get_compile_cwd(M, [{attribute,1,file,{RelPath,1}}|_]) ->
  CompileInfo = M:module_info(compile),
  CompileOpts = proplists:get_value(options, CompileInfo),
  case proplists:get_value(cwd, CompileOpts, undefined) of
    undefined -> pop_dirs(proplists:get_value(source, CompileInfo), RelPath);
    Cwd       -> Cwd
  end.

pop_dirs(Source0, Rel) ->
  L = length(filename:split(Rel)),
  Source = lists:nthtail(L,lists:reverse(filename:split(Source0))),
  case Source of
    [] -> [];
    _  -> filename:join(lists:reverse(Source))
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Initialize current node with AppEnvs.
%% @end
-spec init([{Key::atom(), Value::term()}]) -> ok.
%%------------------------------------------------------------------------------
init(AppEnvs) ->
  lists:foreach(fun({K, V}) -> application:set_env(edts, K, V) end, AppEnvs),
  update_paths().


update_paths() ->
  {ok, ProjectRoot} = application:get_env(edts, project_root_dir),
  {ok, LibDirs} = application:get_env(edts, project_lib_dirs),
  Paths = edts_util:expand_code_paths(ProjectRoot, LibDirs),
  lists:foreach(fun code:add_path/1, Paths).


%%------------------------------------------------------------------------------
%% @doc
%% Returns a list of all modules known currently loaded on the node.
%% @end
-spec modules() -> [atom()].
%%------------------------------------------------------------------------------
modules() ->
  Beams = lists:append([modules_at_path(Path) || Path <- code:get_path()]),
  Binaries = [Module || {Module, _} <- code:all_loaded()],
  lists:usort(Binaries ++ Beams).

modules_at_path(Path) ->
  Beams = filelib:wildcard(filename:join(Path, "*.beam")),
  [list_to_atom(filename:rootname(filename:basename(Beam))) || Beam <- Beams].

%%------------------------------------------------------------------------------
%% @doc
%% Return the nome of this node's project.
%% @end
-spec project_name() -> string().
%%------------------------------------------------------------------------------
project_name() ->
  {ok, ProjectName} = application:get_env(edts, project_name),
  ProjectName.


%%------------------------------------------------------------------------------
%% @doc
%% Return the directory where EDTS-related data is stored for this node's
%% project.
%% @end
-spec project_data_dir() -> string().
%%------------------------------------------------------------------------------
project_data_dir() ->
  {ok, DataDir} = application:get_env(edts, project_data_dir),
  DataDir.


%%------------------------------------------------------------------------------
%% @doc
%% Return the nome of this node's project.
%% @end
-spec project_root_dir() -> string().
%%------------------------------------------------------------------------------
project_root_dir() ->
  {ok, DataDir} = application:get_env(edts, project_root_dir),
  DataDir.

project_specific_data_file(Suffix) ->
  DataDir = project_data_dir(),
  RootDir = project_root_dir(),
  ProjectName = project_name(),
  Filename = io_lib:format("~p_~s~s",
                           [erlang:phash2(RootDir), ProjectName, Suffix]),
  filename:join(DataDir, Filename).


%%------------------------------------------------------------------------------
%% @doc
%% Starts the edts services on the node
%% @end
-spec start() -> ok.
%%------------------------------------------------------------------------------
start() ->
  case edts_module_server:started_p() of
    true  -> edts_module_server:update();
    false -> {ok, _} = edts_module_server:start()
  end,
  ok.


%%------------------------------------------------------------------------------
%% @doc
%% Return true if the edts_code service has been started.
%% @end
-spec started_p() -> boolean().
%%------------------------------------------------------------------------------
started_p() ->
  edts_module_server:started_p().


%%%_* Internal functions =======================================================

%%------------------------------------------------------------------------------
%% @equiv ensure_module_loaded(Mod,
%%                             code:where_is_file(atom_to_list(Mod) ++ ".beam"),
%%                             Reload).
%% @end
-spec ensure_module_loaded(Reload :: boolean(),
                           Mod    :: module()) -> boolean() | {error, term()}.
%%------------------------------------------------------------------------------
ensure_module_loaded(Reload, Mod) ->
  case code:where_is_file(atom_to_list(Mod) ++ ".beam") of
    non_existing -> {error, nofile};
    File         -> ensure_module_loaded(Mod, File, Reload)
  end.


%%------------------------------------------------------------------------------
%% @doc
%% Load module if:
%% - The module is not sticky
%% - The module is loaded from a file other than File OR
%%   the module is currently not loaded.
%%
%% If Reload =:= true, reload the module if its beam-file has been changed.
%%
%% Returns true if the module was (re)loaded, false otherwise.
%% @end
-spec ensure_module_loaded(Mod    :: module(),
                           File   :: filename:filename(),
                           Reload :: boolean()) -> boolean().
%%------------------------------------------------------------------------------
ensure_module_loaded(Mod, File, Reload) ->
  case code:is_sticky(Mod) of
    true  -> false;
    false ->
      case code:is_loaded(Mod) of
        {file, File} when Reload -> maybe_reload(Mod, File);
        {file, File}             -> false;
        {file, _}                -> try_load_mod(Mod, File);
        false                    -> try_load_mod(Mod, File)
      end
  end.

maybe_reload(Mod, File) ->
  case module_modified_p(Mod, File) of
    true  -> try_load_mod(Mod, File);
    false -> false
  end.

reload_if_modified(SrcFile, OutDir) ->
  ModuleName = filename:basename(SrcFile, ".erl"),
  Beam = filename:join([OutDir, ModuleName ++ ".beam"]),
  Module = list_to_atom(ModuleName),

  case code:is_loaded(Module) of
    {file, _Loaded} ->
      do_reload_if_modified(Module, Beam);
    false ->
      false
  end.

do_reload_if_modified(Module, Beam) ->
  case module_modified_p(Module, Beam) of
    true ->
      try_load_mod(Module, Beam);
    false ->
      false
  end.

try_load_mod(Mod, File) ->
  LoadFileName = filename:rootname(File), %% Remove extension
  try load_mod(Mod, LoadFileName)
  catch
    C:E ->
      error_logger:error_msg("Loading ~p failed with ~p:~p", [Mod, C, E]),
      false
  end.

load_mod(Mod, File) ->
  code:purge(Mod),
  case code:load_abs(File) of
    {module, Mod} -> true;
    {error, Rsn}  -> error(Rsn)
  end.

module_modified_p(Mod, File) ->
  case module_modified_md5_p(Mod, File) of
    {ok, ModifiedP} -> ModifiedP;
    error           -> module_modified_mtime_p(Mod, File)
  end.

module_modified_md5_p(Mod, File) ->
  case lists:keyfind(md5, 1, Mod:module_info()) of
    {md5, ModMD5} ->
      case beam_lib:md5(File) of
        {error, _, _}        -> {ok, true};
        {ok, {Mod, FileMD5}} -> {ok, ModMD5 =/= FileMD5}
      end;
    false ->
      error
  end.

module_modified_mtime_p(Mod, File) ->
  do_module_modified_mtime_p(lists:keyfind(time, 1, Mod:module_info(compile)),
                             file:read_file_info(File)).

do_module_modified_mtime_p(false, _) ->
  true;
do_module_modified_mtime_p(_, {error, _}) ->
  true;
do_module_modified_mtime_p({time,
                            {CYear, CMonth, CDay, CHour, CMinute, CSecond}},
                           {ok, #file_info{mtime = MTime}}) ->
  MTime > {{CYear, CMonth, CDay}, {CHour, CMinute, CSecond}}.

get_compile_outdir(File) ->
  Mod  = list_to_atom(filename:basename(filename:rootname(File))),
  Opts = try proplists:get_value(options, Mod:module_info(compile), [])
         catch error:undef -> [] %% No beam-file
         end,
  get_compile_outdir(File, Opts).

get_compile_outdir(File, Opts) ->
  case proplists:get_value(outdir, Opts) of
    undefined -> filename_to_outdir(File);
    OutDir    ->
      case filelib:is_dir(OutDir) of
        true  -> OutDir;
        false -> filename_to_outdir(File)
      end
  end.

filename_to_outdir(File) ->
  DirName = filename:dirname(File),
  EbinDir = filename:join([DirName, "..", "ebin"]),
  case filelib:is_dir(EbinDir) of
    true  -> EbinDir;
    false -> DirName
  end.


%%------------------------------------------------------------------------------
%% @equiv get_module_source(M, M:module_info()).
-spec get_module_source(M::module()) -> {ok, string()} | {error, not_found}.
%%------------------------------------------------------------------------------
get_module_source(M) ->
  get_module_source(M, M:module_info()).

%%------------------------------------------------------------------------------
%% @doc Try to find the source of M using (in order):
%% - The beam-file source compile attribute if present.
%% - If the module is located in a directory called ebin, the src subfolder of
%%   the that directory's parent directory (eg foo/src for a module located in
%%   foo/ebin).
%% - The module beam-file's location (eg foo for a module in foo).
%% - The erts application source-directory. This is for modules located in the
%%   'preloaded' directory of otp.
-spec get_module_source(M::module(), MInfo::[{atom(), term()}]) ->
                           {ok, string()} | {error, not_found}.
%%------------------------------------------------------------------------------
get_module_source(M, MInfo) ->
  case get_module_source_from_info(MInfo) of
    {ok, _Src} = Res   -> Res;
    {error, not_found} -> get_module_source_from_beam(M)
  end.

get_module_source_from_info(MInfo) ->
  try
    {compile, Compile} = lists:keyfind(compile, 1, MInfo),
    {source,  ModSrc}  = lists:keyfind(source,  1, Compile),
    true               = filelib:is_regular(ModSrc),
    {ok, ModSrc}
  catch
    error:_ -> {error, not_found}
  end.

get_module_source_from_beam(M) ->
  try
    ModPath = code:where_is_file(atom_to_list(M) ++ ".beam"),
    SrcFile = atom_to_list(M) ++ ".erl",
    SplitPath = filename:split(filename:dirname(ModPath)),
    [Dir|Rest] = lists:reverse(SplitPath),
    SrcAbs =
      case Dir of
        "ebin" -> filename:join(lists:reverse(Rest) ++ ["src", SrcFile]);
        _      -> filename:join(SplitPath ++ [SrcFile])
      end,
    true = filelib:is_regular(SrcAbs),
    {ok, SrcAbs}
  catch
    error:_ -> {error, not_found}
  end.


%%------------------------------------------------------------------------------
%% @doc Format compiler errors and warnings.
-spec format_errors( ErrType ::warning | error
                   , Errors  ::[{ File::string(), [term()]}]) -> [issue()].
%%------------------------------------------------------------------------------
format_errors(Type, Errors) ->
  LineErrorF =
    fun(File, {Line, Source, ErrorStr}) ->
        {Type, File, Line, lists:flatten(Source:format_error(ErrorStr))}
    end,
  FileErrorF =
    fun({File, Errors0}) ->
        lists:map(fun(Err) -> LineErrorF(File, Err) end, Errors0)
    end,
  lists:flatmap(FileErrorF, Errors).


%%------------------------------------------------------------------------------
%% @doc Get the file and line of a function from abstract code.
-spec get_file_and_line(M::module(), F::atom(), A::non_neg_integer(),
                        File::string(), Abstract::[term()])->
        {ok, {File::string(), Line::non_neg_integer()}} |
        {error, not_found}.
%%------------------------------------------------------------------------------
get_file_and_line(M, new, A, CurFile,
                  [{attribute, Line, module, {M, Attrs}}|_T])
  when length(Attrs) =:= A ->
  {ok, {CurFile, Line}};
get_file_and_line(_M, F, A, CurFile, [{function, Line, F, A, _Clauses}|_T]) ->
  {ok, {CurFile, Line}};
get_file_and_line(M, F, A, _CurFile, [{attribute, _, file, {File, _}}|T]) ->
  get_file_and_line(M, F, A, File, T);
get_file_and_line(M, F, A, CurFile, [_H|T]) ->
  get_file_and_line(M, F, A, CurFile, T);
get_file_and_line(_M, _F, _A, _CurFile, []) ->
  {error, not_found}.

%%------------------------------------------------------------------------------
%% @doc Parse abstract code into a module information substract.
-spec parse_abstract(Abstract::
                       {function, pos_integer(), function(), arity(), any()} |
                       {attribute, pos_integer(), file | import | record, {any(), any()}} |
                       term(),
                     Acc::orddict:orddict()) ->
        orddict:orddict().
%%------------------------------------------------------------------------------
parse_abstract({function, Line, F, A, _Clauses}, Acc) ->
  M = orddict:fetch(module, Acc),
  FunInfo =
    [ {module,   M}
    , {function, F}
    , {arity,    A}
    , {exported, lists:member({F, A}, M:module_info(exports))}
    , {source,   orddict:fetch(cur_file, Acc)}
    , {line,     Line}],
  orddict:update(functions, fun(Fs) -> [FunInfo|Fs] end, Acc);
parse_abstract({attribute, _Line0, file, {[_|_] = Src0, _Line1}}, Acc0) ->
  %% %% Get rid of any local paths, in case function was defined in a
  %% %% file include with a relative path.
  BeamSource = path_flatten(orddict:fetch(source, Acc0)),
  ModSrc =
    case filename:pathtype(Src0) of
      absolute ->
        path_flatten(Src0);
      relative ->
        path_flatten(filename:join(orddict:fetch(compile_cwd, Acc0), Src0))
    end,
  %% Update list of all files.
  Acc =
    case ModSrc of
      BeamSource -> Acc0;
      ModSrc        -> orddict:update(includes, fun(I) -> [ModSrc|I] end, Acc0)
    end,
  %% Update current file.
  orddict:store(cur_file, ModSrc, Acc);
parse_abstract({attribute,_Line,import, {Module, Imports0}}, Acc) ->
  Imports = [[ {module, Module}
             , {function, F}
             , {arity, A}] || {F, A} <- Imports0],
  orddict:update(imports, fun(I) -> Imports ++ I end, Acc);
parse_abstract({attribute, Line ,record,{Recordname, Fields}}, Acc) ->
  FieldsF = fun get_record_field/1,
  RecordInfo =
    [ {record, Recordname}
    , {fields, lists:map(FieldsF, Fields)}
    , {line,   Line}
    , {source, orddict:fetch(cur_file, Acc)}],
  orddict:update(records, fun(Old) -> [RecordInfo|Old] end, Acc);
parse_abstract(_, Acc) -> Acc.

get_record_field({record_field, _, {_, _, FName}}) -> FName;
get_record_field({record_field, _, {_, _, FName}, _Call}) -> FName;
get_record_field({typed_record_field, RecFieldTuple, _}) ->
  get_record_field(RecFieldTuple).

%%------------------------------------------------------------------------------
%% @doc
%% "Flatten" path by expanding all '.' and '..'
%% end
-spec path_flatten(string()) -> string().
%%------------------------------------------------------------------------------
path_flatten(Path0) ->
  Filename = filename:basename(Path0),
  Dirname = filename:dirname(Path0),
  Path = path_flatten(filename:split(Dirname), [], []),
  filename:join(Path, Filename).

path_flatten([], [_|_] = Back, Acc) ->
  filename:join(Back ++ lists:reverse(Acc));

path_flatten([], _Back, Acc) ->
  filename:join(["."|lists:reverse(Acc)]);

path_flatten(["."|Rest], Back, Acc) ->
  path_flatten(Rest, Back, Acc);

path_flatten([".."|Rest], Back, [_|AccRest]) ->
  path_flatten(Rest, Back, AccRest);

path_flatten([".."|Rest], Back, Acc) ->
  path_flatten(Rest, [".."|Back], Acc);

path_flatten([_Dir|Rest], [_|Back], Acc) ->
  path_flatten(Rest, Back, Acc);

path_flatten([Dir|Rest], Back, Acc) ->
  path_flatten(Rest, Back, [Dir|Acc]).


get_additional_includes(FileLoc, PrevOpts) ->
  PrevDirs = [Include || {i, _} = Include <- PrevOpts],
  Dirs = lists:usort(get_project_includes() ++ get_app_includes(FileLoc)),
  Dirs -- PrevDirs.

get_project_includes() ->
  {ok, RootDir}        = application:get_env(edts, project_root_dir),
  {ok, ProjectIncDirs} = application:get_env(edts, project_include_dirs),
  [{i, filename:join(RootDir, IncDir)} || IncDir <- ProjectIncDirs].

get_app_includes(FileLoc) ->
  {ok, AppIncDirs} = application:get_env(edts, app_include_dirs),
  ParentDir = filename:dirname(FileLoc),
  case
    filename:basename(FileLoc) =:= "src" orelse
    filename:basename(FileLoc) =:= "test"
  of
    true  -> [{i, filename:join(ParentDir, D)} || D <- AppIncDirs];
    false -> []
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Extracts compile options from module, if it exists
%% @end
-spec extract_compile_opts(file:filename()) -> [compile:option()].
%%------------------------------------------------------------------------------
extract_compile_opts(File) ->
  Module = list_to_atom(filename:basename(File, ".erl")),
  case code:is_loaded(Module) of
    false     -> [];
    {file, _} ->
      Opts = proplists:get_value(options, Module:module_info(compile)),
      [Opt || Opt <- Opts, extract_compile_opt_p(Opt)]
  end.

extract_compile_opt_p({parse_transform, _})    -> true;
extract_compile_opt_p({i,               _})    -> true;
extract_compile_opt_p({d,               _})    -> true;
extract_compile_opt_p({d,               _, _}) -> true;
extract_compile_opt_p(export_all)              -> true;
extract_compile_opt_p({no_auto_import,  _})    -> true;
extract_compile_opt_p(_)                       -> false.

%%%_* Unit tests ===============================================================

-ifdef(TEST).

get_additional_includes_test_() ->
  AppIncDirs = ["foo"],
  ProjIncs = ["test/include"],
  Root = "root",
  AppDir = filename:join([Root, "lib", "app_dir"]),
  SrcDir = filename:join(AppDir, "src"),
  AbsProjectInc = filename:join(Root, "test/include"),

  {setup,
   fun() ->
       Env = application:get_all_env(edts),
       application:set_env(edts, app_include_dirs, AppIncDirs),
       application:set_env(edts, project_include_dirs, ProjIncs),
       application:set_env(edts, project_root_dir, Root),
       [{prev_env, Env}]
   end,
   fun([{prev_env, Env}]) ->
       [application:unset_env(edts, K) || {K, _V} <- application:get_all_env()],
       [application:set_env(edts, K, V) || {K, V} <- Env]
   end,
   [ ?_assertEqual([], ""),
     ?_assertEqual([{i, AbsProjectInc}],
                   get_additional_includes(filename:join(AppDir, "not_src"),
                                           [])),
     ?_assertEqual([{i, AbsProjectInc}], get_additional_includes(AppDir, [])),

     %% Additional include is added from app_include_dirs
     ?_assertEqual(
        lists:sort([{i, AbsProjectInc}, {i, filename:join(AppDir, "foo")}]),
        lists:sort(get_additional_includes(SrcDir, []))),
     %% The include from app_include_dirs is already present in the options
     ?_assertEqual([{i, AbsProjectInc}],
                   get_additional_includes(SrcDir,
                                          [{i, filename:join(AppDir, "foo")}]))
   ]}.

string_to_mfa_test_() ->
  [ ?_assertEqual({ok, [{function, foo}, {arity, 0}]},
                  string_to_mfa("foo()")),
    ?_assertEqual({ok, [{function, foo}, {arity, 1}]},
                  string_to_mfa("foo(bar)")),
    ?_assertEqual({ok, [{function, foo}, {arity, 1}]},
                  string_to_mfa("foo([]).")),
    ?_assertEqual({ok, [{function, foo}, {arity, 1}]},
                   string_to_mfa("foo(\"aa,bb\")")),
    ?_assertEqual({ok, [{function, foo}, {arity, 2}]},
                  string_to_mfa("foo(\"aa,bb\", cc)")),
    ?_assertEqual({ok, [{function, foo}, {arity, 1}]},
                  string_to_mfa("foo(\"aa,bb\")")),
    ?_assertEqual({ok, [{function, foo}, {arity, 2}]},
                  string_to_mfa("foo(bar, baz).")),
    ?_assertEqual({ok, [{function, foo}, {arity, 3}]},
                  string_to_mfa("foo(a,%a,b\nb, cc).")),
    ?_assertEqual({ok, [{function, foo}, {arity, 2}]},
                  string_to_mfa("foo(a, <<a/b,\nc/d>>).")),
    ?_assertEqual({ok, [{module, foo}, {function, bar}, {arity, 1}]},
                  string_to_mfa("foo:bar(baz).")),
    ?_assertEqual({ok, [{function, foo}, {arity, 1}]},
                  string_to_mfa("fun foo/1.")),
    ?_assertEqual({ok, [{module, foo}, {function, bar}, {arity, 1}]},
                  string_to_mfa("fun foo:bar/1.")),
    ?_assertEqual({ok, [{function, foo}, {arity, 1}]},
                  string_to_mfa("foo/1.")),
    ?_assertEqual({ok, [{module, foo}, {function, bar}, {arity, 1}]},
                  string_to_mfa("foo:bar/1.")),
    ?_assertEqual({ok, [{function, 'MATCH'}, {arity, 0}]},
                  string_to_mfa("MATCH.")),
    ?_assertEqual({ok, [{function, 'MATCH'}, {arity, 0}]},
                  string_to_mfa("MATCH().")),
    ?_assertEqual({error,[{error,"Snippet",1,"syntax error before: '.'"}]},
                  string_to_mfa("foo(\"aa,bb\"."))
  ].

format_errors_test_() ->
  SetupF = fun() ->
               meck:unload(),
               meck:new(source, [non_strict]),
               meck:expect(source, format_error,
                           fun(Error) -> atom_to_list(Error) end)
           end,
  CleanupF = fun(_) ->
                 meck:unload()
             end,
  { setup, SetupF, CleanupF,
    [ ?_assertEqual( [ {warning, "file1", 1337, "error1"},
                       {warning, "file1", 1338, "error2"},
                       {warning, "file2", 1339, "error3"},
                       {warning, "file2", 1340, "error4"} ],
                     format_errors(warning,
                                   [ {"file1", [ {1337, source, error1},
                                                 {1338, source, error2}]},
                                     {"file2", [ {1339, source, error3},
                                                 {1340, source, error4}]}
                                   ])),
      ?_assertEqual( [ {error, "file1", 1337, "error1"},
                       {error, "file1", 1338, "error2"},
                       {error, "file2", 1339, "error3"},
                       {error, "file2", 1340, "error4"} ],
                     format_errors( error,
                                    [ {"file1", [ {1337, source, error1},
                                                  {1338, source, error2}]},
                                      {"file2", [ {1339, source, error3},
                                                  {1340, source, error4}]}
                                    ]))
    ]}.

get_file_and_line_test_() ->
  Forms = test_file_forms("non-parametrised-module"),
  File  = "non_parametrised_nodule.erl",
  Mod   = non_parametrised_nodule,
  [ ?_assertEqual( {error, not_found},
                   get_file_and_line(Mod, f, 0, File, [])),
    ?_assertEqual( {ok, {File, 12}},
                   get_file_and_line(Mod, foo, 0, File, Forms)),
    ?_assertEqual( {ok, {"bar.erl", 12}},
                   get_file_and_line(Mod, foo, 0, File,
                                     [ {attribute, '', file, {"bar.erl", ''}}
                                       | Forms]))
  ].

get_file_and_line_parametrised_mod_test_() ->
  Forms = test_file_forms("parametrised-module"),
  File  = "parametrised_module.erl",
  Mod   = parametrised_module,
  [ ?_assertEqual( {ok, {File, 3}},
                   get_file_and_line(Mod, new, 1, File, Forms)),
    ?_assertEqual( {ok, {File, 10}},
                   get_file_and_line(Mod, foo, 0, File, Forms)),
    ?_assertEqual( {ok, {File, 12}},
                   get_file_and_line(Mod, new, 0, File, Forms)),
    ?_assertEqual( {ok, {File, 14}},
                   get_file_and_line(Mod, new, 2, File, Forms))
  ].

get_file_and_line_non_parametrised_new_test_() ->
  Forms = test_file_forms("non-parametrised-module"),
  File  = "non_parametrised_module.erl",
  Mod   =  non_parametrised_module,
  [ ?_assertEqual( {error, not_found},
                   get_file_and_line(Mod, new, 1, File, Forms)),
    ?_assertEqual( {ok, {File, 12}},
                   get_file_and_line(Mod, foo, 0, File, Forms)),
    ?_assertEqual( {ok, {File, 14}},
                   get_file_and_line(Mod, new, 0, File, Forms)),
    ?_assertEqual( {ok, {File, 16}},
                   get_file_and_line(Mod, new, 2, File, Forms))
  ].

get_module_source_test_() ->
  BaseName = atom_to_list(?MODULE) ++ ".erl",
  {ok, Cwd} = file:get_cwd(),
  ErlangAbsNames =
    %% Check for path based on both Cwd and application lib-dir to try
    %% to get around weird things when EDTS is located beneath a symbolic
    %% link in the file tree.
    [filename:join([Cwd, "lib", "edts", "src", BaseName]),
     filename:join([Cwd, "lib", "edts", ".eunit", BaseName]),
     filename:join(edts_util:shorten_path(code:lib_dir(edts, src)),
                   BaseName),
     filename:join(edts_util:shorten_path(code:lib_dir(edts, '.eunit')),
                   BaseName)],
  [?_assertEqual({error, not_found},
                 module_source_test_ret(
                   get_module_source_from_info(erlang:module_info()))),
   ?_assert(lists:member(module_source_test_ret(
                           get_module_source_from_beam(?MODULE)),
                         ErlangAbsNames)),
   ?_assertEqual({error, not_found},
                 module_source_test_ret(
                   get_module_source_from_info([]))),
   ?_assertEqual({error, not_found},
                 module_source_test_ret(
                   get_module_source_from_beam(erlang_foo))),
   ?_assert(lists:member(module_source_test_ret(
                           get_module_source(?MODULE, ?MODULE:module_info())),
                         ErlangAbsNames)),
   ?_assertEqual({error, not_found},
                module_source_test_ret(
                 get_module_source(erlang_foo, [])))
  ].


path_flatten_test_() ->
  [ ?_assertEqual("./bar.erl",     path_flatten("bar.erl")),
    ?_assertEqual("./bar.erl",     path_flatten("./bar.erl")),
    ?_assertEqual("./bar.erl",     path_flatten("../foo/bar.erl")),
    ?_assertEqual("./bar.erl",     path_flatten(".././foo/bar.erl")),
    ?_assertEqual("./foo/bar.erl", path_flatten("bar/../foo/bar.erl")),
    ?_assertEqual("../bar.erl",    path_flatten("bar/../foo/../../bar.erl")),
    ?_assertEqual("./foo/bar.erl", path_flatten("./././foo//bar.erl"))
   ].

parse_abstract_function_test_() ->
  Mod = test,
  Fun = bar,
  Arity = 1,
  Line = 1337,
  CurFile = "/foo/test.erl",
  Exports = [{bar, 1}],
  meck:unload(),
  meck:new(Mod, [no_link, non_strict]),
  meck:expect(Mod, Fun, fun(_) -> ok end),
  Acc = orddict:from_list([ {module,    Mod},
                            {cur_file,  CurFile},
                            {exports,   Exports},
                            {functions, []}]),
  Res = parse_abstract({function, Line, Fun, Arity, []}, Acc),
  meck:unload(),
  [ ?_assertEqual(
       lists:sort([module, cur_file, exports, functions]),
       lists:sort(orddict:fetch_keys(Res))),
    ?_assertEqual(Mod,     orddict:fetch(module, Res)),
    ?_assertEqual(CurFile, orddict:fetch(cur_file, Res)),
    ?_assertEqual(Exports, orddict:fetch(exports, Res)),
    ?_assertMatch([_],     orddict:fetch(functions, Res)),
    ?_assertEqual(lists:sort([ {module, Mod},
                               {function, Fun},
                               {arity,    Arity},
                               {source,   CurFile},
                               {line,     Line},
                               {exported, true}]),
                  lists:sort(hd(orddict:fetch(functions, Res))))
  ].

parse_abstract_include_test_() ->
  Line = 1337,
  Source0 = "/foo/test.erl",
  Source1 = "test.erl",
  Include0 = "test.hrl",
  CompileCwd = "/foo",
  Acc = orddict:from_list([ {source,      Source0},
                            {compile_cwd, CompileCwd},
                            {includes,    []}]),
  Res0 = parse_abstract({attribute, Line, file, {Source0, Line}}, Acc),
  Res1 = parse_abstract({attribute, Line, file, {Source1, Line}}, Acc),
  Res2 = parse_abstract({attribute, Line, file, {Include0, Line}}, Acc),
  [ ?_assertEqual(
       lists:sort([source, compile_cwd, includes, cur_file]),
       lists:sort(orddict:fetch_keys(Res0))),
    ?_assertEqual(Source0, orddict:fetch(source, Res0)),
    ?_assertEqual(CompileCwd, orddict:fetch(compile_cwd, Res0)),
    ?_assertEqual([], orddict:fetch(includes, Res0)),
    ?_assertEqual([], orddict:fetch(includes, Res1)),
    ?_assertEqual(["/foo/test.hrl"], orddict:fetch(includes, Res2))
  ].

parse_abstract_import_test_() ->
  Acc = orddict:from_list([ {imports, []}]),
  Res0 = parse_abstract({attribute, 0, import, {foo, [{bar, 1}]}}, Acc),
  [ ?_assertEqual([imports], orddict:fetch_keys(Res0)),
    ?_assertMatch([_], orddict:fetch(imports, Res0)),
    ?_assertEqual( lists:sort([ {module,   foo},
                                {function, bar},
                                {arity,    1}]),
                   lists:sort(hd(orddict:fetch(imports, Res0))))
  ].

parse_abstract_record_test_() ->
  Fields = [ {record_field, foo, {foo, foo, field_1}}
           , {record_field, foo, {foo, foo, field_2}, init_call}
           , {typed_record_field,
              {record_field, foo, {foo, foo, field_3}}, type}
           , {typed_record_field,
              {record_field, foo, {foo, foo, field_4}, init_call}, type}
           ],
  Line = 1337,
  CurFile = "/foo/test.erl",
  Acc = orddict:from_list([ {cur_file, CurFile}
                          , {records, []}]),
  Res = parse_abstract({attribute, Line, record, {rec_name, Fields}}, Acc),
  [ ?_assertEqual(
       lists:sort([cur_file, records])
     , lists:sort(orddict:fetch_keys(Res)))
  , ?_assertEqual(CurFile, orddict:fetch(cur_file, Res))
  , ?_assertMatch([_],     orddict:fetch(records, Res))
  , ?_assertEqual( lists:sort([ {record,   rec_name}
                              , {fields, [field_1, field_2, field_3, field_4]}
                              , {line,   Line}
                              , {source, CurFile}])
                 , lists:sort(hd(orddict:fetch(records, Res))))
  ].

parse_abstract_other_test_() ->
  Acc = orddict:from_list([{bar, baz}]),
  [?_assertEqual(Acc, parse_abstract(foo, Acc))].

modules_test_() ->
  [?_assertMatch([_|_], modules())].

get_compile_outdir_test_() ->
  Good = "good/../ebin",
  F    = fun get_compile_outdir/2,
  [{ setup,
     fun () ->
         meck:new(filelib, [passthrough, unstick]),
         meck:expect(filelib, is_dir, fun ("good/../ebin") -> true;
                                          (_)              -> false
                                       end)
     end,
     fun (_) -> meck:unload() end,
     [ ?_assertEqual(Good , F("foo/mod.erl" , [{outdir, Good}])),
       ?_assertEqual(Good , F("good/mod.erl", [{outdir, "foo"}])),
       ?_assertEqual("foo", F("foo/mod.erl" , [])),
       ?_assertEqual(Good , F("good/mod.erl", []))
     ]
   }].

do_module_modified_mtime_p_test_() ->
  CTime1 = {time, {1, 1, 1, 1, 1, 1}},
  CTime2 = {time, {1, 1, 1, 1, 1, 2}},
  MTime1 = #file_info{mtime = {{1, 1, 1}, {1, 1, 1}}},
  MTime2 = #file_info{mtime = {{1, 1, 1}, {1, 1, 2}}},
  [?_assert(do_module_modified_mtime_p(false, {ok, MTime1})),
   ?_assert(do_module_modified_mtime_p(CTime1, {error, bla})),
   ?_assert(do_module_modified_mtime_p(CTime1, {ok, MTime2})),
   ?_assertNot(do_module_modified_mtime_p(CTime1, {ok, MTime1})),
   ?_assertNot(do_module_modified_mtime_p(CTime2, {ok, MTime1}))
  ].

-endif.

%%%_* Test helpers =============================================================

-ifdef(TEST).

module_source_test_ret({ok, Path}) -> edts_util:shorten_path(Path);
module_source_test_ret(Ret)        -> Ret.

test_file_forms(File) ->
  Path = filename:join([code:priv_dir(edts), "test/modules", File]),
  {ok, Bin} = file:read_file(Path),
  {ok, Forms} = edts_syntax:parse_forms(unicode:characters_to_list(Bin)),
  Forms.

-endif.

%%%_* Emacs ====================================================================
%% %%% Local Variables:
%% %%% allout-layout: t
%% %%% erlang-indent-level: 2
%% %%% End:
