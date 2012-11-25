%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc code-analysis library for edts.
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
-module(edts_code).

%%%_* Includes =================================================================
-include_lib("eunit/include/eunit.hrl").

%%%_* Exports ==================================================================

-export([add_paths/1,
         check_module/2,
         compile_and_load/1,
         free_vars/1,
         free_vars/2,
         get_function_info/3,
         get_module_info/1,
         get_module_info/2,
         modules/0,
         parse_expressions/1,
         start/0,
         started_p/0,
         who_calls/3]).

%% internal exports
-export([save_xref_state/0]).

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
%% the path using ?MODULE:shorten_path/1, which means symbolic links could cause
%% duplicate paths to be added.
%% @end
-spec add_path(filename:filename()) -> code:add_path_ret().
%%------------------------------------------------------------------------------
add_path(Path) -> code:add_patha(shorten_path(Path)).

%%------------------------------------------------------------------------------
%% @doc
%% Call add_path/1 for each path in Paths.
%% @end
-spec add_paths([filename:filename()]) -> ok.
%%------------------------------------------------------------------------------
add_paths(Paths) -> lists:foreach(fun add_path/1, Paths).

%%------------------------------------------------------------------------------
%% @doc
%% Do an xref-analysis of Module, applying Checks
%% @end
-spec check_module(Module::module(), Checks::[xref:analysis()]) ->
                      {ok, [issue()]}.
%%------------------------------------------------------------------------------
check_module(Module, Checks) ->
  case code:is_loaded(Module) of
    false      -> reload_module(Module);
    {file, _F} -> ok
  end,
  edts_xref:check_module(Module, Checks).

%%------------------------------------------------------------------------------
%% @doc
%% Equivalent to compile_and_load(Module, []).
%% @end
-spec compile_and_load(Module::file:filename() | module()) ->
                          {ok | error, [issue()]}.
%%------------------------------------------------------------------------------
compile_and_load(Module) ->
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
                          {ok | error, [issue()]}.
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
  CompileOpts =
    [{cwd, Cwd}, binary, debug_info, return|Opts] ++ extract_compile_opts(File),
  %% Only compile to a binary to begin with since compile-options resulting in
  %% an output-file will cause the compile module to remove the existing beam-
  %% file even if compilation fails, in which case we end up with no module
  %% at all for other analyses (xref etc.).
  case compile:file(File, CompileOpts) of
    {ok, Mod, Bin, Warnings} ->
      OutDir  = get_compile_outdir(File0),
      OutFile = filename:join(OutDir, atom_to_list(Mod)),
      case file:write_file(OutFile ++ ".beam", Bin) of
        ok ->
          code:purge(Mod),
          {module, Mod} = code:load_abs(OutFile),
          add_path(OutDir),
          update_xref(),
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
  reload_module(M),
  {M, Bin, _File}                   = code:get_object_code(M),
  {ok, {M, Chunks}}                 = beam_lib:chunks(Bin, [abstract_code]),
  {abstract_code, {_Vsn, Abstract}} = lists:keyfind(abstract_code, 1, Chunks),
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
-spec free_vars(Text::string(), pos_integer()) -> {ok, FreeVars::[atom()]} |
                                   {error, [issue()]}.
%% @equiv free_vars(Text, 1)
%%------------------------------------------------------------------------------
free_vars(Text, StartLine) ->
  case edts_syntax:free_vars(Text, StartLine) of
    {ok, _} = Res    -> Res;
    {error, _} = Err -> format_errors(error, [{"Snippet", [Err]}])
  end.


%%------------------------------------------------------------------------------
%% @doc
%% Tokenize and parse String as a sequence of expressions.
%% @end
-spec parse_expressions(string()) -> Forms::erl_parse:abstract_form().
%%------------------------------------------------------------------------------
parse_expressions(String) ->
  case edts_syntax:parse_expressions(String) of
    {ok, Forms}      -> Forms;
    {error, _} = Err -> format_errors(error, [{"Snippet", [Err]}])
  end.


%%------------------------------------------------------------------------------
%% @doc
%% Equivalent to get_module_info(M, detailed).
%% @end
-spec get_module_info(M::module()) -> [{atom, term()}].
%%------------------------------------------------------------------------------
get_module_info(M) ->
  get_module_info(M, detailed).

%%------------------------------------------------------------------------------
%% @doc
%% Returns information on M.
%% @end
-spec get_module_info(M::module(), Level::basic | detailed) -> [{atom, term()}].
%%------------------------------------------------------------------------------
get_module_info(M, Level) ->
  reload_module(M),
  do_get_module_info(M, Level).

do_get_module_info(M, basic) ->
  Info                         = erlang:get_module_info(M),
  {compile, Compile}           = lists:keyfind(compile, 1, Info),
  {exports, Exports}           = lists:keyfind(exports, 1, Info),
  {time, {Y, Mo, D, H, Mi, S}} = lists:keyfind(time,    1, Compile),
  {ok, ModSrc}                    = get_module_source(M, Info),
  [ {module, M}
  , {exports, [[{function, F}, {arity, A}] || {F, A} <- Exports]}
  , {time, {{Y, Mo, D}, {H, Mi, S}}}
  , {source, ModSrc}];
do_get_module_info(M, detailed) ->
  {M, Bin, _File}                   = code:get_object_code(M),
  {ok, {M, Chunks}}                 = beam_lib:chunks(Bin, [abstract_code]),
  {abstract_code, {_Vsn, Abstract}} = lists:keyfind(abstract_code, 1, Chunks),
  Basic            = do_get_module_info(M, basic),

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
  orddict:to_list(Dict).

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
%% Returns a list of all modules known to the edts_code xref-server.
%% @end
-spec modules() -> [atom()].
%%------------------------------------------------------------------------------
modules() ->
  Beams = lists:append([modules_at_path(Path) || Path <- code:get_path()]),
  Binaries = [Module || {Module, _} <- code:all_loaded()],
  {ok, lists:usort(Binaries ++ Beams)}.

modules_at_path(Path) ->
  Beams = filelib:wildcard(filename:join(Path, "*.beam")),
  [list_to_atom(filename:rootname(filename:basename(Beam))) || Beam <- Beams].

%%------------------------------------------------------------------------------
%% @doc
%% Starts the edts xref-server on the local node.
%% @end
-spec start() -> {node(), ok} | {error, already_started}.
%%------------------------------------------------------------------------------
start() ->
  case edts_xref:started_p() of
    true  -> {error, already_started};
    false ->
      case init_xref() of
        {error, _} = Err -> Err;
        {ok, _Pid}       -> {node(), ok}
      end
  end.


%%------------------------------------------------------------------------------
%% @doc
%% Return true if the edts_code service has been started.
%% @end
-spec started_p() -> boolean().
%%------------------------------------------------------------------------------
started_p() -> edts_xref:started_p().


%%------------------------------------------------------------------------------
%% @doc
%% Returns alist with all functions that call M:F/A on the local node.
%% @end
-spec who_calls(module(), atom(), non_neg_integer()) ->
                   [{module(), atom(), non_neg_integer()}].
%%------------------------------------------------------------------------------
who_calls(M, F, A) -> edts_xref:who_calls(M, F, A).


%%%_* Internal functions =======================================================

init_xref() ->
  File = xref_file(),
  try
    case file:read_file(File) of
      {ok, BinState}      -> edts_xref:start(binary_to_term(BinState));
      {error, enoent}     -> edts_xref:start();
      {error, _} = Error  ->
      error_logger:error_msg("Reading ~p failed with: ~p", [File, Error]),
      edts_xref:start()
    end
  catch
    C:E ->
      error_logger:error_msg("Starting xref from ~p failed with: ~p:~p~n~n"
                             "Starting with clean state instead.",
                             [File, C, E]),
      edts_xref:start()
  end.

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

update_xref() ->
  edts_xref:update(),
  spawn(?MODULE, save_xref_state, []).

save_xref_state() ->
  File = xref_file(),
  State = edts_xref:get_state(),
  case file:write_file(File, term_to_binary(State)) of
    ok -> ok;
    {error, _} = Error ->
      error_logger:error_msg("Failed to write ~p: ~p", [File, Error])
  end.

xref_file() ->
  {ok, XrefDir} = application:get_env(edts, project_dir),
  filename:join(XrefDir, atom_to_list(node()) ++ ".xref").

get_compile_outdir(File) ->
  Mod  = list_to_atom(filename:basename(filename:rootname(File))),
  Opts = try proplists:get_value(options, Mod:module_info(compile), [])
         catch error:undef -> []
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
%% @doc Reloads a module unless it is sticky.
-spec reload_module(M::module()) -> ok.
%%------------------------------------------------------------------------------
reload_module(M) ->
  case code:is_sticky(M) of
    true  -> ok;
    false ->
      case c:l(M) of
        {module, M}     -> ok;
        {error, _} = E ->
          error_logger:error_msg("~p error loading module ~p: ~p",
                                 [node(), M, E]),
          E
      end
  end.

%%------------------------------------------------------------------------------
%% @doc Format compiler errors and warnings.
-spec format_errors( ErrType ::warning | error
                   , Errors  ::[{ File::string(), [term()]}]) -> issue().
%%------------------------------------------------------------------------------
format_errors(Type, Errors) ->
   lists:append(
     [[{Type, File, Line, lists:flatten(Source:format_error(Error))}
       || {Line, Source, Error} <- Errors0]
         || {File, Errors0} <- Errors]).


%%------------------------------------------------------------------------------
%% @doc Get the file and line of a function from abstract code.
-spec get_file_and_line(M::module(), F::atom(), A::non_neg_integer(),
                        File::string(), Abstract::[term()])->
                           {ok, {File::string(), Line::non_neg_integer()}}.
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
-spec parse_abstract(Abstract::[term()], Acc::orddict:orddict()) ->
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
  FieldsF = fun({record_field, _, {_, _, FName}})        -> FName;
               ({record_field, _, {_, _, FName}, _Call}) -> FName
            end,
  RecordInfo =
    [ {record, Recordname}
    , {fields, lists:map(FieldsF, Fields)}
    , {line,   Line}
    , {source, orddict:fetch(cur_file, Acc)}],
  orddict:update(records, fun(Old) -> [RecordInfo|Old] end, Acc);
parse_abstract(_, Acc) -> Acc.


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
shorten_path_test_() ->
  [ ?_assertEqual("", shorten_path("")),
    ?_assertEqual(".", shorten_path(".")),
    ?_assertEqual("..", shorten_path("..")),
    ?_assertEqual("../..", shorten_path("../..")),
    ?_assertEqual("../ebin", shorten_path("../ebin")),
    ?_assertEqual("..", shorten_path("../ebin/..")),
    ?_assertEqual("..", shorten_path("../ebin/./.."))
  ].

format_errors_test_() ->
  SetupF = fun() ->
               meck:unload(),
               meck:new(source),
               meck:expect(source, format_error,
                           fun(Error) -> atom_to_list(Error) end)
           end,
  CleanupF = fun(_) ->
                 meck:unload()
             end,
  { setup, SetupF, CleanupF,
    [ ?_assertEqual( [ {warning, file1, 1337, "error1"},
                       {warning, file1, 1338, "error2"},
                       {warning, file2, 1339, "error3"},
                       {warning, file2, 1340, "error4"} ],
                     format_errors(warning,
                                   [ {file1, [ {1337, source, error1},
                                               {1338, source, error2}]},
                                     {file2, [ {1339, source, error3},
                                               {1340, source, error4}]}
                                   ])),
      ?_assertEqual( [ {error, file1, 1337, "error1"},
                       {error, file1, 1338, "error2"},
                       {error, file2, 1339, "error3"},
                       {error, file2, 1340, "error4"} ],
                     format_errors( error,
                                    [ {file1, [ {1337, source, error1},
                                                {1338, source, error2}]},
                                      {file2, [ {1339, source, error3},
                                                {1340, source, error4}]}
                                    ]))
    ]}.

get_file_and_line_test_() ->
  Forms = test_file_forms("non-parametrised-module"),
  File  = "non_parametrised_nodule.erl",
  Mod   = non_parametrised_nodule,
  [ ?_assertEqual( {error, not_found},
                   get_file_and_line(Mod, f, a, File, [])),
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
  ErlangAbsName = filename:join([code:lib_dir(erts, src), "erlang.erl"]),
  [?_assertEqual({error, not_found},
                 get_module_source_from_info(erlang:module_info())),
   ?_assertEqual({ok, ErlangAbsName},
                 get_module_source_from_beam(erlang)),
   ?_assertEqual({error, not_found},
                 get_module_source_from_info([])),
   ?_assertEqual({error, not_found},
                 get_module_source_from_beam(erlang_foo)),
   ?_assertEqual({ok, ErlangAbsName},
                get_module_source(erlang, erlang:module_info())),
   ?_assertEqual({error, not_found},
                get_module_source(erlang_foo, []))
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
  meck:new(Mod, [no_link]),
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
           , {record_field, foo, {foo, foo, field_2}, init_call}],
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
                              , {fields, [field_1, field_2]}
                              , {line,   Line}
                              , {source, CurFile}])
                 , lists:sort(hd(orddict:fetch(records, Res))))
  ].

parse_abstract_other_test_() ->
  [?_assertEqual(bar, parse_abstract(foo, bar))].

modules_test() ->
  [?_assertMatch({ok, [_|_]}, modules())].

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

%%%_* Test helpers =============================================================

test_file_forms(File) ->
  Path = filename:join([code:priv_dir(edts), "test/modules", File]),
  {ok, Bin} = file:read_file(Path),
  {ok, Forms} = edts_syntax:parse_forms(unicode:characters_to_list(Bin)),
  Forms.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

