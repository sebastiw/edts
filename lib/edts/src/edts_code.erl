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
         start/0,
         who_calls/3]).

%% internal exports
-export([save_xref_state/0]).

%%%_* Defines ==================================================================
-define(SERVER, ?MODULE).

%%%_* Types ====================================================================
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
add_path(Path) -> code:add_path(shorten_path(Path)).

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
%% the module will be written to file by EDTS instead).
%% Any other options passed in will be appended to the above.
%% @end
-spec compile_and_load(Module::file:filename()| module(), [compile:option()]) ->
                          {ok | error, [issue()]}.
%%------------------------------------------------------------------------------
compile_and_load(Module, Opts) when is_atom(Module)->
  File = proplists:get_value(source, Module:module_info(compile)),
  compile_and_load(File, Opts);
compile_and_load(File, Opts) ->
  AbsPath  = filename:absname(File),
  %% Only compile to a binary to begin with since compile-options resulting in
  %% an output-file will cause the compile module to remove the existing beam-
  %% file even if compilation fails, in which case we end up with no module
  %% at all for other analyses (xref etc.).
  case compile:file(AbsPath, [binary, debug_info, return|Opts]) of
    {ok, Mod, Bin, Warnings} ->
      WasInterpreted = maybe_uninterpret_module(Mod),
      OutDir = get_compile_outdir(File),
      OutFile = filename:join(OutDir, atom_to_list(Mod)),
      Result = case file:write_file(OutFile ++ ".beam", Bin) of
                 ok ->
                   code:purge(Mod),
                   {module, Mod} = code:load_abs(OutFile),
                   update_xref(),
                   add_path(OutDir),
                   {ok, {[], format_errors(warning, Warnings)}};
                 {error, _} = Err ->
                   error_logger:error_msg("(~p) Failed to write ~p: ~p",
                                          [node(), OutFile, Err]),
                   Err
               end,
      maybe_interpret_module(Mod, WasInterpreted),
      Result;
    {error, Errors, Warnings} ->
      {error, {format_errors(error, Errors), format_errors(warning, Warnings)}}
  end.

maybe_uninterpret_module(Module) ->
  case edts_debug_server:is_interpreted(Module) of
    true -> edts_debug_server:uninterpret_module(Module),
            true;
    _    -> false
  end.

maybe_interpret_module(Module, true) ->
  edts_debug_server:interpret_modules([Module]);
maybe_interpret_module(_Module, _WasInterpreted) ->
  ok.

%%------------------------------------------------------------------------------
%% @doc
%% Returns information about Function as defined in Module.
%% @end
-spec get_function_info(M::module(), F0::atom(), A0::non_neg_integer()) ->
                           [{atom(), term()}].
%%------------------------------------------------------------------------------
get_function_info(M, F0, A0) ->
  reload_module(M),
  {M, Bin, _File}                   = code:get_object_code(M),
  {ok, {M, Chunks}}                 = beam_lib:chunks(Bin, [abstract_code]),
  {abstract_code, {_Vsn, Abstract}} = lists:keyfind(abstract_code, 1, Chunks),
  OrigSource = proplists:get_value(source, M:module_info(compile)),
  case get_file_and_line(M, F0, A0, OrigSource, Abstract) of
    {error, _} = Err   -> Err;
    {ok, {File, Line}} ->
      %% Get rid of any local paths, in case function was defined in a
      %% file include with a relative path.
      Source =
        case filename:pathtype(File) of
          absolute -> File;
          relative ->
            % Deals with File = "./src". Must be a better way to do this.
            case lists:suffix(File, OrigSource) of
              true  -> OrigSource;
              false ->
                filename:join(filename:dirname(OrigSource), File)
            end
        end,
      [ {module,   M}
      , {function, F0}
      , {arity,    A0}
      , {exported, lists:member({F0, A0}, M:module_info(exports))}
      , {source,   Source}
      , {line,     Line}]
  end.

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
  %% StartLine/EndLine may be useful in error messages.
  {ok, Ts, EndLine} = erl_scan:string(Text, StartLine),
  %%Ts1 = reverse(strip(reverse(Ts))),
  Ts2 = [{'begin', 1}] ++ Ts ++ [{'end', EndLine}, {dot, EndLine}],
  case erl_parse:parse_exprs(Ts2) of
    {ok, Es} ->
      E = erl_syntax:block_expr(Es),
      E1 = erl_syntax_lib:annotate_bindings(E, ordsets:new()),
      {value, {free, Vs}} =
        lists:keysearch(free, 1, erl_syntax:get_ann(E1)),
      {ok, Vs};
    {error, {_Line, erl_parse, _Reason} = Err} ->
        {error, format_errors(error, {"Snippet", [Err]})}
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
  [ {module, M}
  , {exports, [[{function, F}, {arity, A}] || {F, A} <- Exports]}
  , {time, {{Y, Mo, D}, {H, Mi, S}}}
  , lists:keyfind(source,  1, Compile)];
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
-spec start() -> {ok , node()} | {error, already_started}.
%%------------------------------------------------------------------------------
start() ->
  File = xref_file(),
  try
    case file:read_file(File) of
      {ok, BinState} ->
        edts_xref:start(binary_to_term(BinState));
      {error, enoent}     -> edts_xref:start();
      {error, _} = Error  ->
        error_logger:error_msg("Reading ~p failed with: ~p", [File, Error])
    end,
    {node(), ok}
  catch
    C:E ->
      error_logger:error_msg("Starting xref from ~p failed with: ~p:~p",
                             [File, C, E]),
      case edts_xref:started_p() of
        true  -> ok;
        false -> edts_xref:start()
      end
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Returns alist with all functions that call M:F/A on the local node.
%% @end
-spec who_calls(module(), atom(), non_neg_integer()) ->
                   [{module(), atom(), non_neg_integer()}].
%%------------------------------------------------------------------------------
who_calls(M, F, A) -> edts_xref:who_calls(M, F, A).

%%%_* Internal functions =======================================================

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
-spec format_errors( ErrType % warning | error
                   , Errors::[{ File::string(), [term()]}]) ->
                       [{ErrType,
                         File::string(),
                         Line::non_neg_integer(),
                         Description::string()}].
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
  Src =
    case filename:pathtype(Src0) of
      absolute ->
        path_flatten(Src0);
      relative ->
        path_flatten(filename:join(orddict:fetch(compile_cwd, Acc0), Src0))
    end,
  %% Update list of all files.
  Acc =
    case Src of
      BeamSource -> Acc0;
      Src        -> orddict:update(includes, fun(I) -> [Src|I] end, Acc0)
    end,
  %% Update current file.
  orddict:store(cur_file, Src, Acc);
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
  { setup, SetupF, CleanupF
  , [ ?_assertEqual( [ {warning, file1, 1337, "error1"}
                     , {warning, file1, 1338, "error2"}
                     , {warning, file2, 1339, "error3"}
                     , {warning, file2, 1340, "error4"} ]
                   , format_errors( warning
                                  , [ {file1, [ {1337, source, error1}
                                              , {1338, source, error2}]}
                                    , {file2, [ {1339, source, error3}
                                              , {1340, source, error4}]} ]))
    , ?_assertEqual( [ {error, file1, 1337, "error1"}
                     , {error, file1, 1338, "error2"}
                     , {error, file2, 1339, "error3"}
                     , {error, file2, 1340, "error4"} ]
                   , format_errors( error
                                  , [ {file1, [ {1337, source, error1}
                                              , {1338, source, error2}]}
                                    , {file2, [ {1339, source, error3}
                                              , {1340, source, error4}]} ]))
    ]}.

get_file_and_line_test_() ->
  [ ?_assertEqual({error, not_found}, get_file_and_line(m, f, a, "foo.erl", []))
  , ?_assertEqual( {ok, {"foo.erl", 1337}}
                 , get_file_and_line(m, f, 0, "foo.erl",
                                     [{function, 1337, f, 0, ''}]))
  , ?_assertEqual( {ok, {"bar.erl", 1337}}
                 , get_file_and_line( m, f, 0, "foo.erl"
                                    , [ {attribute, '', file, {"bar.erl", ''}}
                                      , {function, 1337, f, 0, ''}]))
  , ?_assertEqual( {ok, {"foo.erl", 1337}}
                 , get_file_and_line(m, f, 0, "foo.erl",
                                     [ {function, 1335, f0, 0, ''}
                                     , {function, 1337, f, 0, ''}]))
  ].

path_flatten_test_() ->
  [ ?_assertEqual("./bar.erl",     path_flatten("bar.erl"))
  , ?_assertEqual("./bar.erl",     path_flatten("./bar.erl"))
  , ?_assertEqual("./bar.erl",     path_flatten("../foo/bar.erl"))
  , ?_assertEqual("./bar.erl",     path_flatten(".././foo/bar.erl"))
  , ?_assertEqual("./foo/bar.erl", path_flatten("bar/../foo/bar.erl"))
  , ?_assertEqual("../bar.erl",    path_flatten("bar/../foo/../../bar.erl"))
  , ?_assertEqual("./foo/bar.erl", path_flatten("./././foo//bar.erl"))
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
  Acc = orddict:from_list([ {module,    Mod}
                          , {cur_file,  CurFile}
                          , {exports,   Exports}
                          , {functions, []}]),
  Res = parse_abstract({function, Line, Fun, Arity, []}, Acc),
  meck:unload(),
  [ ?_assertEqual(
       lists:sort([module, cur_file, exports, functions]),
       lists:sort(orddict:fetch_keys(Res)))
  , ?_assertEqual(Mod,     orddict:fetch(module, Res))
  , ?_assertEqual(CurFile, orddict:fetch(cur_file, Res))
  , ?_assertEqual(Exports, orddict:fetch(exports, Res))
  , ?_assertMatch([_],     orddict:fetch(functions, Res))
  , ?_assertEqual(lists:sort([ {module, Mod}
                             , {function, Fun}
                             , {arity,    Arity}
                             , {source,   CurFile}
                             , {line,     Line}
                             , {exported, true}])
                  , lists:sort(hd(orddict:fetch(functions, Res))))
  ].

parse_abstract_include_test_() ->
  Line = 1337,
  Source0 = "/foo/test.erl",
  Source1 = "test.erl",
  Include0 = "test.hrl",
  CompileCwd = "/foo",
  Acc = orddict:from_list([ {source,      Source0}
                          , {compile_cwd, CompileCwd}
                          , {includes,    []}]),
  Res0 = parse_abstract({attribute, Line, file, {Source0, Line}}, Acc),
  Res1 = parse_abstract({attribute, Line, file, {Source1, Line}}, Acc),
  Res2 = parse_abstract({attribute, Line, file, {Include0, Line}}, Acc),
  [ ?_assertEqual(
       lists:sort([source, compile_cwd, includes, cur_file])
     , lists:sort(orddict:fetch_keys(Res0)))
  , ?_assertEqual(Source0, orddict:fetch(source, Res0))
  , ?_assertEqual(CompileCwd, orddict:fetch(compile_cwd, Res0))
  , ?_assertEqual([], orddict:fetch(includes, Res0))
  , ?_assertEqual([], orddict:fetch(includes, Res1))
  , ?_assertEqual(["/foo/test.hrl"], orddict:fetch(includes, Res2))
  ].

parse_abstract_import_test_() ->
  Acc = orddict:from_list([ {imports, []}]),
  Res0 = parse_abstract({attribute, 0, import, {foo, [{bar, 1}]}}, Acc),
  [ ?_assertEqual([imports], orddict:fetch_keys(Res0))
  , ?_assertMatch([_], orddict:fetch(imports, Res0))
  , ?_assertEqual( lists:sort([ {module,   foo}
                              , {function, bar}
                              , {arity,    1}])
                 , lists:sort(hd(orddict:fetch(imports, Res0))))
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
  [{ setup
   , fun () ->
         meck:new(filelib, [passthrough, unstick]),
         meck:expect(filelib, is_dir, fun ("good/../ebin") -> true;
                                          (_)              -> false
                                       end)
     end
   , fun (_) -> meck:unload() end
   , [ ?_assertEqual(Good , F("foo/mod.erl" , [{outdir, Good}]))
     , ?_assertEqual(Good , F("good/mod.erl", [{outdir, "foo"}]))
     , ?_assertEqual("foo", F("foo/mod.erl" , []))
     , ?_assertEqual(Good , F("good/mod.erl", []))
     ]
   }].

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

