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

-export([ check_module/2
        , compile_and_load/1
        , get_function_info/3
        , get_module_info/1
        , get_module_info/2
        , modules/0
        , start/0
        , stop/0
        , who_calls/3]).

-export([ path_flatten/1]).

%%%_* Defines ==================================================================
-define(SERVER, ?MODULE).
%%%_* Types ====================================================================

%%%_* API ======================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Do an xref-analysis of Module, applying Checks
%% @end
-spec check_module(Module::module(), Checks::xref:analysis()) ->
                      {ok, [{ File::string()
                            , Line::non_neg_integer()
                            , Description::string()}]}.
%%------------------------------------------------------------------------------
check_module(Module, Checks) ->
  %% Fixme, what if module is not compiled and loaded?
  File = proplists:get_value(source, Module:module_info(compile)),
  lists:map(fun(Check) -> do_check_module(Module, File, Check) end, Checks).

do_check_module(Mod0, File, undefined_function_calls) ->
  {ok, Res} = xref:q(edts_code, "(XLin) (XC - UC)"),
  FmtFun = fun({{{Mod, _, _}, {CM, CF, CA}}, [Line]}, Acc) when Mod =:= Mod0 ->
               Desc = io_lib:format("Call to undefined function ~p:~p/~p",
                                    [CM, CF, CA]),
               [{error, File, Line, lists:flatten(Desc)}|Acc];
              (_, Acc) -> Acc
           end,
  lists:foldl(FmtFun, [], Res).

%%------------------------------------------------------------------------------
%% @doc
%% Equivalent to compile_and_load(Module, []).
%% @end
-spec compile_and_load(Module::file:filename() | module()) ->
                          {ok | error,
                           [{error | warning,
                             File::string(),
                             Line::non_neg_integer(),
                             Description::string()}]}.
%%------------------------------------------------------------------------------
compile_and_load(Module) ->
  compile_and_load(Module, []).

%%------------------------------------------------------------------------------
%% @doc
%% Compiles Module with Options and returns a list of any errors and warnings.
%% If there are no errors, the module will be loaded. Compilation options
%% always include [binary, return, debug_info]. Any options passed in will be
%% added to these.
%% @end
-spec compile_and_load(Module::file:filename()| module(), [compile:option()]) ->
                          {ok | error,
                           [{error | warning,
                             File::string(),
                             Line::non_neg_integer(),
                             Description::string()}]}.
%%------------------------------------------------------------------------------
compile_and_load(Module, Options) when is_atom(Module)->
  File = proplists:get_value(source, Module:module_info(compile)),
  compile_and_load(File, Options);
compile_and_load(File, Options0) ->
  AbsPath = filename:absname(File),
  Include = [filename:dirname(File)|get_include_dirs()],
  Out     = get_compile_outdir(File),
  Options = Options0 ++ [{outdir, Out}, return, debug_info, {i, Include}],
  case compile:file(AbsPath, Options) of
    {ok, Mod, Warnings} ->
      code:purge(Mod),
      OutFile = filename:join(Out, atom_to_list(Mod)),
      {module, Mod} = code:load_abs(OutFile),
      spawn(fun() ->
                case xref:replace_module(?SERVER, Mod, OutFile) of
                  {ok, Mod} -> ok;
                  {error, xref_base, {no_such_module, Mod}} ->
                    xref:add_module(?SERVER, OutFile)
                end,
                update()
            end),
      {ok, {[], format_errors(warning, Warnings)}};
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
  Info                         = M:module_info(),
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
-spec start() -> ok.
%%------------------------------------------------------------------------------
start() ->
  case xref:start(?SERVER) of
    {ok, _Pid}                       -> init();
    {error, {already_started, _Pid}} -> ok
  end,
  update().

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

%%------------------------------------------------------------------------------
%% @doc
%% Returns alist with all functions that call M:F/A on the local node.
%% @end
-spec who_calls(module(), atom(), non_neg_integer()) ->
                   [{module(), atom(), non_neg_integer()}].
%%------------------------------------------------------------------------------
who_calls(M, F, A) ->
  Str = lists:flatten(io_lib:format("(E || ~p)", [{M, F, A}])),
  {ok, Calls} = xref:q(edts_code, Str),
  [Caller || {Caller, _Callee} <- Calls].

%%%_* Internal functions =======================================================

get_compile_outdir(File) ->
  Mod = list_to_atom(filename:basename(filename:rootname(File))),
  try
    Opts = proplists:get_value(options, Mod:module_info(compile)),
    proplists:get_value(outdir, Opts)
  catch
    _:_ ->
      DirName = filename:dirname(File),
      EbinDir = filename:join([DirName, "..", "ebin"]),
      case filelib:is_file(EbinDir) of
        true  -> EbinDir;
        false -> DirName
      end
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Translates the code-path to a list if include-directories for compilation
%% @end
-spec get_include_dirs() -> [string()].
%%------------------------------------------------------------------------------

get_include_dirs() ->
  F = fun(Path) ->
          case filename:basename(Path) of
            "ebin" -> [ filename:join(filename:dirname(Path), "include")
                      , filename:join(filename:dirname(Path), "src")
                      , filename:join(filename:dirname(Path), "test")];
            _      -> Path
          end
      end,
  lists:flatmap(F, code:get_path()).


%%------------------------------------------------------------------------------
%% @doc Initializes the server.
-spec init() -> ok.
%%------------------------------------------------------------------------------
init() ->
  ok = xref:set_default(?SERVER, [{verbose,false}, {warnings,false}]),
  Paths = [Path || Path <- code:get_path(), filelib:is_dir(Path)],
  ok = xref:set_library_path(?SERVER, Paths),
  lists:foreach(fun(D) ->
                    case xref:add_application(?SERVER, filename:dirname(D)) of
                      {error, _, _} ->
                        case filelib:is_dir(D) of
                          true  ->
                            xref:add_directory(?SERVER, D);
                          false -> ok
                        end;
                      {ok, _}       -> ok
                    end
                end,
                Paths).

%%------------------------------------------------------------------------------
%% @doc Reloads a module unless it is sticky.
-spec reload_module(M::module()) -> ok.
%%------------------------------------------------------------------------------
reload_module(M) ->
  case code:is_sticky(M) of
    true  -> ok;
    false -> c:l(M)
  end,
  ok.

%%------------------------------------------------------------------------------
%% @doc Updates the xref server
-spec update() -> ok.
%%------------------------------------------------------------------------------
update() ->
  {ok, _Modules} = xref:update(?SERVER),
  xref:q(?SERVER, "E"),
  modules(),
  ok.

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
  FieldsF = fun({record_field, _, {_, _, FName}})         -> FName;
               ({record_field, _, {_, _, FName}, _Call}) -> FName
            end,
  RecordInfo =
    [ {name, Recordname}
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

path_flatten_test_() ->
  [ ?_assertEqual("./bar.erl",     path_flatten("bar.erl"))
  , ?_assertEqual("./bar.erl",     path_flatten("./bar.erl"))
  , ?_assertEqual("./bar.erl",     path_flatten("../foo/bar.erl"))
  , ?_assertEqual("./bar.erl",     path_flatten(".././foo/bar.erl"))
  , ?_assertEqual("./foo/bar.erl", path_flatten("bar/../foo/bar.erl"))
  , ?_assertEqual("../bar.erl",    path_flatten("bar/../foo/../../bar.erl"))
  , ?_assertEqual("./foo/bar.erl", path_flatten("./././foo//bar.erl"))
   ].

basic_module_info_test_() ->
  Info = get_module_info(test_module, basic),
  [ ?_assertEqual(test_module,proplists:get_value(module, Info))
  , ?_assertEqual([ [{function, bar},         {arity, 1}]
                  , [{function, module_info}, {arity, 0}]
                  , [{function, module_info}, {arity, 1}]],
                  proplists:get_value(exports, Info))
  , ?_assertMatch({{_, _, _}, {_, _, _}}, proplists:get_value(time, Info))
  , ?_assertMatch(Src when is_list(Src), proplists:get_value(source, Info))].

detailed_module_info_test_() ->
  Info = get_module_info(test_module),
  BaseDir = filename:dirname(proplists:get_value(source, module_info(compile))),
  [
    ?_assertEqual(test_module,proplists:get_value(module, Info))
  , ?_assertEqual([ [{function, bar},         {arity, 1}]
                  , [{function, module_info}, {arity, 0}]
                  , [{function, module_info}, {arity, 1}]],
                  proplists:get_value(exports, Info))
  , ?_assertMatch({{_, _, _}, {_, _, _}}, proplists:get_value(time, Info))
  , ?_assertMatch(Src when is_list(Src), proplists:get_value(source, Info))
  , ?_assertEqual(
       lists:sort([ [{module, lists}, {function, any},   {arity, 2}]
                  , [{module, lists}, {function, member}, {arity, 2}]])
     , lists:sort(proplists:get_value(imports, Info)))
  , ?_assertEqual(
       lists:sort([ [ {name, rec}
                    , {fields, [ord]}
                    , {line, 11}
                    , {source, proplists:get_value(source, Info)}]
                  , [ {name, rec2}
                    , {fields, [ord]}
                    , {line, 1}
                    , {source,
                       filename:join(
                         filename:dirname(proplists:get_value(source, Info)),
                         "test_2.hrl")}]])
     , lists:sort(proplists:get_value(records, Info)))
  , ?_assertEqual(
        [ filename:join([BaseDir, "test", "test.hrl"])
        , filename:join([BaseDir, "test", "test_2.hrl"])]
      , lists:sort(proplists:get_value(includes, Info)))
  ].

compile_and_load_test_() ->
  Path = code:where_is_file("test_module.erl"),
  [{ setup
   , fun()  -> ok = meck:new(compile, [unstick, passthrough]) end
   , fun(_) -> meck:unload() end
   , [ ?_assertMatch( {ok, {[], [{warning, Path, 11, _}]}}
                    , compile_and_load(Path))
     , ?_assertEqual(1, length(meck_history(compile, file)))]}].

get_function_info_test_() ->
  Info = get_function_info(test_module, bar, 1),
  [ ?_assertEqual(test_module, proplists:get_value(module,   Info))
  , ?_assertEqual(bar,         proplists:get_value(function, Info))
  , ?_assertEqual(1,           proplists:get_value(arity,    Info))
  , ?_assertEqual(16,          proplists:get_value(line,     Info))
  , ?_assertEqual(true,        proplists:get_value(exported, Info))].

modules_test() ->
  [{setup, fun start/0, fun(_) -> stop() end,
    [?_assertMatch({ok, [_|_]}, modules())]}].

reload_module_test() ->
  meck:new(c, [unstick, passthrough]),
  code:stick_mod(test_module),
  reload_module(test_module),
  ?assertEqual(0, length(meck_history(c, l))),
  code:unstick_mod(test_module),
  reload_module(test_module),
  ?assertEqual(1, length(meck_history(c, l))),
  meck:unload().

meck_history(M0, F0) ->
  [{A, R} || {Self, {M, F, A}, R} <- meck:history(M0), M =:= M0,
             F =:= F0, Self =:= self()].

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

