%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc xref-library for edts.
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

%%%_* Exports ==================================================================

-export([ compile_and_load/1
        , get_function_info/3
        , get_module_info/1
        , get_module_info/2
        , modules/0
        , start/0
        , who_calls/3]).

%%%_* Defines ==================================================================
-define(SERVER, ?MODULE).
%%%_* Types ====================================================================

%%%_* API ======================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Equivalent to compile_and_load(Module, []).
%% @end
-spec compile_and_load(Module::module()) ->
                          {ok, {module(), binary(), [term()]}}
                        | {ok, {module(), binary()}}
                        | {error, [term()], [term()]}.
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
-spec compile_and_load(Module::module(), [compile:option()]) ->
                          {ok, {module(), [term()]}}
                        | {error, {[term()], [term()]}}.
%%------------------------------------------------------------------------------
compile_and_load(Path, Options0) ->
  AbsPath = filename:absname(Path),
  Options = Options0 ++ [binary, return, debug_info, {i, get_include_dirs()}],
  case compile:file(AbsPath, Options) of
    {ok, Mod, Bin, Warnings} ->
      code:purge(Mod),
      {module, Mod} = code:load_binary(Mod, Path, Bin),
      {ok, format_errors(warning, Warnings)};
    {error, Errors, Warnings} ->
      {error, format_errors(error, Errors) ++ format_errors(warning, Warnings)}
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
  Basic            = get_module_info(M, basic),
  {source, Source} = lists:keyfind(source, 1, Basic),

  Acc0 = orddict:from_list([ {cur_file,  Source}
                           , {imports,   []}
                           , {includes,  []}
                           , {functions, []}
                           , {records,   []}]
                           ++ Basic),
  Dict0 = lists:foldl(fun parse_abstract/2, Acc0, Abstract),
  Dict1 = orddict:update(imports,  fun(I) -> lists:usort(I) end, Dict0),
  Dict2 = orddict:update(includes, fun(I) -> lists:usort(I) end, Dict1),
  Dict = orddict:erase(cur_file,  Dict2),
  orddict:to_list(Dict).

%%------------------------------------------------------------------------------
%% @doc
%% Returns a list of all modules known to the edts_code xref-server.
%% @end
-spec modules() -> [atom()].
%%------------------------------------------------------------------------------
modules() ->
  xref:update(?SERVER),
  xref:q(?SERVER, '".*" : Mod').

%%------------------------------------------------------------------------------
%% @doc
%% Starts the edts xref-server on the local node.
%% @end
-spec start() -> ok.
%%------------------------------------------------------------------------------
start() ->
  case xref:start(?SERVER) of
    {ok, _Pid}                       -> init();
    {error, {already_started, _Pid}} -> update()
  end,
  analyze().

who_calls(M, F, A) ->
  Str = lists:flatten(io_lib:format("(XXL)(Lin)(E || ~p)", [{M, F, A}])),
  case xref:q(edts_code, Str) of
    {ok, []} -> [];
    {ok, Calls} ->
      [Caller || {{{Caller, _}, {_Callee, _}}, _} <- Calls]
  end.

%%%_* Internal functions =======================================================

%%------------------------------------------------------------------------------
%% @doc
%% Translates the code-path to a list if include-directories for compilation
%% @end
-spec get_include_dirs() -> [string()].
%%------------------------------------------------------------------------------

get_include_dirs() ->
  F = fun(Path) ->
          case filename:basename(Path) of
            "ebin" -> filename:join(filename:dirname(Path), "include");
            _      -> Path
          end
      end,
  lists:map(F, code:get_path()).


%%------------------------------------------------------------------------------
%% @doc Initializes the server.
-spec init() -> ok.
%%------------------------------------------------------------------------------
init() ->
  ok = xref:set_default(?SERVER, [{verbose,false}, {warnings,false}]),
  Paths = code:get_path(),
  ok = xref:set_library_path(?SERVER, Paths),
  lists:foreach(fun(D) ->
                    case xref:add_application(?SERVER, filename:dirname(D)) of
                      {error, _, _} -> xref:add_directory(?SERVER, D);
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
  ok.

%% Query the server to cache values.
analyze() ->
  modules().

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
   [[{Type, File, Line, lists:flatten(Source:format_error(Error))}
     || {Line, Source, Error} <- Errors0]
       || {File, Errors0} <- Errors].


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
    , {source,   orddict:fetch(source, Acc)}
    , {line,     Line}],
  orddict:update(functions, fun(Fs) -> [FunInfo|Fs] end, Acc);
parse_abstract({attribute, _Line0, file, {Source0, _Line1}}, Acc0) ->
  %% %% Get rid of any local paths, in case function was defined in a
  %% %% file include with a relative path.
  BeamSource = orddict:fetch(source, Acc0),
  Source =
    case filename:pathtype(Source0) of
      absolute -> Source0;
      relative -> filename:join(filename:dirname(BeamSource), Source0)
    end,
  %% Update list of all files.
  Acc =
    case Source of
      BeamSource -> Acc0;
      Source     -> orddict:update(includes, fun(I) -> [Source|I] end, Acc0)
    end,
  %% Update current file.
  orddict:store(cur_file, Source, Acc);
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
    [ {name,   Recordname}
      , {fields, lists:map(FieldsF, Fields)}
      , {line,   Line}
      , {source, orddict:fetch(source, Acc)}],
  orddict:update(records, fun(Old) -> [RecordInfo|Old] end, Acc);
parse_abstract(_, Acc) -> Acc.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

