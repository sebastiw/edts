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
-module(edts_plugins).

%%%_* Includes =================================================================
%%%_* Exports ==================================================================

-export([execute/3,
         dirs/0,
         names/0,
         specs/0,
         to_ret_str/1,
         to_ret_str/3
        ]).

%% Callbacks
-export([edts_server_services/1,
         event_formatters/1,
         project_node_modules/1,
         project_node_services/1]).

%%%_* Defines ==================================================================

%%%_* Types ====================================================================

-type ctx() :: edts_cmd:ctx().

-type argument() :: {Name :: atom(), Type :: atom()}.

-type function_name() :: atom().

%%%_* Behaviour callbacks ======================================================

-callback edts_server_services() -> [].

-callback event_formatters() -> [{edts_debug, edts_events_debug}].

-callback project_node_modules() -> [module()].

-callback project_node_services() -> [].

-callback spec(function_name(), arity()) -> [argument()].

%%%_* API ======================================================================

-spec execute(Plugin::module(), Cmd::atom(), ctx()) ->
          {ok, [{result, ok | error} |
                {return, [{atom(), binary()}]}]} |
          {error, {badrpc, term()}} |
          {error, {not_found, [{plugin, Plugin::module()} |
                               {command, Cmd::atom()}]}}.

execute(Plugin, Cmd, Input0) ->
  Node = edts_util:make_nodename(orddict:fetch(node, Input0)),
  Input = orddict:erase(node, Input0),
  case cmd_exists_p(Plugin, Cmd, orddict:size(Input)) of
    true  ->
      do_execute(Node, Plugin, Cmd, Input);
    false -> {error, {not_found, [{plugin, Plugin}, {command, Cmd}]}}
  end.

dirs() ->
  case application:get_env(edts, plugin_dir) of
    undefined -> [];
    {ok, Dir} ->
      AbsDir = filename:absname(Dir),
      PluginDirs = filelib:wildcard(filename:join(AbsDir, "*")),
      [PluginDir || PluginDir <- PluginDirs,
                    filelib:is_dir(PluginDir),
                    "edts" /= filename:basename(PluginDir)]
  end.

names() ->
  [Name || Dir <- dirs(),
           edts /= (Name = list_to_atom(filename:basename(Dir)))].

specs() ->
  lists:map(fun do_spec/1, dirs()).

to_ret_str(Term) ->
  list_to_binary(lists:flatten(io_lib:format("~p", [Term]))).

to_ret_str(Term, Indent, MaxCol) ->
  RecF = fun(_A, _N) -> no end,
  Str = lists:flatten(io_lib_pretty:print(Term, Indent, MaxCol, -1, -1, RecF)),
  list_to_binary(Str).

%% Callbacks
edts_server_services(Plugin) ->
  Plugin:edts_server_services().

event_formatters(Plugin) ->
  Plugin:event_formatters().

project_node_modules(Plugin) ->
  Plugin:project_node_modules().

project_node_services(Plugin) ->
  Plugin:project_node_services().

spec(Plugin, Cmd, Arity) ->
    Plugin:spec(Cmd, Arity).

%%%_* Internal functions =======================================================

cmd_exists_p(Plugin, Cmd, Arity) ->
  code:which(Plugin) =/= non_existing andalso
    lists:member({Cmd, Arity}, Plugin:module_info(exports)).

do_execute(Node, Plugin, Cmd, Input) ->
  edts_log:debug("Validating input for ~p command ~p:~n~p",
                 [Plugin, Cmd, Input]),
  Ctx = convert_params(Input, spec(Plugin, Cmd, orddict:size(Input))),
  edts_log:debug("Running ~p command ~p with Ctx ~p", [Plugin, Cmd, Input]),
  case edts:call(Node, Plugin, Cmd, Ctx) of
    %% The call terminated badly
    {error, E} ->
      {error, E};

    %% The call returned an error
    {ok, {error, E}} ->
      {ok, [{result, error}, {return, to_ret_str(E)}]};

    %% All is well
    {ok, {ok, Ret}} ->
      {ok, [{result, ok}, {return, convert_return(Ret)}]};

    %% Local calls
    {ok, ok} ->
      {ok, [{result, ok}]};
    {ok, Ret} ->
      {ok, [{result, ok}, {return, convert_return(Ret)}]}
  end.

convert_params(Params, Specs) ->
  lists:map(fun({Key, Spec}) ->
                {ok, Val} = edts_util:assoc(Key, Params),
                convert_param(Val, Spec)
            end,
            Specs).

convert_param(Vs, [T])    -> [convert_param(V, T) || V <- Vs];
convert_param(V,  pid)    -> erlang:list_to_pid("<" ++ V ++ ">");
convert_param(V,  string) -> V;
convert_param(V,  T)      ->
  apply(erlang, list_to_atom("list_to_" ++ atom_to_list(T)), [V]).

convert_return(Ret) when is_list(Ret) ->
  IsProp = fun({K, _V}) when is_atom(K) -> true;
              (_)                       -> false
           end,
  case lists:all(IsProp, Ret) of
    true  -> [{K, convert_return(V)} || {K, V} <- Ret];
    false -> [convert_return(V) || V <- Ret]
  end;
convert_return(Ret) when is_tuple(Ret) -> to_ret_str(Ret);
convert_return(Ret) when is_pid(Ret)   ->
  PidStr0 = pid_to_list(Ret),
  PidStr = string:sub_string(PidStr0, 2, length(PidStr0) - 1),
  list_to_binary(PidStr);
convert_return(Ret) ->
  Ret.

do_spec(Dir) ->
  [AppFile] = filelib:wildcard(filename:join([Dir, "src", "*.app.src"])),
  {ok, [AppSpec]} = file:consult(AppFile),
  AppSpec.

%%%_* Unit tests ===============================================================

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

