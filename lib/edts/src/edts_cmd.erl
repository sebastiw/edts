%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc node resource
%%% @end
%%% @author Thomas Järvstrand <tjarvstrand@gmail.com>
%%% @copyright
%%% Copyright 2017 Thomas Järvstrand <tjarvstrand@gmail.com>
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
-module(edts_cmd).

%%%_* Exports ==================================================================

%% API
%% Webmachine callbacks
-export([run/2,
         plugin_run/3]).


%%%_* Includes =================================================================

%%%_* Defines ==================================================================
%%%_* Types ====================================================================
%%%_* API ======================================================================

%% FIXME Rename
run(Cmd, Input) ->
  Module = cmd_module(Cmd),
  case cmd_exists_p(Module) of
    true  -> do_run(Module, Input);
    false -> {error, {not_found, [{command, Cmd}]}}
  end.

%% FIXME Rename
plugin_run(Plugin, Cmd, Input0) ->
  Node = edts_util:make_nodename(orddict:fetch(node, Input0)),
  Input = orddict:erase(node, Input0),
  case plugin_cmd_exists_p(Plugin, Cmd, orddict:size(Input)) of
    true  ->
      plugin_do_run(Node, Plugin, Cmd, Input);
    false -> {error, {not_found, [{plugin, Plugin}, {command, Cmd}]}}
  end.

%%%_* Internal functions =======================================================

cmd_exists_p(Cmd) ->
  code:which(Cmd) =/= non_existing.

plugin_cmd_exists_p(Plugin, Cmd, Arity) ->
  code:which(Plugin) =/= non_existing andalso
    lists:member({Cmd, Arity}, Plugin:module_info(exports)).

cmd_module(Cmd) ->
  list_to_atom("edts_cmd_" ++ atom_to_list(Cmd)).

do_run(Cmd, Input) ->
  edts_log:debug("Validating input for command ~p:~n~p", [Cmd, Input]),
  {ok, Ctx} = edts_cmd_lib:validate(Input, Cmd:spec()),
  edts_log:debug("Running Command ~p with Ctx ~p", [Cmd, Input]),
  Result = Cmd:execute(Ctx),
  edts_log:debug("Command ~p returned ~p", [Cmd, Result]),
  Result.

plugin_do_run(Node, Plugin, Cmd, Input) ->
  edts_log:debug("Validating input for ~p command ~p:~n~p",
                 [Plugin, Cmd, Input]),
  Ctx = plugin_convert_params(Input, Plugin:spec(Cmd, orddict:size(Input))),
  edts_log:debug("Running ~p command ~p with Ctx ~p", [Plugin, Cmd, Input]),
  case edts:call(Node, Plugin, Cmd, Ctx) of
    %% The call terminated badly
    {error, E} ->
      {error, E};

    %% The call returned an error
    {ok, {error, E}} ->
      {ok, [{result, error}, {return, edts_plugins:to_ret_str(E)}]};

    %% All is well
    {ok, {ok, Ret}} ->
      {ok, [{result, ok}, {return, plugin_convert_return(Ret)}]};

    %% Local calls
    {ok, ok} ->
      {ok, [{result, ok}]};
    {ok, Ret} ->
      {ok, [{result, ok}, {return, plugin_convert_return(Ret)}]}
  end.

plugin_convert_params(Params, Specs) ->
  lists:map(fun({Key, Spec}) ->
                {ok, Val} = edts_util:assoc(Key, Params),
                plugin_convert_param(Val, Spec)
            end,
            Specs).

plugin_convert_param(Vs, [T])    -> [plugin_convert_param(V, T) || V <- Vs];
plugin_convert_param(V,  pid)    -> erlang:list_to_pid("<" ++ V ++ ">");
plugin_convert_param(V,  string) -> V;
plugin_convert_param(V,  T)      ->
  apply(erlang, list_to_atom("list_to_" ++ atom_to_list(T)), [V]).

plugin_convert_return(Ret) when is_list(Ret) ->
  IsProp = fun({K, _V}) when is_atom(K) -> true;
              (_)                       -> false
           end,
  case lists:all(IsProp, Ret) of
    true  -> [{K, plugin_convert_return(V)} || {K, V} <- Ret];
    false -> [plugin_convert_return(V) || V <- Ret]
  end;
plugin_convert_return(Ret) when is_tuple(Ret) -> edts_plugins:to_ret_str(Ret);
plugin_convert_return(Ret) when is_pid(Ret)   ->
  PidStr0 = pid_to_list(Ret),
  PidStr = string:sub_string(PidStr0, 2, length(PidStr0) - 1),
  list_to_binary(PidStr);
plugin_convert_return(Ret) ->
  Ret.


%%%_* Unit tests ===============================================================

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
