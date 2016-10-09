%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Debugger resource
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
-module(edts_resource_plugin_call).

%%%_* Exports ==================================================================

%% API
%% Webmachine callbacks
-export([allowed_methods/2,
         allow_missing_post/2,
         init/1,
         malformed_request/2,
         process_post/2,
         resource_exists/2]).


%%%_* Includes =================================================================
-include_lib("webmachine/include/webmachine.hrl").

%%%_* Defines ==================================================================
-define(a2l(A), atom_to_list(A)).
-define(l2a(A), list_to_atom(A)).
%%%_* Types ====================================================================
%%%_* API ======================================================================


%% Webmachine callbacks
init(_Config) ->
  edts_log:debug("Call to ~p", [?MODULE]),
  {ok, orddict:new()}.

allowed_methods(ReqData, Ctx) ->
  {['POST'], ReqData, Ctx}.

malformed_request(ReqData, Ctx0) ->
  try
    Node    = edts_util:make_nodename(wrq:path_info(nodename, ReqData)),
    Plugin  = ?l2a(wrq:path_info(plugin, ReqData)),
    Method  = ?l2a((wrq:path_info(method, ReqData))),
    Params0 = wrq:req_qs(ReqData),
    Arity   = length(Params0),
    Params  = convert_params(Params0, Plugin:spec(Method, Arity)),
    Ctx = [{node,   Node},
           {plugin, Plugin},
           {method, Method},
           {params, Params}],
    {false, ReqData, orddict:from_list(Ctx)}
  catch
    _ -> {true, ReqData, Ctx0}
  end.

resource_exists(ReqData, Ctx) ->
  try
    assert(node, edts_server:node_registered_p(orddict:fetch(node, Ctx))),

    Plugin = orddict:fetch(plugin, Ctx),
    assert(plugin, lists:member(Plugin, edts_plugins:names())),

    c:l(Plugin),
    Method = orddict:fetch(method, Ctx),
    Arity = length(orddict:fetch(params, Ctx)),
    assert(method, erlang:function_exported(Plugin, Method, Arity)),
    {true, ReqData, Ctx}
  catch
    _ -> {false, ReqData, Ctx}
  end.

allow_missing_post(ReqData, Ctx) ->
  {false, ReqData, Ctx}.

process_post(ReqData, Ctx) ->
  Node      = orddict:fetch(node, Ctx),
  Plugin    = orddict:fetch(plugin, Ctx),
  Method    = orddict:fetch(method, Ctx),
  Params    = orddict:fetch(params, Ctx),
  Reply =
    case edts:call(Node, Plugin, Method, Params) of
      %% The call terminated badly
      {error, E}       -> [{result, error},
                           {return, edts_plugins:to_ret_str(E)}];
      %% The call returned an error
      {ok, {error, E}} -> [{result, error},
                           {return, edts_plugins:to_ret_str(E)}];
      %% All is well
      {ok, {ok, Ret}}  -> [{result, ok},
                           {return, convert_return(Ret)}];
      {ok, Ret}         -> [{result, ok},
                            {return, convert_return(Ret)}]
    end,
  {true, wrq:set_resp_body(mochijson2:encode(Reply), ReqData), ReqData}.


%%%_* Internal functions =======================================================

assert(_, true)     -> ok;
assert(Prop, false) -> throw(Prop).

convert_params(Params, Specs) ->
  lists:map(fun({Key, Spec}) ->
                {ok, Val} = edts_util:assoc(?a2l(Key), Params),
                convert_param(Val, Spec)
            end,
            Specs).

convert_param(Vs, [T])    -> [ convert_param(V, T) ||
                               V <- string:tokens(Vs, ",")];
convert_param(V,  pid)    -> erlang:list_to_pid("<" ++ V ++ ">");
convert_param(V,  string) -> V;
convert_param(V,  T)      -> apply(erlang, ?l2a("list_to_" ++ ?a2l(T)), [V]).

convert_return(Ret) when is_list(Ret) ->
  IsProp = fun({K, _V}) when is_atom(K) -> true;
              (_)                      -> false
           end,
  case lists:all(IsProp, Ret) of
    true  -> [{K, convert_return(V)} || {K, V} <- Ret];
    false -> [convert_return(V) || V <- Ret]
  end;
convert_return(Ret) when is_tuple(Ret) -> edts_plugins:to_ret_str(Ret);
convert_return(Ret) when is_pid(Ret)   ->
  PidStr0 = pid_to_list(Ret),
  PidStr = string:sub_string(PidStr0, 2, length(PidStr0) - 1),
  list_to_binary(PidStr);
convert_return(Ret) ->
  Ret.

%%%_* Unit tests ===============================================================
%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
