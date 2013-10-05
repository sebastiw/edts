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
-include_lib("tulib/include/prelude.hrl").
-include_lib("webmachine/include/webmachine.hrl").

%%%_* Defines ==================================================================
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
    Body = case wrq:req_body(ReqData) of
             undefined -> throw(badarg);
             Body0     -> decode(Body0)
           end,
    Ctx = [{plugin, ?l2a(wrq:path_info(plugin, ReqData))},
           {node,   edts_util:shortname2nodename(body_fetch(node, Body))},
           {method, ?l2a(body_fetch(method, Body))},
           {params, case body_fetch(params, Body) of
                      null -> [];
                      Params -> Params
                    end},
           {id,     body_fetch(id,     Body)}],
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

assert(_, true)     -> ok;
assert(Prop, false) -> throw(Prop).

allow_missing_post(ReqData, Ctx) ->
  {false, ReqData, Ctx}.

process_post(ReqData, Ctx) ->
  Node      = orddict:fetch(node, Ctx),
  Plugin    = orddict:fetch(plugin, Ctx),
  Method    = orddict:fetch(method, Ctx),
  Params    = orddict:fetch(params, Ctx),
  Reply = [{id, orddict:fetch(id, Ctx)}] ++
    case edts:call(Node, Plugin, Method, Params) of
      {ok, Ret}  -> [{result, Ret}, {errors, null}];
      {error, E} -> [{result, null}, {errors, [E]}]
      end,
  {true, wrq:set_resp_body(mochijson2:encode(Reply), ReqData), ReqData}.


%%%_* Internal functions =======================================================

body_fetch(Prop, Body) ->
  case tulib_lists:assoc(Prop, Body) of
    {ok, Val}         -> Val;
    {error, notfound} -> throw(Prop)
  end.

decode(Obj) ->
  do_decode(mochijson2:decode(Obj)).


do_decode(String) when is_binary(String) -> binary_to_list(String);
do_decode(Array)  when is_list(Array)    -> [do_decode(El) || El <- Array];
do_decode({struct, Els})                 ->
  [{?l2a(?b2l(K)), do_decode(V)} || {K, V} <- Els];
do_decode(Obj) ->
  Obj.

%%%_* Unit tests ===============================================================
%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
