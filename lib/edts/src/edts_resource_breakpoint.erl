%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Breakpoint resource
%%% @end
%%% @author João Neves <sevenjp@gmail.com>
%%% @copyright
%%% Copyright 2012 João Neves <sevenjp@gmail.com>
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
-module(edts_resource_breakpoint).

%%%_* Exports ==================================================================

%% API
%% Webmachine callbacks
-export([ allow_missing_post/2
        , allowed_methods/2
        , content_types_accepted/2
        , content_types_provided/2
        , create_path/2
        , init/1
        , malformed_request/2
        , post_is_create/2
        , resource_exists/2]).

%% Handlers
-export([ from_json/2
        , to_json/2 ]).

%%%_* Includes =================================================================
-include_lib("webmachine/include/webmachine.hrl").

%%%_* Defines ==================================================================
%%%_* Types ====================================================================
%%%_* API ======================================================================


%% Webmachine callbacks
init(_Config) ->
  lager:debug("Call to ~p", [?MODULE]),
  {ok, orddict:new()}.

allow_missing_post(ReqData, Ctx) ->
  {true, ReqData, Ctx}.

allowed_methods(ReqData, Ctx) ->
  {['GET', 'POST'], ReqData, Ctx}.

content_types_accepted(ReqData, Ctx) ->
  Map = [ {"application/json", from_json} ],
  {Map, ReqData, Ctx}.

content_types_provided(ReqData, Ctx) ->
  Map = [ {"application/json", to_json}
        , {"text/html",        to_json}
        , {"text/plain",       to_json} ],
  {Map, ReqData, Ctx}.

create_path(ReqData, Ctx) ->
  {wrq:path(ReqData), ReqData, Ctx}.

malformed_request(ReqData, Ctx) ->
  Validate = case wrq:method(ReqData) of
               'GET'  -> [nodename];
               'POST' -> [nodename, module, line]
             end,
  edts_resource_lib:validate(ReqData, Ctx, Validate).

post_is_create(ReqData, Ctx) ->
  {true, ReqData, Ctx}.

resource_exists(ReqData, Ctx) ->
  Exists   = edts_resource_lib:exists_p(ReqData, Ctx, [nodename]),
  {Exists, ReqData, Ctx}.

%% Handlers
from_json(ReqData, Ctx) ->
  Nodename = orddict:fetch(nodename, Ctx),
  Module   = orddict:fetch(module, Ctx),
  Line     = orddict:fetch(line, Ctx),
  {ok, Result, {Module, Line}} = edts:toggle_breakpoint(Nodename, Module, Line),
  Data = {struct, [{result, Result}, {module, Module}, {line, Line}]},
  {true, wrq:set_resp_body(mochijson2:encode(Data), ReqData), Ctx}.

to_json(ReqData, Ctx) ->
  Nodename = orddict:fetch(nodename, Ctx),
  {ok, Breakpoints} = edts:get_breakpoints(Nodename),
  Data = {array, [format(B) || B <- Breakpoints]},
  {mochijson2:encode(Data), ReqData, Ctx}.

%%%_* Internal functions =======================================================
format({{Module, Line}, [Status, Trigger, null, Condition]}) ->
  {struct, [ {module, Module}
           , {line, Line}
           , {status, Status}
           , {trigger, Trigger}
           , {condition, io_lib:format("~p", [Condition])}
           ]}.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

