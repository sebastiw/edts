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
-module(edts_debug_resource_cmd).

%%%_* Exports ==================================================================

%% API
%% Webmachine callbacks
-export([ allowed_methods/2
        , content_types_accepted/2
        , init/1
        , malformed_request/2
        , resource_exists/2]).

%% Handlers
-export([ from_json/2]).

%%%_* Includes =================================================================
-include_lib("webmachine/include/webmachine.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%_* Defines ==================================================================

%%%_* Types ====================================================================
%%%_* API ======================================================================


%% Webmachine callbacks
init(_Config) ->
  lager:debug("Call to ~p", [?MODULE]),
  {ok, orddict:new()}.

allowed_methods(ReqData, Ctx) ->
  {['POST'], ReqData, Ctx}.

content_types_accepted(ReqData, Ctx) ->
  Map = [ {"application/json", from_json} ],
  {Map, ReqData, Ctx}.

malformed_request(ReqData, Ctx) ->
  Validate = [nodename, modulen],
  edts_resource_lib:validate(ReqData, Ctx, Validate).

resource_exists(ReqData, Ctx) ->
  {edts_resource_lib:exists_p(ReqData, Ctx, [nodename, module]), ReqData, Ctx}.

%% Handlers
from_json(_ReqData, Ctx) ->
  Node = orddict:fetch(nodename, Ctx),
  Cmd  = orddict:fetch(cmd, Ctx),
  edts_debug:Cmd(Node).




%%%_* Internal functions =======================================================


%%%_* Unit tests ===============================================================
init_test() ->
  ?assertEqual({ok, orddict:new()}, init(foo)).

allowed_methods_test() ->
  ?assertEqual({['GET', 'POST'], foo, bar}, allowed_methods(foo, bar)).

content_types_accepted_test() ->
  ?assertEqual({[ {"application/json", from_json} ], foo, bar},
               content_types_accepted(foo, bar)).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
