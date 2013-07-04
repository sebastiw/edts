%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Debugger resource
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
-module(edts_resource_debug_module).

%%%_* Exports ==================================================================

%% API
%% Webmachine callbacks
-export([ allowed_methods/2
        , allow_missing_post/2
        , content_types_accepted/2
        , content_types_provided/2
        , create_path/2
        , forbidden/2
        , init/1
        , malformed_request/2
        , post_is_create/2
        , resource_exists/2]).

%% Handlers
-export([ from_json/2, to_json/2]).

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
  {['GET', 'POST'], ReqData, Ctx}.

allow_missing_post(ReqData, Ctx) ->
  {true, ReqData, Ctx}.

content_types_accepted(ReqData, Ctx) ->
  Map = [ {"application/json", from_json} ],
  {Map, ReqData, Ctx}.

content_types_provided(ReqData, Ctx) ->
  Map = [ {"application/json", to_json}
        , {"text/html",        to_json}
        , {"text/plain",       to_json}],
  {Map, ReqData, Ctx}.

create_path(ReqData, Ctx) ->
  {wrq:path(ReqData), ReqData, Ctx}.

forbidden(ReqData, Ctx) ->
  Node                = orddict:fetch(nodename, Ctx),
  Module              = orddict:fetch(module, Ctx),
  {ok, Interpretable} = edts_debug:module_interpretable_p(Node, Module),
  {not Interpretable, ReqData, Ctx}.

malformed_request(ReqData, Ctx) ->
  Validate =
    case wrq:method(ReqData) of
      'GET'  -> [nodename, module];
      'POST' -> [nodename, module, {enum, [{name,    interpret},
                                           {allowed, [true, false, toggle]}]}]
    end,
  edts_resource_lib:validate(ReqData, Ctx, Validate).

post_is_create(ReqData, Ctx) ->
  {true, ReqData, Ctx}.

resource_exists(ReqData, Ctx) ->
  {edts_resource_lib:exists_p(ReqData, Ctx, [nodename, module]), ReqData, Ctx}.

%% Handlers
from_json(ReqData, Ctx) ->
  Node               = orddict:fetch(nodename, Ctx),
  Module             = orddict:fetch(module, Ctx),
  Interpret          = orddict:fetch(interpret, Ctx),
  {ok, InterpretedP} = edts_debug:interpret_module(Node, Module, Interpret),
  Body               = mochijson2:encode([{module, Module},
                                          {interpreted, InterpretedP}]),
  {true, wrq:set_resp_body(Body, ReqData), Ctx}.

to_json(ReqData, Ctx) ->
  Node               = orddict:fetch(nodename, Ctx),
  Module             = orddict:fetch(module, Ctx),
  {ok, InterpretedP} = edts_debug:module_interpreted_p(Node, Module),
  Body               = mochijson2:encode([{module, Module},
                                          {interpreted, InterpretedP}]),
  {Body, ReqData, Ctx}.



%%%_* Internal functions =======================================================


%%%_* Unit tests ===============================================================
init_test() ->
  ?assertEqual({ok, orddict:new()}, init(foo)).

allowed_methods_test() ->
  ?assertEqual({['GET', 'POST'], foo, bar}, allowed_methods(foo, bar)).

content_types_accepted_test() ->
  ?assertEqual({[ {"application/json", from_json} ], foo, bar},
               content_types_accepted(foo, bar)).

content_types_provided_test() ->
  ?assertEqual({[ {"application/json", to_json}
                , {"text/html",        to_json}
                , {"text/plain",       to_json} ], foo, bar},
              content_types_provided(foo, bar)).

from_json_test() ->
  meck:unload(),
  meck:new(wrq),
  meck:expect(wrq, set_resp_body, fun(A, _) -> A end),
  meck:new(edts_debug),
  meck:expect(edts_debug, interpret_module, fun(_, foo, toggle) -> true end),

  Dict1 =
    orddict:from_list([{nodename, true},
                       {module, foo},
                       {interpret, toggle}]),
  Res = from_json(req_data, Dict1),
  ?assertMatch({true, _, Dict1}, Res),

  JSON = element(2, Res),
  ?assertEqual({struct,
                [{<<"module">>,<<"foo">>},
                 {<<"interpreted">>,true}]},
               mochijson2:decode(JSON)),
  meck:unload().

to_json_test() ->
  meck:unload(),
  meck:new(edts_debug),
  meck:expect(edts_debug, module_interpreted_p, fun(_, foo) -> true;
                                                   (_, _)   -> false
                                                end),
  Dict1 = orddict:from_list([{nodename, true}, {module, foo}]),
  Res = to_json(req_data, Dict1),
  ?assertMatch({_, req_data, Dict1}, Res),

  JSON = element(1, Res),
  ?assertEqual({struct, [{<<"module">>,<<"foo">>},{<<"interpreted">>,true}]},
               mochijson2:decode(JSON)),
  meck:unload().


%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
