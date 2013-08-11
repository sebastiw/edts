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
-include_lib("eunit/include/eunit.hrl").

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
  Validate =
    case wrq:method(ReqData) of
      'GET'  -> [nodename, module];
      'POST' -> [nodename,
                 module,
                 line,
                 {enum, [{name,    break},
                         {allowed, [true, false, toggle]}]}]
    end,
  edts_resource_lib:validate(ReqData, Ctx, Validate).

post_is_create(ReqData, Ctx) ->
  {true, ReqData, Ctx}.

resource_exists(ReqData, Ctx) ->
  Exists   = edts_resource_lib:exists_p(ReqData, Ctx, [nodename]),
  {Exists, ReqData, Ctx}.

%% Handlers
from_json(ReqData, Ctx) ->
  Node   = orddict:fetch(nodename, Ctx),
  Module = orddict:fetch(module, Ctx),
  Line   = orddict:fetch(line, Ctx),
  Break  = orddict:fetch(break, Ctx),
  {ok, Result} = edts_debug:break(Node, Module, Line, Break),
  {true, wrq:set_resp_body(mochijson2:encode([{break, Result}]), ReqData), Ctx}.

to_json(ReqData, Ctx) ->
  Node   = orddict:fetch(nodename, Ctx),
  Module = orddict:fetch(module, Ctx),
  {ok, Breakpoints} = edts_debug:breakpoints(Node, Module),
  Data = [format(B) || B <- Breakpoints],
  {mochijson2:encode(Data), ReqData, Ctx}.

%%%_* Internal functions =======================================================
format({{Module, Line}, [Status, Trigger, null, Condition]}) ->
  [{module,     Module},
   {line,      Line},
   {status,    Status},
   {trigger,   Trigger},
   {condition, list_to_binary(lists:flatten(io_lib:format("~p",
                                                          [Condition])))}].

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
  meck:new(edts_debug),
  meck:expect(edts_debug, break, fun(true, foo, 1337, true) -> {ok, true} end),
  meck:new(wrq),
  meck:expect(wrq, set_resp_body, fun(A, _) -> A end),
  Dict = orddict:from_list([{nodename, true},
                            {module,   foo},
                            {line,     1337},
                            {break, true}]),
  Res = from_json(req_data, Dict),
  ?assertMatch({true, _, Dict}, Res),
  ?assertEqual({struct, [{<<"break">>, true}]},
               mochijson2:decode(element(2, Res))),
  meck:unload().

%% to_json_test() ->
%%   meck:unload(),
%%   meck:new(edts_debug),
%%   meck:expect(edts_debug, get_breakpoints,
%%               fun(true) -> {ok, [{{foo, 42},
%%                                      [active,
%%                                       enable,
%%                                       null,
%%                                       null]},
%%                                     {{bar, 314},
%%                                      [active,
%%                                       enable,
%%                                       null,
%%                                       null]}]};
%%                  (_)    -> {error, not_found}
%%               end),
%%   Data =  [[{"module", "foo"},
%%             {"line", 42},
%%             {"status", "active"},
%%             {"trigger", "enable"},
%%             {"condition", "null"}],
%%            [{"module", "bar"},
%%             {"line", 314},
%%             {"status", "active"},
%%             {"trigger", "enable"},
%%             {"condition", "null"}]],

%%   meck:new(mochijson2),
%%   meck:expect(mochijson2, encode, fun(_) -> Data
%%                                   end),
%%   Dict1 =
%%     orddict:from_list([{nodename, true}]),
%%   ?assertMatch({Data, req_data, Dict1}, to_json(req_data, Dict1)),

%%   Dict2 =
%%     orddict:from_list([{nodename, false}]),
%%   ?assertError({badmatch, {error, not_found}}, to_json(req_data, Dict2)),
%%   meck:unload().

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

