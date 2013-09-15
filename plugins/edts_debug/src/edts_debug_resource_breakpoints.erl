%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Breakpoint resource
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
-module(edts_debug_resource_breakpoints).

-compile({parse_transform, lager_transform}).

%%%_* Exports ==================================================================

%% API
%% Webmachine callbacks
-export([ allowed_methods/2
        , content_types_provided/2
        , init/1
        , malformed_request/2
        , resource_exists/2]).

%% Handlers
-export([ to_json/2 ]).

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
  {['GET'], ReqData, Ctx}.

content_types_provided(ReqData, Ctx) ->
  Map = [ {"application/json", to_json}
        , {"text/html",        to_json}
        , {"text/plain",       to_json} ],
  {Map, ReqData, Ctx}.

malformed_request(ReqData, Ctx) ->
  edts_resource_lib:validate(ReqData, Ctx, [nodename]).

resource_exists(ReqData, Ctx) ->
  {edts_resource_lib:exists_p(ReqData, Ctx, [nodename]), ReqData, Ctx}.

to_json(ReqData, Ctx) ->
  Nodename = orddict:fetch(nodename, Ctx),
  {ok, Breakpoints} = edts:call(Nodename, edts_debug, breakpoints),
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
  ?assertEqual({['GET'], foo, bar}, allowed_methods(foo, bar)).

content_types_provided_test() ->
  ?assertEqual({[ {"application/json", to_json}
                , {"text/html",        to_json}
                , {"text/plain",       to_json} ], foo, bar},
              content_types_provided(foo, bar)).

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

