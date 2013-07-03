%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc eunit resource
%%% @end
%%% @author Håkan Nilsson <haakan@gmail.com>
%%% @copyright
%%% Copyright 2012 Håkan Nilsson <haakan@gmail.com>
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
-module(edts_resource_eunit).

%%%_* Exports ==================================================================

%% API
%% Webmachine callbacks
-export([ allowed_methods/2
        , content_types_provided/2
        , init/1
        , malformed_request/2
        , resource_exists/2]).

%% Handlers
-export([ to_json/2]).

%%%_* Includes =================================================================
-include_lib("webmachine/include/webmachine.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%_* Defines ==================================================================
%%%_* Types ====================================================================
%%%_* API ======================================================================

%% Webmachine callbacks
init(_Config) ->
  edts_log:debug("Call to ~p", [?MODULE]),
  {ok, orddict:new()}.

allowed_methods(ReqData, Ctx) ->
  {['GET'], ReqData, Ctx}.

content_types_provided(ReqData, Ctx) ->
  Map = [ {"application/json", to_json}
        , {"text/html",        to_json}
        , {"text/plain",       to_json}],
  {Map, ReqData, Ctx}.

malformed_request(ReqData, Ctx) ->
  edts_resource_lib:validate(ReqData, Ctx, [nodename, module]).

resource_exists(ReqData, Ctx) ->
  case edts_resource_lib:exists_p(ReqData, Ctx, [nodename, module]) of
    false -> {false, ReqData, Ctx};
    true  ->
      Node   = orddict:fetch(nodename, Ctx),
      Module = orddict:fetch(module,   Ctx),
      case edts:call(Node, edts_eunit, run_tests, [Module]) of
        {ok, Result} -> {true, ReqData, orddict:store(result, Result, Ctx)};
        {error, _}   -> {false, ReqData, Ctx}
      end
  end.

to_json(ReqData, Ctx) ->
  IsPassed = fun ({Type, _, _, _}) -> Type =:= 'passed-test' end,
  {Passed, Failed} = lists:partition(IsPassed, orddict:fetch(result, Ctx)),
  Struct = [ {passed, {array, [format_test(Test) || Test <- Passed]}}
           , {failed, {array, [format_test(Test) || Test <- Failed]}}],
  {mochijson2:encode({struct, Struct}), ReqData, Ctx}.


format_test({Type, File, Line, Desc}) ->
  {struct, [ {type, Type}
           , {file, list_to_binary(File)}
           , {line, Line}
           , {description, unicode:characters_to_binary(Desc)}]}.

%%%_* Internal functions =======================================================

%%%_* Tests ====================================================================

init_test() ->
  ?assertEqual({ok, orddict:new()}, init(foo)).

allowed_methods_test() ->
  ?assertEqual({['GET'], foo, bar}, allowed_methods(foo, bar)).

content_types_provided_test() ->
  ?assertEqual({[ {"application/json", to_json},
                  {"text/html",        to_json},
                  {"text/plain",       to_json}], foo, bar},
              content_types_provided(foo, bar)).

malformed_request_test() ->
  meck:unload(),
  meck:new(edts_resource_lib),
  meck:expect(edts_resource_lib, validate,
              fun(req_data, [], [nodename, module]) ->
                  {false, req_data, []};
                 (ReqData, Ctx, _) ->
                  {true, ReqData, Ctx}
              end),
  ?assertEqual({false, req_data,  []}, malformed_request(req_data, [])),
  ?assertEqual({true, req_data2, []}, malformed_request(req_data2, [])),
  meck:unload().

resource_exists_test() ->
  Ctx = orddict:from_list([ {nodename, node}
                          , {module,   mod}
                          ]),
  meck:unload(),
  meck:new(edts_resource_lib),
  meck:expect(edts_resource_lib, exists_p, fun(_, _, _) -> true end),
  meck:new(edts),
  meck:expect(edts, get_module_eunit_result, fun(_, _) -> {ok, []} end),
  ?assertMatch({true, req_data, _}, resource_exists(req_data, Ctx)),
  ?assertEqual([], orddict:fetch(result,
                                 element(3, resource_exists(req_data, Ctx)))),
  meck:unload().

to_json_test() ->
  Ctx = orddict:from_list([{result, [{t, "file", 1337, "desc"}]}]),
  meck:unload(),
  meck:new(mochijson2),
  meck:expect(mochijson2, encode, fun(Data) -> Data end),
  ?assertMatch({{struct, [ {passed, {array, []}}
                         , {failed, {array, [_]}}]}, req_data, Ctx},
               to_json(req_data, Ctx)),
  meck:unload().

format_test_test() ->
  ?assertEqual({struct, [{type, t},
                         {file, <<"file">>},
                         {line, 1337},
                         {description, <<"desc">>}]},
               format_test({t, "file", 1337, "desc"})),
  ?assertError(badarg, format_test({t, file, 1337, "desc"})).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
