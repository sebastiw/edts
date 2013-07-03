%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc module resource
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
-module(edts_resource_xref).

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
  edts_resource_lib:validate(ReqData, Ctx, [nodename, modules, xref_checks]).


resource_exists(ReqData, Ctx) ->
  Node    = orddict:fetch(nodename, Ctx),
  Modules = orddict:fetch(modules, Ctx),
  Checks  = orddict:fetch(xref_checks, Ctx),
  case edts_resource_lib:exists_p(ReqData, Ctx, [nodename]) of
    false -> {false, ReqData, Ctx};
    true  ->
      case edts:call(Node, edts_code, check_modules, [Modules, Checks]) of
        {ok, Analysis} ->
          {true, ReqData, orddict:store(analysis, Analysis, Ctx)};
        {error, _} ->
          {false, ReqData, Ctx}
      end
  end.


to_json(ReqData, Ctx) ->
  Errors = {array, [format_error(Err) || Err <- orddict:fetch(analysis, Ctx)]},
  {mochijson2:encode({struct, [{errors, Errors}]}), ReqData, Ctx}.


format_error({Type, File, Line, Desc}) ->
  {struct, [ {type, Type}
           , {file, list_to_binary(File)}
           , {line, Line}
           , {description, list_to_binary(Desc)}]}.

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
              fun(req_data, [], [nodename, modules, xref_checks]) ->
                  {false, req_data, []};
                 (ReqData, Ctx, _) ->
                  {true, ReqData, Ctx}
              end),
  ?assertEqual({false, req_data,  []}, malformed_request(req_data, [])),
  ?assertEqual({true, req_data2, []}, malformed_request(req_data2, [])),
  meck:unload().

resource_exists_test() ->
  Ctx = orddict:from_list([{nodename, node},
                           {modules,   [mod]},
                           {xref_checks, [undefined_function_calls]}]),
  meck:unload(),
  meck:new(edts_resource_lib),
  meck:expect(edts_resource_lib, exists_p, fun(_, _, _) -> true end),
  meck:new(edts),
  meck:expect(edts, call, fun(_, _, _, _) -> {ok, []} end),
  ?assertMatch({true, req_data, _}, resource_exists(req_data, Ctx)),
  ?assertEqual([], orddict:fetch(analysis,
                                 element(3, resource_exists(req_data, Ctx)))),
  meck:unload().

to_json_test_() ->
  Ctx = orddict:from_list([{analysis, [{t, "file", 1337, "desc"}]}]),
  meck:unload(),
  meck:new(mochijson2),
  meck:expect(mochijson2, encode, fun(Data) -> Data end),
  ?assertMatch({{struct, [{errors, {array, [_]}}]}, req_data, Ctx},
               to_json(req_data, Ctx)),
  meck:unload().

format_error_test() ->
  ?assertEqual({struct, [{type, t},
                         {file, <<"file">>},
                         {line, 1337},
                         {description, <<"desc">>}]},
               format_error({t, "file", 1337, "desc"})),
  ?assertError(badarg, format_error({t, file, 1337, "desc"})).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
