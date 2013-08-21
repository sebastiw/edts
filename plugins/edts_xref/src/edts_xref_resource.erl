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
-module(edts_xref_resource).

%%%_* Exports ==================================================================

%% API
%% Webmachine callbacks
-export([ allowed_methods/2
        , allow_missing_post/2
        , content_types_accepted/2
        , init/1
        , malformed_request/2
        , process_post/2]).


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
  {['POST'], ReqData, Ctx}.

allow_missing_post(ReqData, Ctx) ->
  {true, ReqData, Ctx}.

content_types_accepted(ReqData, Ctx) ->
  Map = [ {"application/json", from_json}],
  {Map, ReqData, Ctx}.

malformed_request(ReqData, Ctx) ->
  case wrq:get_qs_value("start", ReqData) of
    "true" -> edts_resource_lib:validate(ReqData, Ctx, [nodename]);
    _      -> {true, ReqData, Ctx}
  end.

process_post(ReqData, Ctx) ->
  io:format("Callaaed!"),
  {true, ReqData, Ctx}.

%%%_* Internal functions =======================================================

%%%_* Tests ====================================================================

%% init_test() ->
%%   ?assertEqual({ok, orddict:new()}, init(foo)).

%% allowed_methods_test() ->
%%   ?assertEqual({['GET'], foo, bar}, allowed_methods(foo, bar)).

%% content_types_provided_test() ->
%%   ?assertEqual({[ {"application/json", to_json},
%%                   {"text/html",        to_json},
%%                   {"text/plain",       to_json}], foo, bar},
%%               content_types_provided(foo, bar)).

%% malformed_request_test() ->
%%   meck:unload(),
%%   meck:new(edts_resource_lib),
%%   meck:expect(edts_resource_lib, validate,
%%               fun(req_data, [], [nodename, modules, xref_checks]) ->
%%                   {false, req_data, []};
%%                  (ReqData, Ctx, _) ->
%%                   {true, ReqData, Ctx}
%%               end),
%%   ?assertEqual({false, req_data,  []}, malformed_request(req_data, [])),
%%   ?assertEqual({true, req_data2, []}, malformed_request(req_data2, [])),
%%   meck:unload().

%% to_json_test_() ->
%%   Ctx = orddict:from_list([{result, [{t, "file", 1337, "desc"}]}]),
%%   meck:unload(),
%%   meck:new(mochijson2),
%%   meck:expect(mochijson2, encode, fun(Data) -> Data end),
%%   ?assertMatch({{struct, [{errors, {array, [_]}}]}, req_data, Ctx},
%%                to_json(req_data, Ctx)),
%%   meck:unload().

%% format_error_test() ->
%%   ?assertEqual({struct, [{type, t},
%%                          {file, <<"file">>},
%%                          {line, 1337},
%%                          {description, <<"desc">>}]},
%%                format_error({t, "file", 1337, "desc"})),
%%   ?assertError(badarg, format_error({t, file, 1337, "desc"})).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
