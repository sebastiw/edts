%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc dialyzer resource
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
-module(edts_resource_dialyzer).

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

malformed_request(ReqData, Ctx0) ->
  % Non-standard validation
  OtpPlt = wrq:get_qs_value("otp_plt", ReqData),
  OutPlt = wrq:get_qs_value("out_plt", ReqData),
  OtpPltExistsP = filelib:is_file(OtpPlt),
  if
    OtpPlt =/= undefined andalso not OtpPltExistsP ->
      edts_log:error("API input validation failed. Key otp_plt, "
                     "Rsn: no_exists"),
      {true, ReqData, Ctx0};
    OutPlt =:= undefined ->
      edts_log:error("API input validation failed. Key out_plt, "
                     "Rsn: undefined"),
      {true, ReqData, Ctx0};
    true ->
      Validators = [nodename, modules],
      Ctx =
        orddict:store(otp_plt, OtpPlt, orddict:store(out_plt, OutPlt, Ctx0)),
      edts_resource_lib:validate(ReqData, Ctx, Validators)
  end.

resource_exists(ReqData, Ctx) ->
  Nodename = orddict:fetch(nodename, Ctx),
  OtpPlt   = orddict:fetch(otp_plt, Ctx),
  OutPlt   = orddict:fetch(out_plt, Ctx),
  Files    = orddict:fetch(modules, Ctx),
  Result   = edts:get_dialyzer_result(Nodename, OtpPlt, OutPlt, Files),
  Exists       = (edts_resource_lib:exists_p(ReqData, Ctx, [nodename, modules])
                  andalso not (Result =:= {error, not_found})),
  {Exists, ReqData, orddict:store(result, Result, Ctx)}.

to_json(ReqData, Ctx) ->
  Result = orddict:fetch(result, Ctx),
  Struct = [ {warnings, {array, [format_warning(Test) || Test <- Result]}}],
  {mochijson2:encode({struct, Struct}), ReqData, Ctx}.


format_warning({Type, File, Line, Desc}) ->
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
  meck:new(wrq),
  meck:expect(wrq, get_qs_value, fun("otp_plt", _)         -> undefined;
                                    ("out_plt", req_data3) -> undefined;
                                    ("out_plt", _)         -> out_plt
                                 end),
  meck:new(edts_resource_lib),
  Validators0 = [nodename, modules],
  meck:expect(edts_resource_lib, validate,
              fun(req_data, Ctx, Validators) when Validators =:= Validators0 ->
                  {false, req_data, Ctx};
                 (ReqData, Ctx, _) ->
                  {true, ReqData, Ctx}
              end),
  ?assertEqual({false, req_data, [{otp_plt, undefined},
                                  {out_plt, out_plt}]},
               malformed_request(req_data, [])),
  ?assertEqual({true, req_data2, [{otp_plt, undefined},
                                  {out_plt, out_plt}]},
               malformed_request(req_data2, [])),
  ?assertEqual({true, req_data3, []}, malformed_request(req_data3, [])),
  meck:unload().

resource_exists_test() ->
  Ctx = orddict:from_list([{nodename, node},
                           {modules,   mod},
                           {otp_plt,  otp},
                           {out_plt,  out}]),
  meck:unload(),
  meck:new(edts_resource_lib),
  meck:expect(edts_resource_lib, exists_p, fun(_, _, _) -> true end),
  meck:new(edts),
  meck:expect(edts, get_dialyzer_result, fun(_, _, _, _) -> ok end),
  ?assertMatch({true, req_data, _}, resource_exists(req_data, Ctx)),
  ?assertEqual(ok, orddict:fetch(result,
                                 element(3, resource_exists(req_data, Ctx)))),
  meck:unload().

to_json_test() ->
  Ctx = orddict:from_list([{result, [{t, "file", 1337, "desc"}]}]),
  meck:unload(),
  meck:new(mochijson2),
  meck:expect(mochijson2, encode, fun(Data) -> Data end),
  ?assertMatch({{struct, [ {warnings, {array, [_]}}]}, req_data, Ctx},
               to_json(req_data, Ctx)),
  meck:unload().

format_warning_test() ->
  ?assertEqual({struct, [{type, t},
                         {file, <<"file">>},
                         {line, 1337},
                         {description, <<"desc">>}]},
               format_warning({t, "file", 1337, "desc"})),
  ?assertError(badarg, format_warning({t, file, 1337, "desc"})).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
