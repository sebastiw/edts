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
-module(edts_resource_module).

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
        , to_json/2]).

%%%_* Includes =================================================================
-include_lib("eunit/include/eunit.hrl").
-include_lib("webmachine/include/webmachine.hrl").

%%%_* Defines ==================================================================
%%%_* Types ====================================================================
%%%_* API ======================================================================


%% Webmachine callbacks
init(_Config) ->
  edts_log:debug("Call to ~p", [?MODULE]),
  {ok, orddict:new()}.

allow_missing_post(ReqData, Ctx) ->
  {true, ReqData, Ctx}.

allowed_methods(ReqData, Ctx) ->
  {['GET', 'POST'], ReqData, Ctx}.

content_types_accepted(ReqData, Ctx) ->
  Map = [ {"application/json", from_json}],
  {Map, ReqData, Ctx}.

content_types_provided(ReqData, Ctx) ->
  Map = [ {"application/json", to_json}
        , {"text/html",        to_json}
        , {"text/plain",       to_json}],
  {Map, ReqData, Ctx}.

create_path(ReqData, Ctx) ->
  {wrq:path(ReqData), ReqData, Ctx}.

malformed_request(ReqData, Ctx) ->
  Validate = case wrq:method(ReqData) of
               'GET'  -> [nodename, module, info_level];
               'POST' -> [nodename, module, file]
             end,
  edts_resource_lib:validate(ReqData, Ctx, Validate).

post_is_create(ReqData, Ctx) ->
  {true, ReqData, Ctx}.

resource_exists(ReqData, Ctx) ->
  case wrq:method(ReqData) of
    'POST' ->
      Exists = edts_resource_lib:exists_p(ReqData, Ctx, [nodename, module]),
      {Exists, ReqData, Ctx};
    'GET' ->
      MFArgKeys = {edts_code, get_module_info, [module, info_level]},
      edts_resource_lib:check_exists_and_do_rpc(ReqData, Ctx, [], MFArgKeys)
  end.


%% Handlers
from_json(ReqData, Ctx) ->
  Node     = orddict:fetch(nodename, Ctx),
  Filename = orddict:fetch(file, Ctx),
  {ok, {Result, {Errors0, Warnings0}}} =
    edts:call(Node, edts_code, compile_and_load, [Filename]),
  Errors   = {array, [format_error(Error) || Error <- Errors0]},
  Warnings = {array, [format_error(Warning) || Warning <- Warnings0]},
  Data = {struct, [{result, Result}, {warnings, Warnings}, {errors, Errors}]},
  {true, wrq:set_resp_body(mochijson2:encode(Data), ReqData), Ctx}.

to_json(ReqData, Ctx) ->
  Info = orddict:fetch(result, Ctx),
  Data = format(Info),
  {mochijson2:encode(Data), ReqData, Ctx}.

%%%_* Internal functions =======================================================

format(Info) ->
  lists:foldl(fun format/2, [], Info).

format({exports, Exports}, Acc) ->
  [{exports, Exports}|Acc];
format({source, Source}, Acc) ->
  [{source, list_to_binary(Source)}|Acc];
format({time, {{Y, Mo, D}, {H, Mi, S}}}, Acc) ->
  Fmt = "~b-~2.10.0b-~2.10.0b ~2.10.0b:~2.10.0b:~2.10.0b",
  Str = lists:flatten(io_lib:format(Fmt, [Y, Mo, D, H, Mi, S])),
  [{time, list_to_binary(Str)}|Acc];
format({records, Recs0}, Acc) ->
  Recs = [lists:map(fun format_element/1, Rec) || Rec <- Recs0],
  [{records, Recs}|Acc];
format({functions, Funs0}, Acc) ->
  Funs = [lists:map(fun format_element/1, Fun) || Fun <- Funs0],
  [{functions, Funs}|Acc];
format({imports, Imports}, Acc) ->
  [{imports, Imports}|Acc];
format({includes, Includes}, Acc) ->
  [{includes, [list_to_binary(I) || I <- Includes]}|Acc];
format({module, _} = Module, Acc) ->
  [Module|Acc];
format(_, Acc) ->
  Acc.

format_element({source, Source}) -> {source, list_to_binary(Source)};
format_element(Attr)             -> Attr.

format_error({Type, File, Line, Desc}) ->
    [ {type, Type}
    , {file, list_to_binary(File)}
    , {line, Line}
    , {description, list_to_binary(Desc)}].


%%%_* Unit tests ===============================================================

init_test() ->
  ?assertEqual({ok, orddict:new()}, init(foo)).

allowed_methods_test() ->
  ?assertEqual({['GET', 'POST'], foo, bar}, allowed_methods(foo, bar)).

content_types_provided_test() ->
  ?assertEqual({[ {"application/json", to_json},
                  {"text/html",        to_json},
                  {"text/plain",       to_json}], foo, bar},
              content_types_provided(foo, bar)).

malformed_request_get_test() ->
  meck:unload(),
  meck:new(wrq),
  meck:expect(wrq, method, fun(_) -> 'GET' end),
  meck:new(edts_resource_lib),
  meck:expect(edts_resource_lib, validate,
              fun(req_data, [], [nodename, module, info_level]) ->
                  {false, req_data, []};
                 (ReqData, Ctx, _) ->
                  {true, ReqData, Ctx}
              end),
  ?assertEqual({false, req_data,  []}, malformed_request(req_data, [])),
  ?assertEqual({true, req_data2, []}, malformed_request(req_data2, [])),
  meck:unload().

malformed_request_post_test() ->
  meck:unload(),
  meck:new(wrq),
  meck:expect(wrq, method, fun(_) -> 'POST' end),
  meck:new(edts_resource_lib),
  meck:expect(edts_resource_lib, validate,
              fun(req_data, [], [nodename, module, file]) ->
                  {false, req_data, []};
                 (ReqData, Ctx, _) ->
                  {true, ReqData, Ctx}
              end),
  ?assertEqual({false, req_data,  []}, malformed_request(req_data, [])),
  ?assertEqual({true, req_data2, []}, malformed_request(req_data2, [])),
  meck:unload().


format_error_test_() ->
  ?_assertEqual([{type, t}, {file, <<"f">>}, {line, l}, {description, <<"d">>}],
                format_error({t, "f", l, "d"})).

format_element_test_() ->
  [
   ?_assertEqual(foo, format_element(foo)),
   ?_assertEqual({source, <<"foo">>}, format_element({source, "foo"}))
  ].

format_test_() ->
  [
   ?_assertEqual([{exports, []}],
                 format({exports, []}, [])),
   ?_assertEqual([{exports, [foo, bar]}],
                 format({exports, [foo, bar]}, [])),
   ?_assertEqual([{source, <<"foo.erl">>}],
                 format({source, "foo.erl"}, [])),
   ?_assertEqual([{time, <<"2012-09-20 21:22:00">>}],
                 format({time, {{2012, 9, 20}, {21, 22, 00}}}, [])),
   ?_assertEqual([{records, []}],
                 format({records, []}, [])),
   ?_assertEqual([{records, [[{source, <<"foo.erl">>}]]}],
                 format({records, [[{source, "foo.erl"}]]}, [])),
   ?_assertEqual([{functions, []}],
                 format({functions, []}, [])),
   ?_assertEqual([{functions, [[{source, <<"foo.erl">>}]]}],
                 format({functions, [[{source, "foo.erl"}]]}, [])),
   ?_assertEqual([{imports, []}],
                 format({imports, []}, [])),
   ?_assertEqual([{imports, ["foo"]}],
                 format({imports, ["foo"]}, [])),
   ?_assertEqual([{module, mod}],
                 format({module, mod}, [])),
   ?_assertEqual([],
                 format(foo, []))
  ].

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

