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
-module(edts_debug_resource_process_break).

%%%_* Exports ==================================================================

%% API
%% Webmachine callbacks
-export([ allowed_methods/2
        , content_types_provided/2
        , init/1
        , resource_exists/2]).

%% Handlers
-export([to_json/2]).

%%%_* Includes =================================================================
%% -include_lib("webmachine/include/webmachine.hrl").
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
        , {"text/plain",       to_json}],
  {Map, ReqData, Ctx}.

resource_exists(ReqData, Ctx) ->
  Exists   = edts_resource_lib:exists_p(ReqData, Ctx, [nodename]),
  {Exists, ReqData, Ctx}.

%% Handlers
to_json(ReqData, Ctx) ->
  Node    = orddict:fetch(nodename, Ctx),
  Command = orddict:fetch(cmd, Ctx),
  Info    = edts_debug:wait_for_break(Node),
  Data    = encode_debugger_info(Info),
  {mochijson2:encode(Data), ReqData, Ctx}.


%%%_* Internal functions =======================================================
%%------------------------------------------------------------------------------
%% @doc
%% Encodes debugger replies into the appropriate json structure
%% @end
-spec encode_debugger_info({ok, Info :: term()}) -> term().
%%------------------------------------------------------------------------------
encode_debugger_info({ok, Info}) ->
  do_encode_debugger_info(Info);
encode_debugger_info({error, Error}) ->
  [{state, error}, {message, Error}].

do_encode_debugger_info({break, File, {Module, Line}, VarBindings}) ->
  [{state, break}, {file, list_to_binary(File)},{module, Module}, {line, Line},
   {var_bindings,
    {struct, encode(VarBindings)}}];
do_encode_debugger_info([{module, _} | _] = Interpreted) ->
  [{interpreted, {array, Interpreted}}];
do_encode_debugger_info(State) ->
  [{state, State}].

encode(VarBindings) ->
  [{Key, list_to_binary(io_lib:format("~p", [Value]))}
   || {Key, Value} <- VarBindings].

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
%%   meck:new(wrq),
%%   meck:expect(wrq, req_body, fun(A) -> list_to_binary(atom_to_list(A)) end),
%%   meck:expect(wrq, get_qs_value, fun("cmd", _) -> "wait_for_debugger" end),
%%   meck:expect(wrq, set_resp_body, fun(A, _) -> A end),
%%   meck:new(edts),
%%   meck:expect(edts, wait_for_debugger,
%%               fun(_) -> {ok, {break, "foo.erl", {foo, 42}, [{bar, 1}]}} end),

%%   Dict1 =
%%     orddict:from_list([{nodename, true},
%%                       {cmd,  wait_for_debugger}]),
%%   Res = from_json(req_data, Dict1),
%%   ?assertMatch({true, _, Dict1}, Res),

%%   JSON = element(2, Res),
%%   ?assertEqual({struct,
%%                 [{<<"state">>,<<"break">>},
%%                  {<<"file">>,<<"foo.erl">>},
%%                  {<<"module">>,<<"foo">>},
%%                  {<<"line">>,42},
%%                  {<<"var_bindings">>, {struct,[{<<"bar">>,<<"1">>}]}}]},
%%                mochijson2:decode(JSON)),
%%   meck:unload().

encode_debugger_info_test() ->
  ?assertEqual([{state, error}, {message, foo}],
               encode_debugger_info({error, foo})),
  ?assertEqual([ {state, break}
                 , {file, <<"/awsum/foo.erl">>}
                 , {module, foo}
                 , {line, 42}
                 , {var_bindings, {struct, []}}],
               encode_debugger_info({ok, {break, "/awsum/foo.erl", {foo, 42},
                                          []}})),
  ?assertEqual([{state, break},
                {file, <<"/awsum/bar.erl">>},
                {module, bar},
                {line, 123},
                {var_bindings, {struct, [{'A', <<"3.14">>}]}}],
               encode_debugger_info({ok, {break, "/awsum/bar.erl", {bar, 123},
                                          [{'A', 3.14}]}})).

encode_test() ->
  ?assertEqual([{'A', <<"\"foo\"">>}], encode([{'A', "foo"}])),
  ?assertEqual([{"bar", <<"\"BAZ\"">>}], encode([{"bar", [$B, $A, $Z]}])),
  ?assertEqual([{'foo', <<"bar">>}, {"pi", <<"3.14">>}],
               encode([{'foo', bar}, {"pi", 3.14}])),
  ?assertEqual([{a_tuple, <<"{with,3,\"fields\"}">>}],
               encode([{a_tuple, {with, 3, "fields"}}])).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
