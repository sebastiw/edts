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
-module(edts_resource_debugger).

%%%_* Exports ==================================================================

%% API
%% Webmachine callbacks
-export([ allowed_methods/2
        , allow_missing_post/2
        , content_types_accepted/2
        , content_types_provided/2
        , create_path/2
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

malformed_request(ReqData, Ctx) ->
  edts_resource_lib:validate(ReqData, Ctx, [nodename]).

post_is_create(ReqData, Ctx) ->
  {true, ReqData, Ctx}.

resource_exists(ReqData, Ctx) ->
  Exists   = edts_resource_lib:exists_p(ReqData, Ctx, [nodename]),
  {Exists, ReqData, Ctx}.

%% Handlers
from_json(ReqData, Ctx) ->
  Node    = orddict:fetch(nodename, Ctx),
  Command = list_to_atom(wrq:get_qs_value("cmd", ReqData)),
  Info    = edts:Command(Node),
  Data    = edts_resource_lib:encode_debugger_info(Info),
  {true, wrq:set_resp_body(mochijson2:encode(Data), ReqData), Ctx}.

to_json(ReqData, Ctx) ->
  Node     = orddict:fetch(nodename, Ctx),
  Command  = list_to_atom(wrq:get_qs_value("cmd", ReqData)),
  Info     = edts:Command(Node),
  Data     = edts_resource_lib:encode_debugger_info(Info),
  {mochijson2:encode(Data), ReqData, Ctx}.

%%%_* Internal functions =======================================================
%%%_* Unit tests ===============================================================
init_test() ->
  ?assertEqual({ok, orddict:new()}, init(foo)).

allowed_methods_test() ->
  ?assertEqual({['GET', 'POST'], foo, bar}, allowed_methods(foo, bar)).

content_types_accepted_test() ->
  ?assertEqual({[ {"application/json", to_json} ], foo, bar},
               content_types_accepted(foo, bar).)

content_types_provided_test() ->
  ?assertEqual({[ {"application/json", to_json}
                , {"text/html",        to_json}
                , {"text/plain",       to_json} ], foo, bar},
              content_types_provided(foo, bar)).

%% to_json_test() ->
%%   meck:unload(),
%%   meck:new(wrq),
%%   meck:expect(wrq, req_body, fun(A) -> list_to_binary(atom_to_list(A)) end),
%%   meck:new(edts_code),
%%   meck:expect(edts_code, free_vars,
%%               fun("req_data1") -> {ok, ['VarA', 'VarB']};
%%                  ("req_data2") -> {error, [{err, "S", 13, "D"}]}
%%               end),
%%   meck:new(mochijson2),
%%   meck:expect(mochijson2, encode, fun(A) -> A end),
%%   ?assertEqual({[{vars, ['VarA', 'VarB']}], req_data1, []},
%%                to_json(req_data1, [])),
%%   ?assertEqual({[{errors, [[ {type, err}
%%                           , {file, <<"S">>}
%%                           , {line, 13}
%%                           , {description, <<"D">>}]]}], req_data2, []},
%%                to_json(req_data2, [])),
%%   meck:unload().


%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
