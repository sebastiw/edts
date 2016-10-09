%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc erlang code parsing resource
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
-module(edts_resource_parse).

%%%_* Exports ==================================================================

%% API
%% Webmachine callbacks
-export([ allowed_methods/2,
          content_types_provided/2,
          init/1
        ]).

%% Handlers
-export([to_json/2]).

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

allowed_methods(ReqData, Ctx) ->
  {['GET'], ReqData, Ctx}.

content_types_provided(ReqData, Ctx) ->
  Map = [ {"application/json", to_json}],
  {Map, ReqData, Ctx}.

%% Handlers
to_json(ReqData, Ctx) ->
  Json = mochijson2:decode(binary_to_list(wrq:req_body(ReqData))),
  Data = lists:map(fun json_to_mfa/1, Json),
  {mochijson2:encode(Data), ReqData, Ctx}.

%%%_* Internal functions =======================================================

json_to_mfa(Str) ->
  case edts_code:string_to_mfa(binary_to_list(Str)) of
    {ok, MFA}       -> MFA;
    {error, Errors} -> [{errors, [format_error(Err) || Err <- Errors]}]
  end.

format_error({Type, File, Line, Desc}) ->
  [ {type, Type}
  , {file, list_to_binary(File)}
  , {line, Line}
  , {description, list_to_binary(Desc)}].


%%%_* Unit tests ===============================================================


init_test() ->
  ?assertEqual({ok, orddict:new()}, init(foo)).

allowed_methods_test() ->
  ?assertEqual({['GET'], foo, bar}, allowed_methods(foo, bar)).

content_types_provided_test() ->
  ?assertEqual({[ {"application/json", to_json}], foo, bar},
              content_types_provided(foo, bar)).

to_json_test_() ->
  {setup,
   fun() ->
     meck:unload(),
     meck:new(wrq),
     meck:expect(wrq, req_body,
                 fun(A) ->
                     Str = lists:flatten(io_lib:format("[\"~p\"]", [A])),
                     list_to_binary(Str)
                 end),
     meck:new(edts_code),
     meck:expect(edts_code, string_to_mfa,
                 fun("req_data1") ->
                     {ok, [{module, foo}, {function, bar}, {arity, 1}]};
                    ("req_data2") ->
                     {error, [{error, "S", 1, "D"}]}
                 end),
     meck:new(mochijson2, [passthrough]),
     meck:expect(mochijson2, encode, fun(A) -> A end)
   end,
   fun(_) ->
       meck:unload()
   end,
   [?_assertEqual({[[{module, foo}, {function, bar}, {arity, 1}]],
                   req_data1,
                   []},
                  to_json(req_data1, [])),
    ?_assertEqual({[[{errors, [[ {type, error}
                                , {file, <<"S">>}
                                , {line, 1}
                                , {description, <<"D">>}]]}]], req_data2, []},
                  to_json(req_data2, []))
   ]}.


%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
