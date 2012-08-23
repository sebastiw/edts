%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc The main edts server
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
-module(edts_resource_function).

%%%_* Exports ==================================================================

%% API
%% Webmachine callbacks
-export([ allowed_methods/2
        , content_types_provided/2
        , init/1
        , malformed_request/2
        , resource_exists/2]).

%% Handlers
-export([to_json/2]).

%%%_* Includes =================================================================
-include_lib("webmachine/include/webmachine.hrl").

%%%_* Defines ==================================================================
%%%_* Types ====================================================================
%%%_* API ======================================================================


%% Webmachine callbacks
init(_Config) ->
  {ok, orddict:new()}.

allowed_methods(ReqData, Ctx) ->
  {['GET'], ReqData, Ctx}.

content_types_provided(ReqData, Ctx) ->
  Map = [ {"application/json", to_json}
        , {"text/html",        to_json}
        , {"text/plain",       to_json}],
  {Map, ReqData, Ctx}.

malformed_request(ReqData, Ctx) ->
  case wrq:get_qs_value("arity", ReqData) of
    undefined -> {false, ReqData, orddict:store(arity, 0, Ctx)};
    Arity ->
      try {false, ReqData, orddict:store(arity, list_to_integer(Arity), Ctx)}
      catch error:badarg -> {true ,ReqData, Ctx}
      end
  end.

resource_exists(ReqData, Ctx) ->
  Nodename = edts_resource_lib:make_nodename(wrq:path_info(nodename, ReqData)),
  Module   = list_to_atom(wrq:path_info(module, ReqData)),
  Function = list_to_atom(wrq:path_info(function, ReqData)),
  Arity    = orddict:fetch(arity, Ctx),
  Info     = edts:get_function_info(Nodename, Module, Function, Arity),
  Exists   = edts:node_available_p(Nodename) andalso
             not (Info =:= {error, not_found}),
  {Exists, ReqData, orddict:store(info, Info, Ctx)}.

%% Handlers
to_json(ReqData, Ctx) ->
  Info0 = orddict:fetch(info, Ctx),
  io:format("Info ~p~n", [Info0]),
  {value, {source, S}, Other} = lists:keytake(source, 1, Info0),
  io:format("3"),
  Data = {struct, [{source, list_to_binary(S)}|Other]},
  {mochijson2:encode(Data), ReqData, Ctx}.

%%%_* Internal functions =======================================================


%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
