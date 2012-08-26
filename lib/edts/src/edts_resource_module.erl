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
-module(edts_resource_module).

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
  edts_resource_lib:validate(ReqData, Ctx, [nodename, module, info_level]).

resource_exists(ReqData, Ctx) ->
  Nodename = orddict:fetch(nodename, Ctx),
  Module   = orddict:fetch(module, Ctx),
  Level    = orddict:fetch(info_level, Ctx),
  Info     = edts:get_module_info(Nodename, Module, Level),
  Exists   =
    edts_resource_lib:exists_p(ReqData, Ctx, [nodename, module]) andalso
    not (Info =:= {error, not_found}),
  {Exists, ReqData, orddict:store(info, Info, Ctx)}.

%% Handlers
to_json(ReqData, Ctx) ->
  Info = orddict:fetch(info, Ctx),
  Data = {struct, lists:foldl(fun format/2, [], Info)},
  {mochijson2:encode(Data), ReqData, Ctx}.

format({exports, Exports}, Acc) ->
  [{exports, {array, [{struct, Export} || Export <- Exports]}}|Acc];
format({source, Source}, Acc) ->
  [{source, list_to_binary(Source)}|Acc];
format({time, {{Y, Mo, D}, {H, Mi, S}}}, Acc) ->
  Str = lists:flatten(io_lib:format("~B-~B-~B ~B:~B:~B", [Y, Mo, D, H, Mi, S])),
  [{time, list_to_binary(Str)}|Acc];
format({records, Records0}, Acc) ->
  RecFun = fun({name,   Name})   -> {name, Name};
              ({line,   Line})   -> {line, Line};
              ({fields, Fs})     -> {fields, {array, Fs}};
              ({source, Source}) -> {source, list_to_binary(Source)}
           end,
  Records = [{struct, lists:map(RecFun, Record)} || Record <- Records0],
  [{struct, [{records, {array, Records}}]}|Acc];
format({functions, Functions0}, Acc) ->
  FunFun = fun({source, Source}) -> {source, list_to_binary(Source)};
              (KV) -> KV
           end,
  Functions = [{struct, lists:map(FunFun, Function)} || Function <- Functions0],
  [{struct, [{functions, {array, Functions}}]}|Acc];
format({imports, Imports}, Acc) ->
  [{struct, [{imports, {array, Imports}}]}|Acc];
format({includes, Includes}, Acc) ->
  [{struct, [{includes, [list_to_binary(I) || I <- Includes]}]}|Acc];
format({module, _} = Module, Acc) ->
  [Module|Acc];
format(_, Acc) ->
  Acc.


%%%_* Internal functions =======================================================


%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
