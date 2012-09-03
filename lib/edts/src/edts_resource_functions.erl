%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc functions resource
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
-module(edts_resource_functions).

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
  lager:debug("Call to ~p", [?MODULE]),
  {ok, orddict:new()}.

allowed_methods(ReqData, Ctx) ->
  {['GET'], ReqData, Ctx}.

content_types_provided(ReqData, Ctx) ->
  Map = [ {"application/json", to_json}
        , {"text/html",        to_json}
        , {"text/plain",       to_json}],
  {Map, ReqData, Ctx}.

malformed_request(ReqData, Ctx) ->
  edts_resource_lib:validate(ReqData, Ctx, [nodename, module, exported]).

resource_exists(ReqData, Ctx) ->
  case edts_resource_lib:exists_p(ReqData, Ctx, [nodename]) of
    false -> {false, ReqData, Ctx};
    true ->
      Nodename = orddict:fetch(nodename, Ctx),
      Module = orddict:fetch(module, Ctx),
      case edts:get_module_info(Nodename, Module, detailed) of
        {error, not_found} -> {false, ReqData, Ctx};
        Info               -> {true,  ReqData, orddict:store(info, Info, Ctx)}
      end
  end.

to_json(ReqData, Ctx) ->
  Exported = orddict:fetch(exported, Ctx),
  Info     = orddict:fetch(info, Ctx),
  {functions, Functions} = lists:keyfind(functions, 1, Info),
  Data = format(Exported, Functions), [], Info,
  {mochijson2:encode(Data), ReqData, Ctx}.

format(Exported, Functions0) ->
  FunFun =
    fun(Function, Acc) ->
        case lists:keyfind(exported, 1, Function) of
          {exported, V} when Exported =:= all orelse
                             Exported =:= V ->
            {value, {source, S}, Other} =
              lists:keytake(source, 1, Function),
            [{struct, [{source, list_to_binary(S)}|Other]}|Acc];
          _ -> Acc
        end
    end,
  Functions = lists:foldl(FunFun, [], Functions0),
  {struct, [{functions, {array, Functions}}]}.


%%%_* Internal functions =======================================================


%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
