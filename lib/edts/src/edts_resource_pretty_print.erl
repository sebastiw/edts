%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc nodes resource
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
-module(edts_resource_pretty_print).

%%%_* Exports ==================================================================

%% API
%% Webmachine callbacks
-export([ allowed_methods/2,
          content_types_provided/2,
          malformed_request/2,
          init/1]).

%% Handlers
-export([ to_json/2]).

%%%_* Includes =================================================================
-include_lib("webmachine/include/webmachine.hrl").

%%%_* Defines ==================================================================
%%%_* Types ====================================================================
%%%_* API ======================================================================


%% Webmachine callbacks
init(_Config) ->
  edts_log:debug("Call to ~p", [?MODULE]),
  {ok, []}.

allowed_methods(ReqData, Ctx) ->
  {['GET'], ReqData, Ctx}.

content_types_provided(ReqData, Ctx) ->
  Map = [ {"application/json", to_json}
        , {"text/html",        to_json}
        , {"text/plain",       to_json}],
  {Map, ReqData, Ctx}.

malformed_request(ReqData, Ctx) ->
  try
    Str    = wrq:get_qs_value("string", ReqData),
    {ok, AbsTerm, _} = erl_scan:string(Str ++ "."),
    {ok, Term} = erl_parse:parse_term(AbsTerm),

    Indent = list_to_integer(wrq:get_qs_value("indent", ReqData)),
    MaxCol = list_to_integer(wrq:get_qs_value("max_column", ReqData)),

    RecF = fun(_A, _N) -> no end,
    PPString =
      lists:flatten(io_lib_pretty:print(Term, Indent, MaxCol, -1, -1, RecF)),
    {false, ReqData, orddict:store(return, PPString, Ctx)}
  catch
    _:_ -> {true, ReqData, Ctx}
  end.

%% Handlers
to_json(ReqData, Ctx) ->
  case orddict:find(return, Ctx) of
    false     -> {[], ReqData, Ctx};
    {ok, Ret} ->
      Bin = list_to_binary(Ret),
      {mochijson2:encode([{return, Bin}]), ReqData, Ctx}
  end.


%%%_* Internal functions =======================================================

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
