%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Compilation resource
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
-module(edts_resource_compilation_result).

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
  edts_resource_lib:validate(ReqData, Ctx, [nodename, module, file]).

resource_exists(ReqData, Ctx) ->
  case edts_resource_lib:exists_p(ReqData, Ctx, [nodename, file]) of
    false -> {false, ReqData, Ctx};
    true  ->
      Nodename = orddict:fetch(nodename, Ctx),
      Filename = wrq:get_qs_value("file", ReqData),
      Result   = edts:compile_and_load(Nodename, Filename),
      {true, ReqData, orddict:store(compilation_result, Result, Ctx)}
  end.

%% Handlers

to_json(ReqData, Ctx) ->
  {Errors0, Warnings0} = orddict:fetch(compilation_result, Ctx),
  Result = case Errors0 of
             [] -> ok;
             _  -> error
           end,
  Errors   = {array, [format_error(Error) || Error <- Errors0]},
  Warnings = {array, [format_error(Warning) || Warning <- Warnings0]},
  Data = {struct, [{result, Result}, {warnings, Warnings}, {errors, Errors}]},
  {mochijson2:encode(Data), ReqData, Ctx}.

format_error({Type, File, Line, Desc}) ->
  {struct, [ {type, Type}
           , {file, list_to_binary(File)}
           , {line, Line}
           , {description, list_to_binary(Desc)}]}.

%%%_* Internal functions =======================================================


%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
