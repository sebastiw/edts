%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc node resource
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
-module(edts_resource_code).

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
  Data =
    case edts_code:free_vars(binary_to_list(wrq:req_body(ReqData))) of
      {ok, Vars}      -> [{vars, Vars}];
      {error, Errors} -> [{errors, [format_error(Err) || Err <- Errors]}]
    end,
  {mochijson2:encode(Data), ReqData, Ctx}.

%%%_* Internal functions =======================================================
format_error({Type, File, Line, Desc}) ->
  {struct, [ {type, Type}
           , {file, list_to_binary(File)}
           , {line, Line}
           , {description, list_to_binary(Desc)}]}.

%%%_* Unit tests ===============================================================

init_test() ->
  ?assertEqual({ok, orddict:new()}, init(foo)).

allowed_methods_test() ->
  ?assertEqual({['POST'], foo, bar}, allowed_methods(foo, bar)).

content_types_provided_test() ->
  ?assertEqual({[ {"application/json", to_json}], foo, bar},
              content_types_provided(foo, bar)).

to_json_test() ->
  meck:unload(),
  meck:new(edts),
  meck:expect(edts, init_node, fun(true, r, []) -> ok;
                                  (_, _, _)    -> error
                               end),
  Dict1 =
    orddict:from_list([{nodename, true}, {project_root, r}, {lib_dirs, []}]),
  ?assertEqual({true, req_data,  Dict1},
               to_json(req_data, Dict1)),
  Dict2 =
    orddict:from_list([{nodename, false}, {project_root, r}, {lib_dirs, []}]),
  ?assertError({badmatch, error}, to_json(req_data, Dict2)),
  meck:unload().


%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
