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
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% EDTS is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with EDTS. If not, see <http://www.gnu.org/licenses/>.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration =======================================================
-module(edts_resource_node).

%%%_* Exports ==================================================================

%% API
%% Webmachine callbacks
-export([ allow_missing_post/2
        , allowed_methods/2
        , create_path/2
        , content_types_accepted/2
        , init/1
        , malformed_request/2
        , post_is_create/2]).

%% Handlers
-export([from_json/2]).

%%%_* Includes =================================================================
-include_lib("webmachine/include/webmachine.hrl").

%%%_* Defines ==================================================================
%%%_* Types ====================================================================
%%%_* API ======================================================================


%% Webmachine callbacks
init(_Config) ->
  {ok, orddict:new()}.

allow_missing_post(ReqData, Ctx) ->
  {true, ReqData, Ctx}.

allowed_methods(ReqData, Ctx) ->
  {['POST'], ReqData, Ctx}.

content_types_accepted(ReqData, Ctx) ->
  Map = [ {"application/json", from_json}],
  {Map, ReqData, Ctx}.

create_path(ReqData, Ctx) ->
  {atom_to_list(orddict:fetch(nodename, Ctx)), ReqData, Ctx}.

malformed_request(ReqData, Ctx0) ->
  Name = wrq:path_info(nodename, ReqData),
  case edts:node_exists(Name) of
    false -> {true, ReqData, Ctx0};
    true ->
      case edts_resource_lib:try_make_nodename(Name) of
        {ok, Nodename} ->
          {false, ReqData, orddict:store(nodename, Nodename, Ctx0)};
        error ->
          {true, ReqData, Ctx0}
      end
  end.

post_is_create(ReqData, Ctx) ->
  {true, ReqData, Ctx}.

%% Handlers
from_json(ReqData, Ctx) ->
  case edts:node_exists(wrq:path_info(nodename, ReqData)) of
    true ->
      ok = edts:init_node(orddict:fetch(nodename, Ctx)),
      {true, ReqData, Ctx};
    false ->
      {false, ReqData, Ctx}
  end.

%%%_* Internal functions =======================================================

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
