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
-include_lib("eunit/include/eunit.hrl").
-include_lib("webmachine/include/webmachine.hrl").

%%%_* Defines ==================================================================
%%%_* Types ====================================================================
%%%_* API ======================================================================


%% Webmachine callbacks
init(_Config) ->
  edts_log:debug("Call to ~p", [?MODULE]),
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

malformed_request(ReqData, Ctx) ->
  edts_resource_lib:validate(ReqData, Ctx, [project_name,
                                            nodename,
                                            project_root,
                                            project_lib_dirs,
                                            app_include_dirs,
                                            project_include_dirs,
                                            erlang_cookie]).

post_is_create(ReqData, Ctx) ->
  {true, ReqData, Ctx}.

%% Handlers
from_json(ReqData, Ctx) ->
  case init_node(Ctx) of
    ok           -> {true, ReqData, Ctx};
    {error, Err} -> erlang:error(Err)
  end.

%%%_* Internal functions =======================================================

init_node(Ctx) ->
  edts:init_node(orddict:fetch(project_name,         Ctx),
                 orddict:fetch(nodename,             Ctx),
                 orddict:fetch(project_root,         Ctx),
                 orddict:fetch(project_lib_dirs,     Ctx),
                 orddict:fetch(app_include_dirs,     Ctx),
                 orddict:fetch(project_include_dirs, Ctx),
                 orddict:fetch(erlang_cookie,        Ctx)).

%%%_* Unit tests ===============================================================

init_test() ->
  ?assertEqual({ok, orddict:new()}, init(foo)).

allowed_methods_test() ->
  ?assertEqual({['POST'], foo, bar}, allowed_methods(foo, bar)).

allow_missing_post_test() ->
  ?assertEqual({true, foo, bar}, allow_missing_post(foo, bar)).

content_types_accepted_test() ->
  ?assertEqual({[ {"application/json", from_json}], foo, bar},
              content_types_accepted(foo, bar)).

create_path_test() ->
  Dict = orddict:from_list([{nodename, true}]),
  ?assertEqual({"true", req_data, Dict}, create_path(req_data, Dict)).

malformed_request_test() ->
  meck:unload(),
  meck:new(edts_resource_lib),
  Args = [project_name,
          nodename,
          project_root,
          project_lib_dirs,
          app_include_dirs,
          project_include_dirs,
          erlang_cookie],
  meck:expect(edts_resource_lib, validate,
              fun(req_data, [], Args0) when Args0 =:= Args ->
                  {false, req_data, []};
                 (ReqData, Ctx, _) ->
                  {true, ReqData, Ctx}
              end),
  ?assertEqual({false, req_data,  []}, malformed_request(req_data, [])),
  ?assertEqual({true, req_data2, []}, malformed_request(req_data2, [])),
  meck:unload().

post_is_create_test() ->
  ?assertEqual({true, foo, bar}, post_is_create(foo, bar)).

from_json_test() ->
  meck:unload(),
  meck:new(edts),
  meck:expect(edts, init_node, fun(name, true, r, [], [], [], undefined) -> ok;
                                  (name, _,    _, _,  _,  _, _)  -> {error, foo}
                               end),
  Dict1 = orddict:from_list([{project_name, name},
                             {nodename, true},
                             {project_root, r},
                             {project_lib_dirs, []},
                             {app_include_dirs, []},
                             {project_include_dirs, []},
                             {erlang_cookie, undefined}]),
  ?assertEqual({true, req_data,  Dict1},
               from_json(req_data, Dict1)),
  Dict2 = orddict:from_list([{project_name, name},
                             {nodename, false},
                             {project_root, r},
                             {project_lib_dirs, []},
                             {app_include_dirs, []},
                             {project_include_dirs, []},
                             {erlang_cookie, undefined}]),
  ?assertException(error, foo, from_json(req_data, Dict2)),
  meck:unload().


%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
