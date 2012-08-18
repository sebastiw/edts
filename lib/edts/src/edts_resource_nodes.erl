%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc The main edts server
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration =======================================================
-module(edts_resource_nodes).

%%%_* Exports ==================================================================

%% API
%% Webmachine callbacks
-export([ allow_missing_post/2
        , allowed_methods/2
        , create_path/2
        , content_types_accepted/2
        , content_types_provided/2
        , init/1
        , malformed_request/2
        , post_is_create/2
        , resource_exists/2]).

%% Handlers
-export([ from_json/2
        , to_json/2]).

%%%_* Includes =================================================================
-include_lib("webmachine/include/webmachine.hrl").

%%%_* Defines ==================================================================
%%%_* Types ====================================================================
%%%_* API ======================================================================


%% Webmachine callbacks
init(_Config) ->
  {ok, []}.

allow_missing_post(ReqData, Ctx) ->
  {true, ReqData, Ctx}.

allowed_methods(ReqData, Ctx) ->
  {['GET', 'POST'], ReqData, Ctx}.

content_types_accepted(ReqData, Ctx) ->
  Map = [ {"application/json", from_json}],
  {Map, ReqData, Ctx}.


content_types_provided(ReqData, Ctx) ->
  Map = [ {"application/json", to_json}
        , {"text/html",        to_json}
        , {"text/plain",       to_json}],
  {Map, ReqData, Ctx}.

create_path(ReqData, Ctx) ->
  {nodename, Nodename} = lists:keyfind(nodename, 1, Ctx),
  {atom_to_list(Nodename), ReqData, Ctx}.

malformed_request(ReqData, Ctx0) ->
  case wrq:method(ReqData) of
    'GET'  -> {false, ReqData, Ctx0};
    'POST' ->
      case edts_resource_lib:try_make_nodename(ReqData) of
        {ok, Nodename} ->
          Ctx = lists:keystore(nodename, 1, Ctx0, {nodename, Nodename}),
          {false, ReqData, Ctx};
        error ->
          {true, ReqData, Ctx0}
      end
  end.

post_is_create(ReqData, Ctx) ->
  {true, ReqData, Ctx}.

resource_exists(ReqData, Ctx) ->
  case wrq:path_info(nodename, ReqData) of
    undefined -> {true, ReqData, Ctx};
    Name      ->
      case edts_resource_lib:try_make_nodename(Name) of
        error          -> {false, ReqData, Ctx};
        {ok, Nodename} -> {edts:is_edts_node(Nodename), ReqData, Ctx}
      end
  end.

%% Handlers

to_json(ReqData, Ctx) ->
  {ok, Names} = edts:nodes(),
  {mochijson2:encode([{nodes, Names}]), ReqData, Ctx}.

from_json(ReqData, Ctx) ->
  {nodename, Nodename} = lists:keyfind(nodename, 1, Ctx),
  ok = edts:init_node(Nodename),
  {true, ReqData, Ctx}.

%%%_* Internal functions =======================================================

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
