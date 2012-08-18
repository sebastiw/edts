%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc The main edts server
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration =======================================================
-module(edts_resource_modules).

%%%_* Exports ==================================================================

%% API
%% Webmachine callbacks
-export([ allowed_methods/2
        , content_types_provided/2
        , init/1
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
  {ok, []}.

allowed_methods(ReqData, Ctx) ->
  {['GET'], ReqData, Ctx}.

content_types_provided(ReqData, Ctx) ->
  Map = [ {"application/json", to_json}
        , {"text/html",        to_json}
        , {"text/plain",       to_json}],
  {Map, ReqData, Ctx}.

resource_exists(ReqData, Ctx) ->
  case wrq:path_info(nodename, ReqData) of
    undefined -> false;
    Name      ->
      case edts_resource_lib:try_make_nodename(Name) of
        error          -> {false, ReqData, Ctx};
        {ok, Nodename} -> {edts:is_edts_node(Nodename), ReqData, Ctx}
      end
  end.

%% Handlers

to_json(ReqData, Ctx) ->
  Modules = edts:modules(lists:keyfind(nodename, 1, Ctx)),
  {mochijson2:encode(Modules), ReqData, Ctx}.

%%%_* Internal functions =======================================================


%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
