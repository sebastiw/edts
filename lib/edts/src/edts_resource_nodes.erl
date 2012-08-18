%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc The main edts server
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration =======================================================
-module(edts_resource_nodes).

%%%_* Exports ==================================================================
-export([ content_types_provided/2
        , init/1
        , to_json/2]).

%% API

%%%_* Includes =================================================================
-include_lib("webmachine/include/webmachine.hrl").

%%%_* Defines ==================================================================
%%%_* Types ====================================================================
%%%_* API ======================================================================

init(_Config) ->
    {ok, []}.

content_types_provided(ReqData, Context) ->
    Map = [ {"application/json", to_json}
          , {"text/html",        to_json}
          , {"text/plain",       to_json}],
    {Map, ReqData, Context}.

to_json(ReqData, Context) ->
    {mochijson2:encode([{nodes, nodes()}]), ReqData, Context}.

%%%_* Internal functions =======================================================

