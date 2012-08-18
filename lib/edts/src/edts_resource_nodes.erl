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
        , post_is_create/2]).

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

allow_missing_post(ReqData, Context) ->
  {true, ReqData, Context}.

allowed_methods(ReqData, Context) ->
  {['GET', 'POST'], ReqData, Context}.

content_types_accepted(ReqData, Context) ->
  Map = [ {"application/json", from_json}],
  {Map, ReqData, Context}.


content_types_provided(ReqData, Context) ->
  Map = [ {"application/json", to_json}
        , {"text/html",        to_json}
        , {"text/plain",       to_json}],
  {Map, ReqData, Context}.

create_path(ReqData, Context) ->
  {nodename, Nodename} = lists:keyfind(nodename, 1, Context),
  {atom_to_list(Nodename), ReqData, Context}.

malformed_request(ReqData, Context0) ->
  case wrq:method(ReqData) of
    'GET'  -> {false, ReqData, Context0};
    'POST' ->
      case try_make_nodename(ReqData) of
        {ok, Nodename} ->
          Context = lists:keystore(nodename, 1, Context0, {nodename, Nodename}),
          {false, ReqData, Context};
        error ->
          {true, ReqData, Context0}
      end
  end.

post_is_create(ReqData, Context) ->
  {true, ReqData, Context}.

%% Handlers

to_json(ReqData, Context) ->
  {mochijson2:encode([{nodes, nodes()}]), ReqData, Context}.

from_json(ReqData, Context) ->
  {nodename, Nodename} = lists:keyfind(nodename, 1, Context),
  edts_dist:connect(Nodename),
  {true, ReqData, Context}.

%%%_* Internal functions =======================================================

try_make_nodename(ReqData) ->
  try
    NameStr = wrq:get_qs_value("node", ReqData),
    {ok, Name} =
      case string:chr(NameStr, $@) of
        0 ->
          {ok, edts_dist:make_sname(NameStr)};
        _ ->
          [Name0, Host] = string:tokens(NameStr, "@"),
          {ok, edts_dist:make_sname(Name0, Host)}
      end,
    Name
  catch
    _:_ -> error
  end.


%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
