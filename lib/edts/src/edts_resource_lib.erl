%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration =======================================================
-module(edts_resource_lib).

%%%_* Exports ==================================================================

%% Application callbacks
-export([try_make_nodename/1]).

%%%_* Includes =================================================================

%%%_* Defines ==================================================================

%%%_* Types ====================================================================

%%%_* API ======================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Try to construct a node sname from either a string() or wrq:reqdata().
%% @end
-spec try_make_nodename(string()|wrq:reqdata()) -> node() | error.
%%------------------------------------------------------------------------------
try_make_nodename(NameStr) when is_list(NameStr) ->
  try
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
  end;
try_make_nodename(ReqData) ->
  case wrq:get_qs_value("node", ReqData) of
    undefined -> error;
    Name -> try_make_nodename(Name)
  end.


%%%_* Internal functions =======================================================

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

