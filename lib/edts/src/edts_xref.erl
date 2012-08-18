%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration =======================================================
-module(edts_xref).

%%%_* Exports ==================================================================

%% Application callbacks
-export([start/0]).

%%%_* Includes =================================================================

%%%_* Defines ==================================================================
-define(SERVER, ?MODULE).
%%%_* Types ====================================================================

%%%_* API ======================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Starts the edts xref-server on the local node.
%% @end
-spec start() -> ok.
%%------------------------------------------------------------------------------
start() ->
  {ok, _Pid}= xref:start(?SERVER),
  %% ok = xref:set_default(?SERVER, [{verbose,false}, {warnings,false}]),
  Paths = code:get_path(),
  ok = xref:set_library_path(?SERVER, Paths),
  lists:foreach(fun(D) ->
                    case xref:add_application(?SERVER, filename:dirname(D)) of
                      {error, _, _} -> xref:add_directory(?SERVER, D);
                      {ok, _}       -> ok
                    end
                end,
                Paths).


%%%_* Internal functions =======================================================

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

