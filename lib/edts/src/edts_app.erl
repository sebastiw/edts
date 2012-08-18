%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc The edts otp-application entry-point.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration =======================================================
-module(edts_app).
-behaviour(application).

%%%_* Exports ==================================================================

%% API
-export([start/0]).

%% Application callbacks
-export([ start/2
        , stop/1]).

%%%_* Includes =================================================================

%%%_* Defines ==================================================================

%%%_* Types ====================================================================

%%%_* API ======================================================================

%% Start the whole shebang.
start() ->
  application:start(inets),
  application:start(crypto),
  application:start(edts).

%% Application callbacks
start(_StartType, _Start) ->
  edts_dist:connect_all(),
  edts_sup:start_link().

stop(_State) ->
  ok.

%%%_* Internal functions =======================================================

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

