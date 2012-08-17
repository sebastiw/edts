-module(edts).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
  application:start(inets),
  application:start(crypto),
  application:start(mochiweb),
  application:start(webmachine),
  edts_sup:start_link().

start(_StartType, _Start) ->
  start().

stop(_State) ->
  ok.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

