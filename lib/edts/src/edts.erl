-module(edts).

-export([start/0]).

start() ->
  application:start(inets),
  application:start(crypto),
  application:start(mochiweb),
  application:start(webmachine),
  application:start(edts).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
