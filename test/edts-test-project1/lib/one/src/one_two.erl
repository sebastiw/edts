-module(one_two).

-export([one_two_fun/1]).

one_two_fun(ok) ->
  two:two_fun().

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
