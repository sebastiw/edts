-module(one_two).

-export([one_two_fun/1]).

one_two_fun(ok) ->
  two:two_fun().

