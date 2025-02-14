-module(three).

-include("../include/three.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-export([three_fun/1]).

three_fun(Expected) ->
  MS = ets:fun2ms(fun(#three{type = Type}) when Type == Expected -> true end),
  ets:select(?MODULE, MS).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
