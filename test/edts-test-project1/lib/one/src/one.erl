-module(one).

-include_lib("eunit/include/eunit.hrl").
-include_lib("one/include/test.hrl").

-export([one/1]).

-record(record, {field}).

-define(macro_ok, ok).

-spec one(any()) -> string().
one(Foo) -> lists:keymap(fo, bar, a),
  case one_two:one_two_fun(a) of
    Foo -> ?macro_ok;
    _   -> error(oo)
  end,
  two:two_fun(),
  some_function("hello world!").


-spec some_function(string()) -> atom().
some_function(Foo) ->
  one_two:one_two_fun(),
  #record{field = list_to_atom(Foo)}.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
