-module(test_module).

-export([bar/1]).

-import(lists, [any/2]).


-include_lib("edts/test/test_2.hrl").
-include("../test/test.hrl").

-record(rec,
        {ord = now()}).

-define(yo, yaow).

bar(ping) -> pong;

bar(_Hej) -> 'bar'(?yo).
