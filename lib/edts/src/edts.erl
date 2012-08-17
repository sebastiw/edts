-module(edts).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    edts_sup:start_link().

start(_StartType, _Start) ->
    start().

stop(_State) ->
    ok.
