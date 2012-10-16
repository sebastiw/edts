%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc This is the EUnit listner used to collect the results when running
%%%      EUnit tests.
%%% @end
%%% @author Hakan Nilsson
%%% @copyright
%%% Copyright 2012 Hakan Nilsson
%%%
%%% This file is part of EDTS.
%%%
%%% EDTS is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU Lesser General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% EDTS is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Lesser General Public License for more details.
%%%
%%% You should have received a copy of the GNU Lesser General Public License
%%% along with EDTS. If not, see <http://www.gnu.org/licenses/>.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration =======================================================
-module(edts_eunit_listener).

-behaviour(eunit_listener).

%%%_* Includes =================================================================
-include_lib("eunit/include/eunit.hrl").

%%%_* Exports ==================================================================

-export([ handle_begin/3
        , handle_cancel/3
        , handle_end/3
        , init/1
        , start/0
        , start/1
        , terminate/2
        ]).

%%%_* Defines ==================================================================
%-define(DEBUG, true).

%%%_* Records ==================================================================
-record(state, { parent     :: pid()
               , ref        :: reference()
               , tests = [] :: [edts_eunit:test()]
               }).

%%%_* Types ====================================================================

%%%_* API ======================================================================

-spec start() -> pid().
start() -> start([]).

-spec start(proplists:proplist()) -> pid().
start(Options) -> eunit_listener:start(?MODULE, Options).

-spec init(proplists:proplist()) -> #state{}.
init(Options) ->
  debug("init, waiting for start..."),
  receive
    {start, Reference} ->
      #state{ ref    = Reference
            , parent = proplists:get_value(parent, Options)
            }
  end.

-spec handle_begin(group|test, proplists:proplist(), #state{}) -> #state{}.
handle_begin(test, Data, #state{tests=Tests} = State) ->
  debug("handle_begin test: ~p", [Data]),
  Source = proplists:get_value(source, Data),
  case orddict:is_key(Source, Tests) of
    true  -> State;
    false -> State#state{tests = orddict:store(Source, [], Tests)}
  end;
handle_begin(L, Data, State) ->
  debug("handle_begin ~p: ~p", [L, Data]),
  State.

-spec handle_end(group|test, proplists:proplist(), #state{}) -> #state{}.
handle_end(test, Data, State) ->
  debug("handle_end test: ~p", [Data]),
  case proplists:get_value(status, Data, ok) of
    {error, Err} -> add_fail(Err, Data, State);
    ok           -> State;
    _Other       -> State
  end;
handle_end(L, Data, State) ->
  debug("handle_end ~p: ~p", [L, Data]),
  State.

-spec add_fail(tuple(), proplists:proplist(), #state{}) -> #state{}.
add_fail({_What, How, _St}, Data, #state{tests=Tests} = State) ->
  Source = proplists:get_value(source, Data),
  Fail   = mk_fail(How),
  Fails  = orddict:fetch(Source, Tests),
  State#state{tests = orddict:store(Source, [Fail|Fails], Tests)}.

-spec mk_fail({edts_eunit:reason(), proplists:proplist()}
              | edts_eunit:reason()) -> edts_eunit:info().
mk_fail({Reason, Info}) -> orddict:from_list([{reason, Reason}|Info]);
mk_fail(Reason)         -> orddict:from_list([{reason, Reason}]).

-spec handle_cancel(group|test, proplists:proplist(), #state{}) -> #state{}.
handle_cancel(L, Data, State) ->
  debug("handle_cancel ~p: ~p", [L, Data]),
  State.

-spec terminate({ok, proplists:proplist()}, #state{}) ->
              {result, reference(), {edts_eunit:summary(),
                                     [edts_eunit:test()]}}.
terminate({ok, Summary}, #state{ref=Ref, parent=Parent, tests=Tests}) ->
  debug("terminate: ~p", [Summary]),
  Parent ! {result, Ref, {orddict:from_list(Summary), Tests}};
terminate(Other, #state{parent=Parent}) ->
  debug("terminate: ~p", [Other]),
  Parent ! {error, Other}.

debug(Str) -> debug(Str, []).

-ifdef(DEBUG).
debug(FmtStr, Args) -> error_logger:info_msg(FmtStr, Args).
-else.
debug(_FmtStr, _Args) -> ok.
-endif.

%%%_* Unit tests ===============================================================

init_test() ->
  self() ! {start, ref},
  ?assertEqual(#state{ref = ref, parent = foo}, init([{parent, foo}])).

handle_begin_test_() ->
  [ ?_assertEqual(#state{tests=[{a, []}]},
                  handle_begin(test, [{source, a}], #state{tests=[]}))
  , ?_assertEqual(#state{tests=[{a, [1]}]},
                  handle_begin(test, [{source, a}], #state{tests=[{a, [1]}]}))
  , ?_assertEqual(#state{tests=[{a, [1]}, {b, []}]},
                  handle_begin(test, [{source, b}], #state{tests=[{a, [1]}]}))
  , ?_assertEqual(#state{}, handle_begin(foo, [], #state{}))
  ].

handle_end_test_() ->
  [ ?_assertEqual(#state{tests=[{foo, []}]},
                  handle_end(test, [], #state{tests=[{foo, []}]}))
  , ?_assertEqual(#state{tests=[{foo, []}]},
                  handle_end(foo, [], #state{tests=[{foo, []}]}))
  , ?_assertEqual(#state{tests=[{foo, []}]},
                  handle_end(test, [{status, asdf}], #state{tests=[{foo, []}]}))
  , ?_assertEqual(#state{tests=[{foo, [[{reason, bar}]]}]},
                  handle_end(test, [ {status, {error, {error, bar, st}}}
                                   , {source, foo}
                                   ], #state{tests=[{foo, []}]}))
  , ?_assertEqual(#state{tests=[{foo, [[{a, 1}, {reason, bar}]]}]},
                  handle_end(test,
                             [ {status, {error, {error, {bar, [{a,1}]}, st}}}
                             , {source, foo}
                             ], #state{tests=[{foo, []}]}))
  , ?_assertEqual(#state{tests=[{foo, [[{a, 1}, {reason, bar}], baz]}]},
                  handle_end(test,
                             [ {status, {error, {error, {bar, [{a,1}]}, st}}}
                             , {source, foo}
                             ], #state{tests=[{foo, [baz]}]}))
  ].

terminate_test() ->
  Ref   = make_ref(),
  State = #state{ref=Ref, parent=self(), tests=[foo, bar]},
  Exp   = {result, Ref, {[{a, 1}, {b, 2}], [foo, bar]}},
  ?assertEqual(Exp, terminate({ok, [{b, 2}, {a, 1}]}, State)),
  ?assertEqual(Exp, receive Exp -> Exp end),
  ?assertEqual({error, foo}, terminate(foo, State)),
  ?assertEqual({error, foo}, receive ExpErr -> ExpErr end).

mk_fail_test_() ->
  [ ?_assertEqual([{reason, foo}],             mk_fail(foo))
  , ?_assertEqual([{reason, foo}],             mk_fail({foo, []}))
  , ?_assertEqual([{aaa, bar}, {reason, foo}], mk_fail({foo, [{aaa, bar}]}))
  ].

handle_cancel_test() ->
  State = #state{ref=foo, parent=bar, tests=baz},
  ?assertEqual(State, handle_cancel(l, data, State)).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
