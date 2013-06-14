%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc This is the EUnit listner used to collect the results when running
%%%      EUnit tests.
%%% @end
%%% @author Håkan Nilsson <haakan@gmail.com>
%%% @copyright
%%% Copyright 2012 Håkan Nilsson <haakan@gmail.com>
%%%           2013 Thomas Järvstrand <tjarvstrand@gmail.com>
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

%%%_* Records ==================================================================
-record(state, {parent          :: pid(),
                ref             :: reference(),
                successful = [] :: [event()],
                failed     = [] :: [event()],
                cancelled  = [] :: [event()]
               }).

%%%_* Types ====================================================================
-type event() :: proplists:proplist().

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
      #state{ref    = Reference,
             parent = proplists:get_value(parent, Options)}
  end.

-spec handle_begin(group|test, proplists:proplist(), #state{}) -> #state{}.
handle_begin(L, Data, State) ->
  debug("handle_begin ~p: ~p", [L, Data]),
  State.

-spec handle_end(group|test, proplists:proplist(), #state{}) -> #state{}.
handle_end(test, Data, State) ->
  debug("handle_end test: ~p", [Data]),
  case proplists:get_value(status, Data, ok) of
    {error, _Err} -> State#state{failed     = [Data|State#state.failed]};
    ok            -> State#state{successful = [Data|State#state.successful]}
  end;
handle_end(L, Data, State) ->
  debug("handle_end ~p: ~p", [L, Data]),
  State.

-spec handle_cancel(group|test, proplists:proplist(), #state{}) -> #state{}.
handle_cancel(group = L, Data, State) ->
  debug("handle_cancel ~p: ~p", [L, Data]),
  %% Can't handle this atm because there's not always MFA info in the error
  State;
handle_cancel(test = L, Data, State) ->
  debug("handle_cancel ~p: ~p", [L, Data]),
  State#state{cancelled = [Data|State#state.cancelled]}.


-spec terminate({ok, proplists:proplist()}, #state{}) ->
              {result, reference(), {edts_eunit:summary(),
                                     [edts_eunit:test()]}}.
terminate({ok, Summary}, #state{ref=Ref, parent=Parent} = State) ->
  Result =
    orddict:from_list([{successful, State#state.successful},
                       {failed,     State#state.failed},
                       {cancelled,  State#state.cancelled}]),
  debug("terminate: ~p", [Summary]),
  Parent ! {result, Ref, {orddict:from_list(Summary), Result}};
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
  flush_mailbox(),
  self() ! {start, ref},
  ?assertEqual(#state{ref = ref, parent = foo}, init([{parent, foo}])).

handle_begin_test_() ->
  [ ?_assertEqual(#state{},
                  handle_end(group, [{status, ok}], #state{}))
  ].

handle_end_test_() ->
  [ ?_assertEqual(#state{successful = [[{status, ok}]]},
                  handle_end(test, [{status, ok}], #state{})),
    ?_assertEqual(#state{failed = [[{status, {error, an_error}}]]},
                  handle_end(test, [{status, {error, an_error}}], #state{})),
    ?_assertEqual(#state{},
                  handle_end(group, [{status, ok}], #state{}))
  ].

terminate_test() ->
  Ref   = make_ref(),
  State = #state{ref=Ref, parent=self()},
  Results = [{cancelled,[]},{failed,[]},{successful,[]}],
  Summary = [{a, 1}, {b, 2}],
  Exp   = {result, Ref, {Summary, Results}},
  ?assertEqual(Exp, terminate({ok, [{b, 2}, {a, 1}]}, State)),
  ?assertEqual(Exp, receive Exp -> Exp end),
  ?assertEqual({error, foo}, terminate(foo, State)),
  ?assertEqual({error, foo}, receive {error, _} = ExpErr -> ExpErr end).

handle_cancel_test_() ->
  [?_assertEqual(#state{cancelled = [data]},
                 handle_cancel(test, data, #state{})),
   ?_assertEqual(#state{},
                 handle_cancel(group, data, #state{}))
  ].

flush_mailbox() ->
  receive _ -> flush_mailbox()
  after   0 -> ok
  end.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
