%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% @end
%%% @author Thomas Järvstrand <tjarvstrand@gmail.com>
%%% @copyright
%%% Copyright 2012 Thomas Järvstrand <tjarvstrand@gmail.com>
%%%
%%% This file is part of foo.
%%%
%%% foo is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% foo is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with foo. If not, see <http://www.gnu.org/licenses/>.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration =======================================================
-module(edts_event_server).
-behaviour(gen_server).

%%%_* Exports ==================================================================

%% API
-export([dispatch_event/2,
         listen/0,
         start/0,
         start/1,
         start_link/0,
         start_link/1
        ]).

%% gen_server callbacks
-export([code_change/3,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2
        ]).

%%%_* Includes =================================================================

%%%_* Defines ==================================================================
-define(SERVER, ?MODULE).

-record(state, {events    = queue:new(),
                listeners = queue:new()}).

%%%_* Types ====================================================================
-type state() :: #state{}.

%%%_* API ======================================================================

dispatch_event(EventType, EventInfo) ->
  gen_server:cast(?SERVER, {dispatch_event, {EventType,EventInfo}}).

listen() ->
  gen_server:call(?SERVER, listen, infinity).

%% gen_server callbacks.
%%------------------------------------------------------------------------------
%% @doc Equivalent to start_link([]).
-spec start_link() -> {ok, pid} | ignore | {error,term()}.
%%------------------------------------------------------------------------------
start_link() -> start_link([]).

%%------------------------------------------------------------------------------
%% @doc Starts the server.
-spec start_link([term()]) -> {ok, pid} | ignore | {error,term()}.
%%------------------------------------------------------------------------------
start_link(Args) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%%------------------------------------------------------------------------------
%% @doc Equivalent to start([]).
-spec start() -> {ok, pid} | ignore | {error,term()}.
%%------------------------------------------------------------------------------
start() -> start([]).

%%------------------------------------------------------------------------------
%% @doc Starts the server.
-spec start([term()]) -> {ok, pid} | ignore | {error,term()}.
%%------------------------------------------------------------------------------
start(Args) ->
  gen_server:start({local, ?MODULE}, ?MODULE, Args, []).

%% gen_server callbacks.

%%------------------------------------------------------------------------------
%% @doc Initiates the server.
-spec init(term()) -> {ok, state()} |
                      {ok, state(), timeout()} |
                      ignore |
                      {stop, term()}.
%%------------------------------------------------------------------------------
init(_Args) -> {ok, #state{}}.

%%------------------------------------------------------------------------------
%% @doc Handle call messages
-spec handle_call(term(), pid(), state()) ->
                     {reply, Reply::term(), state()} |
                     {reply, Reply::term(), state(), timeout()} |
                     {noreply, state()} |
                     {noreply, state(), timeout()} |
                     {stop, Reason::term(), Reply::term(), state()} |
                     {stop, Reason::term(), state()}.
%%------------------------------------------------------------------------------
handle_call(listen, From, #state{events = Events0} = State0) ->
  case queue:is_empty(Events0) of
    true  ->
      {noreply, #state{listeners = queue:in(From, State0#state.listeners)}};
    false ->
      {{value, Event}, Events} = queue:out(Events0),
      {reply, {ok, Event}, State0#state{events = Events}}
  end;
handle_call(Message, _From, State) ->
  {reply, {unknown_call, Message}, State}.

%%------------------------------------------------------------------------------
%% @doc Handle cast messages.
-spec handle_cast(Msg::term(), state()) ->
                     {noreply, state()} |
                     {noreply, state(), timeout() | hibernate} |
                     {stop, Reason::term(), state()}.
%%------------------------------------------------------------------------------
handle_cast({dispatch_event, Event}, #state{listeners = Listeners0} = State0) ->
  State =
    case queue:is_empty(Listeners0) of
      true  -> State0#state{events = queue:in(Event, State0#state.events)};
      false ->
        {{value, Listener}, Listeners} = queue:out(Listeners0),
        gen_server:reply(Listener, {ok, Event}),
        State0#state{listeners = Listeners}
    end,
  {noreply, State};
handle_cast(_Message, State) -> {noreply, State}.


%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages
-spec handle_info(Info::term(), state()) -> {noreply, state()} |
                                            {noreply, state(), timeout()} |
                                            {stop, Reason::term(), state()}.
%%------------------------------------------------------------------------------
handle_info(_Info, State) -> {noreply, State}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
-spec terminate(Reason::term(), state()) -> no_return().
%%------------------------------------------------------------------------------
terminate(_Reason, _State) ->
ok.

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed
-spec code_change(term() | {down, term()}, state(), Extra::term()) ->
                     {ok, state()} | {error, Reason::term()}.
%%------------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) -> {ok, State}.



%%%_* Internal functions =======================================================

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

