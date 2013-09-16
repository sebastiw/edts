%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Erlang interpreter interface through a gen_server for communication
%%% with external processes
%%% @end
%%% @author Thomas Järvstrand <tjarvstrand@gmail.com>
%%% @copyright
%%% Copyright 2012 Thomas Järvstrand <tjarvstrand@gmail.com>
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
-module(edts_debug_server).

-behaviour(gen_server).

%%%_* Exports =================================================================

%% server API
-export([ensure_started/0, start/0, started_p/0, stop/0, start_link/0]).

%% Debugger API
-export([step/0,
         step_out/0,
         stop_debug/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%%_* Includes =================================================================
-include_lib("eunit/include/eunit.hrl").

%%%_* Defines ==================================================================
-define(SERVER, ?MODULE).

-record(state, {}).

%%%_* Types ====================================================================
-type state():: #state{}.

%%%_* API ======================================================================
ensure_started() ->
  case started_p() of
    true  -> ok;
    false -> start()
  end.

start() ->
  gen_server:start({local, ?SERVER}, ?MODULE, [], []),
  ok.

stop() ->
  ok.

%%------------------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
%%-----------------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

started_p() -> whereis(?SERVER) =/= undefined.

%%%_* gen_server callbacks  ====================================================

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
-spec init(list()) -> {ok, state()} |
                      {ok, state(), timeout()} |
                      ignore |
                      {stop, atom()}.
%%------------------------------------------------------------------------------
init([]) ->
  %% TODO Monitor dbg_iserver and restart/subscribe if it goes down.
  int:subscribe(),
  {ok, #state{}}.

handle_call(Msg, _From, State) ->
  error_logger:error_msg("Unknown call ~p", [Msg]),
  {reply, {error, {unknown_msg, Msg}}, State}.

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
-spec handle_cast(Msg::term(), state()) -> {noreply, state()} |
                                           {noreply, state(), timeout()} |
                                           {stop, Reason::atom(), state()}.
%%------------------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%% @doc Handling all non call/cast messages
%% @end
%%
-spec handle_info(term(), state()) -> {noreply, state()} |
                                      {noreply, state(), Timeout::timeout()} |
                                      {stop, Reason::atom(), state()}.
%%------------------------------------------------------------------------------
handle_info({int, Msg}, State) ->
  maybe_dispatch_event(Msg),
  {noreply, State};
handle_info(Msg, State) ->
  error_logger:info_msg("~p: Unexpected message: ~p~n", [?MODULE, Msg]),
  {noreply, State}.


%%------------------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
-spec terminate(Reason::atom(), state()) -> any().
%%------------------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
-spec code_change(OldVsn::string(), state(), Extra::term()) -> {ok, state()}.
%%------------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%_* Internal functions =======================================================

maybe_dispatch_event(Msg) ->
  case dispatch_event_p(Msg) of
    true  -> dispatch_event(Msg);
    false -> ok
  end.

dispatch_event_p(Event) ->
  is_tuple(Event) andalso
  lists:member(element(1, Event),
               [interpret,
                no_interpret,
                new_process,
                new_status,
                new_break,
                delete_break,
                no_break]).


dispatch_event(Msg) ->
  edts_event:dispatch_event(edts_debug, fmt_event(Msg)).

fmt_event({interpret, Mod}) ->
  [{type, interpret},
   {module, Mod}];
fmt_event({no_interpret, Mod}) ->
  [{type, no_interpret},
   {module, Mod}];
fmt_event({new_process, Pid, Fun, Status, Info}) ->
  [{type,     new_process},
   {pid,      edts_util:pid2atom(Pid)},
   {function, Fun},
   {status,   Status},
   {info,     Info}];
fmt_event({new_status, Pid, Status, Info}) ->
  [{type,   new_status},
   {pid,    edts_util:pid2atom(Pid)},
   {status, Status},
   {info,   Info}];
fmt_event({new_break, {{Mod, Line}, Options}}) ->
  [{type,   new_break},
   {module, Mod},
   {line,   Line},
   {options, Options}];
fmt_event({delete_break, {Mod, Line}}) ->
  [{type,   delete_break},
   {module, Mod},
   {line,   Line}];
fmt_event({no_break, Mod}) ->
  [{type,   delete_break},
   {module, Mod}].


%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
