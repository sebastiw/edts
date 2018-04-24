%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% code-analysis library for edts. This module is loaded on the project
%%% node.
%%% @end
%%%
%%% @author Thomas Järvstrand <tjarvstrand@gmail.com>
%%% @copyright
%%% Copyright 2012-2013 Thomas Järvstrand <tjarvstrand@gmail.com>
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
-module(edts_module_server).
-behaviour(gen_server).

%%%_* Exports ==================================================================

%% API
-export([get_modules/0,
         start/0,
         start_link/0,
         started_p/0,
         update/0
        ]).

-export([fetch_modules/0]).

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
-record(state, {timerref,
                modules = []}).

%%%_* Types ====================================================================
-type state() :: #state{}.

%%%_* API ======================================================================

get_modules() ->
  gen_server:call(?MODULE, get_modules).

update() ->
  gen_server:call(?MODULE, update).


%%------------------------------------------------------------------------------
%% @doc Starts the server.
-spec start_link() -> {ok, pid} | ignore | {error,term()}.
%%------------------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%%------------------------------------------------------------------------------
%% @doc Returns true if the server is running
-spec started_p() -> boolean().
%%------------------------------------------------------------------------------
started_p() -> whereis(?MODULE) =/= undefined.


%%------------------------------------------------------------------------------
%% @doc Starts the server.
-spec start() -> {ok, pid} | ignore | {error,term()}.
%%------------------------------------------------------------------------------
start() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% gen_server callbacks.

%%------------------------------------------------------------------------------
%% @doc Initiates the server.
-spec init(term()) -> {ok, state()} |
                      {ok, state(), timeout()} |
                      ignore |
                      {stop, term()}.
%%------------------------------------------------------------------------------
init(_Args) ->
  {ok, TRef} = timer:send_after(3000, update),
  {ok, #state{modules = fetch_modules(),
              timerref = TRef}}.

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
handle_call(get_modules, _From, State) ->
  {reply, {ok, State#state.modules}, State};
handle_call(update, _From, State) ->
  {reply, ok, State#state{modules = fetch_modules()}};
handle_call(Message, _From, State) ->
  {reply, {unknown_call, Message}, State}.

%%------------------------------------------------------------------------------
%% @doc Handle cast messages.
-spec handle_cast(Msg::term(), state()) ->
                     {noreply, state()} |
                     {noreply, state(), timeout() | hibernate} |
                     {stop, Reason::term(), state()}.
%%------------------------------------------------------------------------------
handle_cast(_Message, State) -> {noreply, State}.


%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages
-spec handle_info(Info::term(), state()) -> {noreply, state()} |
                                            {noreply, state(), timeout()} |
                                            {stop, Reason::term(), state()}.
%%------------------------------------------------------------------------------
handle_info(update, State) ->
  Modules = fetch_modules(),
  {ok, TRef} = timer:send_after(3000, update),
  {noreply, State#state{modules = Modules, timerref = TRef}};
handle_info(_Info, State) ->
  {noreply, State}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
-spec terminate(Reason::term(), state()) -> no_return().
%%------------------------------------------------------------------------------
terminate(_Reason, #state{timerref = Ref}) ->
  timer:cancel(Ref),
  ok.

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed
-spec code_change(term() | {down, term()}, state(), Extra::term()) ->
                     {ok, state()} | {error, Reason::term()}.
%%------------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) -> {ok, State}.



%%%_* Internal functions =======================================================

fetch_modules() ->
  Loaded = [M || {M, _} <- code:all_loaded()],
  {_ErlLibDirs, AppDirs} = edts_util:lib_and_app_dirs(),
  AppMods = lists:flatmap(fun fetch_modules_in_dir/1, AppDirs),
  ordsets:to_list(ordsets:union(ordsets:from_list(Loaded),
                                ordsets:from_list(AppMods))).

fetch_modules_in_dir(Dir) ->
  Files = filelib:wildcard("*.beam", Dir),
  [list_to_atom(filename:rootname(filename:basename(F))) || F <- Files].

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
