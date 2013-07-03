%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Erlang interpreter interface through a gen_server for communication
%%% with external processes
%%% @end
%%% @author João Neves <sevenjp@gmail.com>
%%% @copyright
%%% Copyright 2012 João Neves <sevenjp@gmail.com>
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
-export([ continue/0
        , get_breakpoints/0
        , interpret_module/2
        , module_interpreted_p/1
        , maybe_attach/1
        , module_interpretable_p/1
        , step/0
        , step_out/0
        , stop_debug/0
        , toggle_breakpoint/2
        , wait_for_debugger/0 ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%%_* Includes =================================================================
-include_lib("eunit/include/eunit.hrl").

%%%_* Defines ==================================================================
-define(SERVER, ?MODULE).

-record(dbg_state, {
          debugger = undefined   :: undefined | pid(),
          proc = unattached      :: unattached | pid(),
          stack = {1, 1}         :: {non_neg_integer(), non_neg_integer()},
          listeners = []         :: [term()],
          interpretation = false :: boolean()
         }).

%%%_* Types ====================================================================
-type state():: #dbg_state{}.

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
%% Potentially attach to an interpreter process Pid. Will not
%% reattach if already attached.
%% @end
-spec maybe_attach(Pid :: pid()) -> {attached, pid(), pid()}
                                  | {already_attached, pid(), pid()}.
%%------------------------------------------------------------------------------
maybe_attach(Pid) ->
  case gen_server:call(?SERVER, {attach, Pid}) of
    {ok, attach, AttPid} ->
      {attached, AttPid, Pid};
    {error, already_attached, AttPid} ->
      {already_attached, AttPid, Pid}
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Return true if Module is interpretable, false otherwise
%% @end
-spec module_interpretable_p(module()) -> boolean().
%%------------------------------------------------------------------------------
module_interpretable_p(Module) ->
  case int:interpretable(Module) of
    true       -> true;
    {error, _} -> false
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Get all breakpoints and their status in the current interpreter
%% @end
-spec get_breakpoints() -> [{ { Module :: module()
                              , Line   :: non_neg_integer()
                              }
                            , Options  :: [term()]
                            }].
%%------------------------------------------------------------------------------
get_breakpoints() ->
  gen_server:call(?SERVER, get_breakpoints).

%%------------------------------------------------------------------------------
%% @doc
%% Change the interpretation state of Module depending on Intepret. Returns
%% {ok, Bool} where Bool is true if Module is now interpreted and false
%% otherwise.
%% @end
-spec interpret_module(Modules   :: module(),
                       Interpret :: true | false | toggle) ->
                          {ok, boolean()}.
%%------------------------------------------------------------------------------
interpret_module(Module, toggle) ->
  interpret_module(Module, not module_interpreted_p(Module));
interpret_module(Module, true) ->
  case module_interpretable_p(Module) of
    false -> {error, uninterpretable};
    true  ->
      {module, Module} = int:i(Module),
      true
  end;
interpret_module(Module, false) ->
  int:n(Module),
  false.


%%------------------------------------------------------------------------------
%% @doc
%% Reports if Module is interpreted.
%% @end
-spec module_interpreted_p(Module :: module()) -> boolean().
%%------------------------------------------------------------------------------
module_interpreted_p(Module) ->
  lists:member(Module, int:interpreted()).


%%------------------------------------------------------------------------------
%% @doc
%% Toggles a breakpoint at Module:Line.
%% @end
-spec toggle_breakpoint(Module :: module(), Line :: non_neg_integer()) ->
                           {ok, set, {Module, Line}}
                         | {ok, unset, {Module, Line}}.
%%------------------------------------------------------------------------------
toggle_breakpoint(Module, Line) ->
  gen_server:call(?SERVER, {toggle_breakpoint, Module, Line}).


%%------------------------------------------------------------------------------
%% @doc
%% Waits for debugging to be triggered by a breakpoint and returns
%% relevant module and line information.
%% @end
-spec wait_for_debugger() -> {ok, {module(), non_neg_integer()}}.
%%------------------------------------------------------------------------------
wait_for_debugger() ->
  gen_server:call(?SERVER, wait_for_debugger, infinity).

%%------------------------------------------------------------------------------
%% @doc
%% Orders the debugger to continue execution until it reaches another
%% breakpoint or execution terminates.
%% @end
-spec continue() -> ok.
%%------------------------------------------------------------------------------
continue() ->
  gen_server:call(?SERVER, continue, infinity).

%%------------------------------------------------------------------------------
%% @doc
%% Orders the debugger to step in execution.
%% @end
-spec step() -> ok.
%%------------------------------------------------------------------------------
step() ->
  gen_server:call(?SERVER, step, infinity).

%%------------------------------------------------------------------------------
%% @doc
%% Order the debugger to step out of the current function.
%% @end
-spec step_out() -> ok.
%%------------------------------------------------------------------------------
step_out() ->
  gen_server:call(?SERVER, step_out, infinity).

%%------------------------------------------------------------------------------
%% @doc
%% Stop debugging
%% @end
-spec stop_debug() -> ok.
%%------------------------------------------------------------------------------
stop_debug() ->
  gen_server:call(?SERVER, stop_debug).

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
  int:auto_attach([break], {?MODULE, maybe_attach, []}),
  {ok, #dbg_state{}}.

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%
-spec handle_call(term(), {pid(), atom()}, state()) ->
                     {reply, Reply::term(), state()} |
                     {reply, Reply::term(), state(), timeout()} |
                     {noreply, state()} |
                     {noreply, state(), timeout()} |
                     {stop, Reason::atom(), term(), state()} |
                     {stop, Reason::atom(), state()}.
%%------------------------------------------------------------------------------
handle_call({attach, Pid}, _From, #dbg_state{proc = unattached} = State) ->
  register_attached(self()),
  int:attached(Pid),
  error_logger:info_msg("Debugger ~p attached to ~p~n", [self(), Pid]),
  {reply, {ok, attach, self()}, State#dbg_state{proc = Pid}};
handle_call({attach, Pid}, _From, #dbg_state{debugger=Dbg, proc=Pid} = State) ->
  {reply, {error, {already_attached, Dbg, Pid}}, State};
handle_call({toggle_breakpoint, Module, Line}, _From, State) ->
  Reply = case lists:keymember({Module, Line}, 1, int:all_breaks()) of
            true  -> int:delete_break(Module, Line),
                     {ok, unset, {Module, Line}};
            false -> int:break(Module, Line),
                     {ok, set, {Module, Line}}
          end,
  {reply, Reply, State};

handle_call(get_breakpoints, _From, State) ->
  {reply, {ok, int:all_breaks()}, State};

handle_call(wait_for_debugger, From, State) ->
  Listeners = State#dbg_state.listeners,
  {noreply, State#dbg_state{listeners = [From|Listeners]}};

handle_call(_Cmd, _From, #dbg_state{proc = unattached} = State) ->
  {reply, {error, unattached}, State};

handle_call(continue, From, #dbg_state{proc = Pid} = State) ->
  int:continue(Pid),
  Listeners = State#dbg_state.listeners,
  {noreply, State#dbg_state{listeners = [From|Listeners]}};

handle_call(step, From, #dbg_state{proc = Pid} = State) ->
  int:step(Pid),
  Listeners = State#dbg_state.listeners,
  {noreply, State#dbg_state{listeners = [From|Listeners]}};

handle_call(step_out, From, #dbg_state{proc = Pid} = State) ->
  int:finish(Pid),
  Listeners = State#dbg_state.listeners,
  {noreply, State#dbg_state{listeners = [From|Listeners]}};

handle_call(stop_debug, _From, #dbg_state{proc = Pid}) ->
  exit(Pid, kill),
  {reply, {ok, finished}, #dbg_state{}}.


%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
-spec handle_cast(Msg::term(), state()) -> {noreply, state()} |
                                           {noreply, state(), timeout()} |
                                           {stop, Reason::atom(), state()}.
%%------------------------------------------------------------------------------
handle_cast({register_attached, Pid}, State) ->
  {noreply, State#dbg_state{debugger = Pid}};
handle_cast({notify, Info}, #dbg_state{listeners = Listeners} = State) ->
  notify(Info, Listeners),
  {noreply, State};
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
%% Hit a breakpoint
handle_info({Meta, {break_at, Module, Line, _Cur}}, State) ->
  Bindings = int:meta(Meta, bindings, nostack),
  File = int:file(Module),
  notify({break, File, {Module, Line}, Bindings}),
  {noreply, State};

%% Became idle (not executing any code under debugging)
handle_info({_Meta, idle}, State) ->
  notify(idle),
  {noreply, State};

%% Came back from uninterpreted code
handle_info({_Meta, {re_entry, _, _}}, State) ->
  {noreply, State};

%% Running code, but not telling anything really relevant
handle_info({_Meta, running}, State) ->
  {noreply, State};

%% Something attached to the debugger (most likely ourselves)
handle_info({_Meta, {attached, _, _, _}}, State) ->
  {noreply, State};

%% Process under debug terminated
handle_info({_Meta, {exit_at, _, _Reason, _}}, State) ->
  {noreply, State};

handle_info(Msg, State) ->
  error_logger:info_msg("Unexpected message: ~p~n", [Msg]),
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
  int:auto_attach(false),
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

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Notifies all registered debugger clients of a change in debugger state
%% through Info
%%
-spec notify(Info :: term()) -> ok.
%%------------------------------------------------------------------------------
notify(Info) ->
  gen_server:cast(?SERVER, {notify, Info}).

notify(_, []) ->
  ok;
notify(Info, [Client|R]) ->
  gen_server:reply(Client, {ok, Info}),
  notify(Info, R).

%%------------------------------------------------------------------------------
%% @doc
%% Register in idbg_server as a debugger process attached to Pid.
%%
-spec register_attached(Pid :: pid()) -> ok.
%%------------------------------------------------------------------------------
register_attached(Pid) ->
  gen_server:cast(?SERVER, {register_attached, Pid}).

%%%_* Unit tests ===============================================================

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
