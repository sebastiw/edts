%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc The main edts server
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
-module(edts_server).
-behaviour(gen_server).

%%%_* Exports ==================================================================

%% API
-export([ ensure_node_initialized/1
        , init_node/1
        , is_node/1
        , node_available_p/1
        , nodes/0
        , start_link/0]).

%% gen_server callbacks
-export([ code_change/3
        , init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2]).

%%%_* Includes =================================================================

%%%_* Defines ==================================================================
-define(SERVER, ?MODULE).
-record(node, { name     = undefined :: atom()
              , promises = []        :: [rpc:key()]}).

-record(state, {nodes = [] :: edts_node()}).

%%%_* Types ====================================================================
-type edts_node() :: #node{}.
-type state():: #state{}.

%%%_* API ======================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
%%------------------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%------------------------------------------------------------------------------
%% @doc
%% Wait for Node's initialization to finish (wait for all its promises to be
%% fulfilled).
%% @end
%%
-spec ensure_node_initialized(node()) -> ok.
%%------------------------------------------------------------------------------
ensure_node_initialized(Node) ->
  gen_server:call(?SERVER, {ensure_node_initialized, Node}, infinity).


%%------------------------------------------------------------------------------
%% @doc
%% Initializes a new edts node.
%% @end
%%
-spec init_node(node()) -> ok.
%%------------------------------------------------------------------------------
init_node(Node) ->
  gen_server:call(?SERVER, {init_node, Node}, infinity).

%%------------------------------------------------------------------------------
%% @doc
%% Returns true iff Node is registered with this edts instance.
%% @end
%%
-spec is_node(node()) -> boolean().
%%------------------------------------------------------------------------------
is_node(Node) ->
  gen_server:call(?SERVER, {is_node, Node}, infinity).

%%------------------------------------------------------------------------------
%% @doc
%% Returns true iff Node is registered with this edts instance and has finished
%% its initialization.
%% @end
%%
-spec node_available_p(node()) -> boolean().
%%------------------------------------------------------------------------------
node_available_p(Node) ->
  gen_server:call(?SERVER, {node_available_p, Node}, infinity).


%%------------------------------------------------------------------------------
%% @doc
%% Returns a list of the edts_nodes currently registered with this
%% edts-server instance
%% @end
%%
-spec nodes() -> [node()].
%%------------------------------------------------------------------------------
nodes() ->
  gen_server:call(?SERVER, nodes, infinity).


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
  net_kernel:monitor_nodes(true, [{node_type, all}]),
  {ok, #state{}}.

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
handle_call({ensure_node_initialized, Name}, _From, State) ->
    case node_find(Name, State) of
      false -> {reply, {error, not_found}, State};
      #node{promises = [_|_]} = Node->
        lists:foreach(fun rpc:yield/1, Node#node.promises),
        {reply, ok, node_store(Node#node{promises = []}, State)};
      #node{} ->
        {reply, ok, State}
    end;
handle_call({init_node, Name}, _From, State) ->
  edts_log:info("Node ~p Initializing.", [Name]),
  Node = #node{name = Name, promises = edts_dist:init_node(Name)},
  {reply, ok, node_store(Node, State)};
handle_call({is_node, Name}, _From, State) ->
  Reply = case node_find(Name, State) of
            #node{} -> true;
            false   -> false
          end,
  {reply, Reply, State};
handle_call({node_available_p, Name}, _From, State) ->
  Reply = case node_find(Name, State) of
            #node{promises = []} -> true;
            #node{}              -> false;
            false                -> false
          end,
  {reply, Reply, State};
handle_call(nodes, _From, #state{nodes = Nodes} = State) ->
  {reply, {ok, [N#node.name || N <- Nodes]}, State};
handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

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
handle_info({Pid, {promise_reply, R}}, #state{nodes = Nodes0} = State) ->
  Nodes = [Node#node{promises = lists:delete(Pid, Node#node.promises)}
           || Node <- Nodes0],
  edts_log:info("Promise reply from ~p: ~p", [node(Pid), R]),
  {noreply, State#state{nodes = Nodes}};
handle_info({nodedown, Node, _Info}, State) ->
  edts_log:info("Node down: ~p", [Node]),
  case node_find(Node, State) of
    false   -> {noreply, State};
    #node{} -> {noreply, node_delete(Node, State)}
  end;
handle_info(_Info, State) ->
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

node_delete(Name, State) ->
  State#state{nodes = lists:keydelete(Name, #node.name, State#state.nodes)}.

node_find(Name, State) ->
  lists:keyfind(Name, #node.name, State#state.nodes).

node_store(Node, State) ->
  Nodes = lists:keystore(Node#node.name, #node.name, State#state.nodes, Node),
  State#state{nodes = Nodes}.


%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
