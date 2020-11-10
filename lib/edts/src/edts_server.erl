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
-export([init_node/7,
         node_registered_p/1,
         nodes/0,
         start_link/0,
         start_service/2]).

%% gen_server callbacks
-export([ code_change/3
        , init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2]).

%%%_* Includes =================================================================

-include_lib("kernel/include/logger.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%_* Defines ==================================================================
-define(SERVER, ?MODULE).
-record(node, {name :: node()}).

-record(state, {nodes         = [] :: [edts_node()]}).

%%%_* Types ====================================================================
-type edts_node() :: #node{}.
-type state()     :: #state{}.

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
%% Initializes a new edts node.
%% @end
%%
-spec init_node(ProjectName  :: string(),
                Node         :: node(),
                ProjectRoot  :: filename:filename(),
                LibDirs      :: [filename:filename()],
                AppInclDirs  :: [filename:filename()],
                SysInclDirs  :: [filename:filename()],
                ErlangCookie :: string()) -> ok.
%%------------------------------------------------------------------------------
init_node(ProjectName,
          Node,
          ProjectRoot,
          LibDirs,
          AppInclDirs,
          SysInclDirs,
          ErlangCookie) ->
  Call = {init_node,
          ProjectName,
          Node,
          ProjectRoot,
          LibDirs,
          AppInclDirs,
          SysInclDirs,
          ErlangCookie},
  gen_server:call(?SERVER, Call, infinity).


%%------------------------------------------------------------------------------
%% @doc
%% Returns true iff Node is registered with this edts instance, but has possibly
%% not finished its initialization.
%% @end
%%
-spec node_registered_p(node()) -> boolean().
%%------------------------------------------------------------------------------
node_registered_p(Node) ->
  gen_server:call(?SERVER, {node_registered_p, Node}, infinity).


%%------------------------------------------------------------------------------
%% @doc
%% Returns a list of the edts_nodes currently registered with this
%% edts-server instance
%% @end
%%
-spec nodes() -> {ok, [node()]}.
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
  {ok, #state{nodes = [#node{name = node()}]}}.

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
handle_call({init_node,
             ProjectName,
             NodeName,
             ProjectRoot,
             LibDirs,
             AppInclDirs,
             SysInclDirs,
             ErlangCookie},
            _From,
            State0) ->
  ?LOG_INFO("Initializing ~p.", [NodeName]),
  case do_init_node(ProjectName,
                    NodeName,
                    ProjectRoot,
                    LibDirs,
                    AppInclDirs,
                    SysInclDirs,
                    ErlangCookie) of
    ok ->
      ?LOG_DEBUG("Initialization call done on ~p.", [NodeName]),
      State =
        case node_find(NodeName, State0) of
          #node{} -> State0;
          false   -> node_store(#node{name = NodeName}, State0)
        end,
      {reply, ok, State};
    {error, _} = Err ->
      ?LOG_ERROR("Initializing node ~p failed with ~p.", [NodeName, Err]),
      {reply, Err, State0}
  end;
handle_call({node_registered_p, NodeName}, _From, State) ->
  Reply = case node_find(NodeName, State) of
            #node{} -> true;
            false   -> false
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
handle_info({nodedown, Node, _Info}, State0) ->
  ?LOG_INFO("Node down: ~p", [Node]),
  edts_event:dispatch(edts, node_down, [{node, Node}]),
  State = case node_find(Node, State0) of
            false   -> State0;
            #node{} -> node_delete(Node, State0)
          end,
  {noreply, State};
handle_info(Info, State) ->
  ?LOG_DEBUG("Unhandled message: ~p", [Info]),
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

%%------------------------------------------------------------------------------
%% @doc
%% Initialize edts-related services etc. on remote node. Returns a list of keys
%% to promises that are to be fulfilled by the remote node. These keys can later
%% be used in calls to rpc:yield/1, rpc:nbyield/1 and rpc:nbyield/2.
%% @end
-spec do_init_node(ProjectName    :: string(),
                   Node           :: node(),
                   ProjectRoot    :: filename:filename(),
                   LibDirs        :: [filename:filename()],
                   AppIncludeDirs :: [filename:filename()],
                   SysIncludeDirs :: [filename:filename()],
                   ErlangCookie   :: string()) ->
        ok | {error, term()}.
%%------------------------------------------------------------------------------
do_init_node(ProjectName,
             Node,
             ProjectRoot,
             LibDirs,
             AppIncludeDirs,
             ProjectIncludeDirs,
             ErlangCookie) ->
  try
    case ErlangCookie of
      undefined -> ok;
      _         -> ok = edts_dist:set_cookie(Node, list_to_atom(ErlangCookie))
    end,
    Plugins = edts_plugins:names(),
    edts_plugins:specs(),
    ok = lists:foreach(fun(Spec) -> edts_dist:load_app(Node, Spec) end,
                       edts_plugins:specs()),
    PluginRemoteLoad =
      lists:flatmap(fun(Plugin) -> Plugin:project_node_modules() end, Plugins),
    PluginRemoteServices =
      lists:flatmap(fun(Plugin) -> Plugin:project_node_services() end, Plugins),

    ModulesToLoad = [edts_code,
                     edts_eunit,
                     edts_eunit_listener,
                     edts_event,
                     edts_module_server,
                     edts_plugins,
                     edts_util] ++ PluginRemoteLoad,
    ?LOG_DEBUG("Loading modules on ~p: ~p", [Node, ModulesToLoad]),
    ok = edts_dist:remote_load_modules(Node, ModulesToLoad),

    {ok, ProjectDir} = application:get_env(edts, project_data_dir),
    AppEnv = [{server_node,          node()},
              {project_lib_dirs,     LibDirs},
              {project_name,         ProjectName},
              {project_data_dir,     ProjectDir},
              {project_root_dir,     ProjectRoot},
              {app_include_dirs,     AppIncludeDirs},
              {project_include_dirs, ProjectIncludeDirs}],

    ?LOG_DEBUG("Initalizing ~p with environment: ~p", [Node, AppEnv]),
    edts_dist:init_node(Node, AppEnv),

    start_services(Node, [edts_code] ++ PluginRemoteServices)
  catch
    C:E:S ->
      ?LOG_ERROR("~p initialization crashed with ~p:~p~nStacktrace:~n~p",
                 [Node, C, E, S]),
      {error, E}
  end.

start_services(_Node, []) -> ok;
start_services(Node, [Service|Rest]) ->
  case start_service(Node, Service) of
    ok           -> start_services(Node, Rest);
    {error, Err} -> {error, {Service, Err}}
  end.

start_service(Node, Service) ->
  ?LOG_INFO("Starting service ~p on ~p", [Service, Node]),
  case edts_dist:start_service(Node, Service) of
    ok ->
      ?LOG_INFO("Service ~p started on ~p", [Service, Node]),
      ok;
    {error, _}  = Err ->
      ?LOG_ERROR("Starting service ~p on ~p failed with ~p",
                 [Service, Node, Err]),
      Err
  end.

node_delete(Name, State) ->
  State#state{nodes = lists:keydelete(Name, #node.name, State#state.nodes)}.

node_find(Name, State) ->
  lists:keyfind(Name, #node.name, State#state.nodes).

node_store(Node, State) ->
  Nodes = lists:keystore(Node#node.name, #node.name, State#state.nodes, Node),
  State#state{nodes = Nodes}.

%%%_* Unit tests ===============================================================

-ifdef(TEST).

init_node_test() ->
  N1 = #node{name = foo},
  S1 = #state{},
  S2 = #state{nodes = [N1]},

  PrevEnv = application:get_env(edts, project_data_dir),
  application:set_env(edts, project_data_dir, "foo_dir"),

  meck:new(edts_dist),
  meck:expect(edts_dist, add_paths,               fun(foo, _) -> ok;
                                                     (bar, _) -> ok
                                                  end),
  meck:expect(edts_dist, load_app,                fun(_, _) -> ok
                                                  end),
  meck:expect(edts_dist, init_node,               fun(_, _) -> ok
                                                  end),
  meck:expect(edts_dist, remote_load_modules,     fun(foo, _) -> ok;
                                                     (bar, _) -> ok
                                                  end),
  meck:expect(edts_dist, set_cookie,             fun(foo, cookie) -> ok
                                                 end),
  meck:expect(edts_dist, start_service,
              fun(foo, _Srv) -> ok;
                 (bar, _Srv) -> {error, already_started}
              end),
  meck:expect(edts_dist, refresh_service, fun(bar, _Srv) -> ok end),

  Call0 = {init_node, project_name, N1#node.name, "", [], [], [], "cookie"},
  ?assertEqual({reply, ok, S1#state{nodes = [N1]}},
               handle_call(Call0, {self(), cookie}, S1)),
  Call1 = {init_node, project_name, N1#node.name, "", [], [], [], "cookie"},
  ?assertEqual({reply, ok, S2#state{nodes = [N1]}},
               handle_call(Call1, {self(), cookie}, S2)),

  meck:expect(edts_dist, init_node,
              fun(foo, _) -> error(some_error) end),
  Call2 = {init_node, project_name, N1#node.name, "", [], [], [], undefined},
  ?assertEqual({reply, {error, some_error}, S1},
               handle_call(Call2, {self(), undefined}, S1)),

  case PrevEnv of
    undefined -> ok;
    {ok, Env} -> application:set_env(edts, project_data_dir, Env)
  end,
  meck:unload().

node_registered_p_test() ->
  N1 = #node{name = foo},
  N2 = #node{name = bar},
  S1 = #state{nodes = [N1]},
  ?assertEqual({reply, true, S1},
               handle_call({node_registered_p, N1#node.name}, {self(), foo}, S1)),
  ?assertEqual({reply, false, S1},
               handle_call({node_registered_p, N2#node.name}, {self(), bar}, S1)).

nodes_test() ->
  N1 = #node{name = foo},
  N2 = #node{name = bar},
  S1 = #state{nodes = [N1]},
  S2 = #state{nodes = [N2]},
  ?assertEqual({reply, {ok, [N1#node.name]}, S1},
               handle_call(nodes, {self(), foo}, S1)),
  ?assertEqual({reply, {ok, [N2#node.name]}, S2},
               handle_call(nodes, {self(), bar}, S2)).

ignored_call_test() ->
  S1 = #state{},
  ?assertEqual({reply, ignored, S1}, handle_call(foo, {self(), a}, S1)),
  ?assertEqual({reply, ignored, S1}, handle_call(bar, {self(), b}, S1)).

handle_cast_test() ->
  Bar = #state{nodes = [#node{name = bar}]},
  Foo = #state{nodes = [#node{name = foo}]},
  ?assertEqual({noreply, Bar}, handle_cast(foo, Bar)),
  ?assertEqual({noreply, Foo}, handle_cast(bar, Foo)).

nodedown_test() ->
  N1 = #node{name = foo},
  N2 = #node{name = bar},
  S1 = #state{nodes = [N1]},
  ?assertEqual({noreply, S1},
               handle_info({nodedown, N2#node.name, dummy}, S1)),
  ?assertEqual({noreply, #state{}},
               handle_info({nodedown, N1#node.name, dummy}, S1)).


unhandled_message_test() ->
  Bar = #state{nodes = [#node{name = bar}]},
  Foo = #state{nodes = [#node{name = foo}]},
  ?assertEqual({noreply, Bar}, handle_info(foo, Bar)),
  ?assertEqual({noreply, Foo}, handle_info(bar, Foo)).

terminate_test() ->
  ?assertEqual(ok, terminate(foo, #state{nodes = [#node{name = bar}]})),
  ?assertEqual(ok, terminate(bar, #state{nodes = [#node{name = foo}]})).

code_change_test() ->
  Foo = #state{nodes = [#node{name = foo}]},
  Extra = #state{nodes = [#node{name = extra}]},
  ?assertEqual({ok, Foo}, code_change("vsn", Foo, extra)),
  ?assertEqual({ok, Extra}, code_change("vsn", Extra, foo)).

node_find_test() ->
  N = #node{name = foo},
  N2 = #node{name = bar},
  ?assertEqual(false, node_find(foo, #state{})),
  ?assertEqual(N, node_find(foo, #state{nodes = [N]})),
  ?assertEqual(false, node_find(foo, #state{nodes = [N2]})).

node_delete_test() ->
  N = #node{name = foo},
  N2 = #node{name = bar},
  ?assertEqual(#state{}, node_delete(foo, #state{})),
  ?assertEqual(#state{}, node_delete(foo, #state{nodes = [N]})),
  ?assertEqual(#state{nodes = [N2]}, node_delete(foo, #state{nodes = [N2]})).

node_store_test() ->
  N = #node{name = foo},
  N2 = #node{name = bar},
  ?assertEqual(#state{nodes = [N]}, node_store(N, #state{})),
  ?assertEqual(#state{nodes = [N]}, node_store(N, #state{nodes = [N]})),
  ?assertEqual(#state{nodes = [N2, N]}, node_store(N, #state{nodes = [N2]})).

-endif.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
