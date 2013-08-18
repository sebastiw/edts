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
-export([ wait_for_node/1
        , init_node/6
        , node_available_p/1
        , node_registered_p/1
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
-include_lib("eunit/include/eunit.hrl").

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
-spec wait_for_node(node()) -> ok.
%%------------------------------------------------------------------------------
wait_for_node(Node) ->
  gen_server:call(?SERVER, {wait_for_node, Node}, infinity).


%%------------------------------------------------------------------------------
%% @doc
%% Initializes a new edts node.
%% @end
%%
-spec init_node(ProjectName    :: string(),
                Node        :: node(),
                ProjectRoot :: filename:filename(),
                LibDirs     :: [filename:filename()],
                AppInclDirs :: [filename:filename()],
                SysInclDirs :: [filename:filename()]) -> ok.
%%------------------------------------------------------------------------------
init_node(ProjectName, Node, ProjectRoot, LibDirs, AppInclDirs, SysInclDirs) ->
  Call = {init_node,
          ProjectName,
          Node,
          ProjectRoot,
          LibDirs,
          AppInclDirs,
          SysInclDirs},
  gen_server:call(?SERVER, Call, infinity).

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
handle_call({wait_for_node, NodeName}, _From, State) ->
    case node_find(NodeName, State) of
      false -> {reply, {error, not_found}, State};
      #node{promises = [_|_]} = Node->
        lists:foreach(fun rpc:yield/1, Node#node.promises),
        {reply, ok, node_store(Node#node{promises = []}, State)};
      #node{} ->
        {reply, ok, State}
    end;
handle_call({init_node,
             ProjectName,
             NodeName,
             ProjectRoot,
             LibDirs,
             AppInclDirs,
             SysInclDirs},
            _From,
            State0) ->
  edts_log:info("Initializing ~p.", [NodeName]),
  case do_init_node(ProjectName,
                    NodeName,
                    ProjectRoot,
                    LibDirs,
                    AppInclDirs,
                    SysInclDirs) of
    ok ->
      edts_log:debug("Initialization call done on ~p.", [NodeName]),
      State =
        case node_find(NodeName, State0) of
          #node{} -> State0;
          false   -> node_store(#node{name = NodeName}, State0)
        end,
      {reply, ok, State};
    {error, _} = Err ->
      edts_log:error("Initializing node ~p failed with ~p.", [NodeName, Err]),
      {reply, Err, State0}
  end;
handle_call({node_available_p, NodeName}, _From, State) ->
  Reply = case node_find(NodeName, State) of
            #node{promises = []} -> true;
            #node{}              -> false;
            false                -> false
          end,
  {reply, Reply, State};
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
handle_info({nodedown, Node, _Info}, State) ->
  edts_log:info("Node down: ~p", [Node]),
  case node_find(Node, State) of
    false   -> {noreply, State};
    #node{} -> {noreply, node_delete(Node, State)}
  end;
handle_info(Info, State) ->
  edts_log:debug("Unhandled message: ~p", [Info]),
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
                   SysIncludeDirs :: [filename:filename()]) -> [rpc:key()].
%%------------------------------------------------------------------------------
do_init_node(ProjectName,
             Node,
             ProjectRoot,
             LibDirs,
             AppIncludeDirs,
             ProjectIncludeDirs) ->
  try
    Plugins = plugins(),
    PluginRemoteLoad =
      lists:flatmap(fun(Plugin) -> Plugin:project_node_modules() end, Plugins),
    PluginRemoteServices =
      lists:flatmap(fun(Plugin) -> Plugin:project_node_services() end, Plugins),
    ok = edts_dist:remote_load_modules(Node,
                                       [edts_code,
                                        edts_dialyzer,
                                        edts_eunit,
                                        edts_eunit_listener,
                                        edts_module_server,
                                        edts_xref,
                                        edts_util] ++
                                         PluginRemoteLoad),
    ok = edts_dist:add_paths(Node, expand_code_paths(ProjectRoot, LibDirs)),
    {ok, ProjectDir} = application:get_env(edts, project_data_dir),
    AppEnv = [{project_name,         ProjectName},
              {project_data_dir,     ProjectDir},
              {project_root_dir,     ProjectRoot},
              {app_include_dirs,     AppIncludeDirs},
              {project_include_dirs, ProjectIncludeDirs}],
    init_node_env(Node, AppEnv),
    start_services(Node, [edts_code] ++ PluginRemoteServices)
  catch
    C:E ->
      edts_log:error("~p initialization crashed with ~p:~p~nStacktrace:~n~p",
                     [Node, C, E, erlang:get_stacktrace()]),
      E
  end.

plugins() ->
  case application:get_env(edts, plugin_dir) of
    undefined -> [];
    {ok, Dir} ->
      {ok, PluginDirs} = file:list_dir(Dir),
      [list_to_atom(PluginDir) || PluginDir <- PluginDirs,
                                  filelib:is_dir(
                                    filename:absname_join(Dir, PluginDir))]
  end.

init_node_env(Node, AppEnv) ->
  [] = [R || R <- edts_dist:set_app_envs(Node, edts, AppEnv), R =/= ok].

start_services(_Node, []) -> ok;
start_services(Node, [Service|Rest]) ->
  case start_service(Node, Service) of
    ok               -> start_services(Node, Rest);
    {error, _} = Err -> Err
  end.

start_service(Node, Service) ->
  edts_log:info("Starting service ~p on ~p", [Service, Node]),
  case edts_dist:start_service(Node, Service) of
    ok ->
      edts_log:info("Service ~p started on ~p", [Service, Node]),
      ok;
    {error, _}  = Err ->
      edts_log:error("Starting service ~p on ~p failed with ~p",
                     [Service, Node, Err]),
      Err
  end.


expand_code_paths("", _LibDirs) -> [];
expand_code_paths(ProjectRoot, LibDirs) ->
  RootPaths = [filename:join(ProjectRoot, "ebin"),
               filename:join(ProjectRoot, "test")],
  F = fun(Dir) -> expand_code_path(ProjectRoot, Dir) end,
  RootPaths ++ lists:flatmap(F, LibDirs).

expand_code_path(Root, Dir) ->
  Fun = fun(F) -> [filename:join(F, "ebin"), filename:join(F, "test")] end,
  lists:flatmap(Fun, filelib:wildcard(filename:join([Root, Dir, "*"]))).

node_delete(Name, State) ->
  State#state{nodes = lists:keydelete(Name, #node.name, State#state.nodes)}.

node_find(Name, State) ->
  lists:keyfind(Name, #node.name, State#state.nodes).

node_store(Node, State) ->
  Nodes = lists:keystore(Node#node.name, #node.name, State#state.nodes, Node),
  State#state{nodes = Nodes}.

%%%_* Unit tests ===============================================================

wait_for_node_test() ->
  S1 = #state{},
  ?assertEqual({reply, {error, not_found}, S1},
               handle_call({wait_for_node, foo}, self(), S1)),
  meck:new(rpc, [unstick]),
  meck:expect(rpc, yield, fun(dummy) -> ok end),
  N2 = #node{name = foo, promises = [dummy]},
  S2 = #state{nodes = [N2]},
  ?assertEqual({reply, ok, S2#state{nodes = [N2#node{promises = []}]}},
               handle_call({wait_for_node, foo}, self(), S2)),
  N3 = #node{name = foo, promises = []},
  S3 = #state{nodes = [N3]},
  ?assertEqual({reply, ok, S3},
               handle_call({wait_for_node, foo}, self(), S3)),
  meck:unload().

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
  meck:expect(edts_dist, remote_load_modules,     fun(foo, _) -> ok;
                                                     (bar, _) -> ok
                                                  end),
  meck:expect(edts_dist, set_app_envs,             fun(foo, _, _) -> [ok];
                                                      (bar, _, _) -> [ok]
                                                  end),
  meck:expect(edts_dist, start_service,
              fun(foo, _Srv) -> ok;
                 (bar, _Srv) -> {error, already_started}
              end),
  meck:expect(edts_dist, refresh_service, fun(bar, _Srv) -> ok end),

  ?assertEqual(
     {reply, ok, S1#state{nodes = [N1]}},
     handle_call({init_node, project_name, N1#node.name, "", [], [], []}, self(), S1)),
  ?assertEqual(
     {reply, ok, S2#state{nodes = [N1]}},
     handle_call({init_node, project_name, N1#node.name, "", [], [], []}, self(), S2)),

  meck:expect(edts_dist, add_paths,
              fun(foo, _) -> error({error, some_error}) end),
  ?assertEqual({reply, {error, some_error}, S1},
               handle_call({init_node, project_name, N1#node.name, "", [], [], []}, self(), S1)),

  case PrevEnv of
    undefined -> ok;
    {ok, Env} -> application:set_env(edts, project_data_dir, Env)
  end,
  meck:unload().

node_registered_p_test() ->
  N1 = #node{name = foo, promises = [dummy]},
  N2 = #node{name = bar},
  S1 = #state{nodes = [N1]},
  ?assertEqual({reply, true, S1},
               handle_call({node_registered_p, N1#node.name}, self(), S1)),
  ?assertEqual({reply, false, S1},
               handle_call({node_registered_p, N2#node.name}, self(), S1)).

node_available_p_test() ->
  N1 = #node{name = foo, promises = [dummy]},
  N2 = #node{name = bar},
  S1 = #state{nodes = [N1]},
  S2 = #state{nodes = [N2]},
  ?assertEqual({reply, false, S1},
               handle_call({node_available_p, N1#node.name}, self(), S1)),
  ?assertEqual({reply, false, S2},
               handle_call({node_available_p, N1#node.name}, self(), S2)),
  ?assertEqual({reply, true, S2},
               handle_call({node_available_p, N2#node.name}, self(), S2)).

nodes_test() ->
  N1 = #node{name = foo, promises = [dummy]},
  N2 = #node{name = bar},
  S1 = #state{nodes = [N1]},
  S2 = #state{nodes = [N2]},
  ?assertEqual({reply, {ok, [N1#node.name]}, S1},
               handle_call(nodes, self(), S1)),
  ?assertEqual({reply, {ok, [N2#node.name]}, S2},
               handle_call(nodes, self(), S2)).

ignored_call_test() ->
  S1 = #state{},
  ?assertEqual(handle_call(foo, self(), S1), {reply, ignored, S1}),
  ?assertEqual(handle_call(bar, self(), S1), {reply, ignored, S1}).

handle_cast_test() ->
  ?assertEqual({noreply, bar}, handle_cast(foo, bar)),
  ?assertEqual({noreply, foo}, handle_cast(bar, foo)).

nodedown_test() ->
  N1 = #node{name = foo, promises = [dummy]},
  N2 = #node{name = bar},
  S1 = #state{nodes = [N1]},
  ?assertEqual({noreply, S1},
               handle_info({nodedown, N2#node.name, dummy}, S1)),
  ?assertEqual({noreply, #state{}},
               handle_info({nodedown, N1#node.name, dummy}, S1)).


unhandled_message_test() ->
  ?assertEqual({noreply, bar}, handle_info(foo, bar)),
  ?assertEqual({noreply, foo}, handle_info(bar, foo)).


terminate_test() ->
  ?assertEqual(ok, terminate(foo, bar)),
  ?assertEqual(ok, terminate(bar, foo)).

code_change_test() ->
  ?assertEqual({ok, foo}, code_change("vsn", foo, extra)),
  ?assertEqual({ok, extra}, code_change("vsn", extra, foo)).

expand_code_paths_test() ->
  ?assertEqual([], expand_code_paths("", ["/foo"])),
  ?assertEqual(["/foo/ebin", "/foo/test"], expand_code_paths("/foo", [])).

expand_code_path_test() ->
  meck:new(filelib, [passthrough, unstick]),
  meck:expect(filelib, wildcard,
              fun(Path) ->
                  Dirname = filename:dirname(Path),
                  [filename:join(Dirname, "foo"), filename:join(Dirname, "bar")]
              end),
  Root = "/foo",
  Lib  = "lib",
  ?assertEqual([filename:join([Root, "lib", "foo", "ebin"]),
                filename:join([Root, "lib", "foo", "test"]),
                filename:join([Root, "lib", "bar", "ebin"]),
                filename:join([Root, "lib", "bar", "test"])],
               expand_code_path(Root, Lib)),
  meck:unload().


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


%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
