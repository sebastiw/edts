%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc edts code to deal with distribution.
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
-module(edts_dist).

%%%_* Exports ==================================================================

%% API
-export([add_paths/2,
         call/3,
         call/4,
         connect/1,
         connect_all/0,
         start_service/2,
         load_all/1,
         make_sname/1,
         make_sname/2,
         refresh_service/2,
         remote_load_modules/2,
         set_app_envs/3]).

-compile({no_auto_import,[load_module/2]}).

%%%_* Includes =================================================================

%%%_* Defines ==================================================================

%%%_* Types ====================================================================

%%%_* API ======================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Adds LibDirs to the code-path on Node
%% @end
-spec add_paths(Node::node(), LibDirs::[file:filename()]) ->
              ok | {badrpc, term()}.
%%------------------------------------------------------------------------------
add_paths(Node, LibDirs) ->
  ok = call(Node, edts_code, add_paths, [LibDirs]).

%%------------------------------------------------------------------------------
%% @doc
%% Calls call/4 with Args = [].
%% @end
-spec call(Node::node(), Mod::atom(), Fun::atom()) ->
              term() | {badrpc, term()}.
%%------------------------------------------------------------------------------
call(Node, Mod, Fun) ->
  rpc:call(Node, Mod, Fun, []).


%%------------------------------------------------------------------------------
%% @doc
%% Calls Mod:Fun with Args remotely on Node.
%% @end
-spec call(Node::node(), Mod::atom(), Fun::atom(), Args::[term()]) ->
              term() | {badrpc, term()}.
%%------------------------------------------------------------------------------
call(Node, Mod, Fun, Args) ->
  rpc:call(Node, Mod, Fun, Args).

%%------------------------------------------------------------------------------
%% @doc
%% Pings Node registered with the local epmd, so that a connection is
%% established.
%% @end
-spec connect(Node::node()) -> ok.
%%------------------------------------------------------------------------------
connect(Node) ->
  pong = net_adm:ping(Node),
  ok.

%%------------------------------------------------------------------------------
%% @doc
%% Calls connect/1 for all nodes registered with the local epmd.
%% @end
-spec connect_all() -> ok.
%%------------------------------------------------------------------------------
connect_all() ->
  {ok, Hostname} = inet:gethostname(),
  {ok, Nodes}    = net_adm:names(),
  lists:foreach(fun({Name, _Port}) ->
                    {ok, Nodename} = make_sname(Name, Hostname),
                    connect(Nodename)
                end,
                Nodes).


%%------------------------------------------------------------------------------
%% @doc
%% Load all modules on Node that are in its code-path.
%% @end
-spec load_all(node()) -> {ok, [module()]}.
%%------------------------------------------------------------------------------
load_all(Node) ->
  call(Node, edts_code, load_all).


%%------------------------------------------------------------------------------
%% @doc
%% Converts a string to a valid erlang sname for localhost.
%% @end
-spec make_sname(string()) -> node().
%%------------------------------------------------------------------------------
make_sname(Name) ->
  {ok, Hostname} = inet:gethostname(),
  make_sname(Name, Hostname).

%%------------------------------------------------------------------------------
%% @doc
%% Converts a string to a valid erlang sname for Hostname.
%% @end
-spec make_sname(Name::string(), Hostname::string()) -> node().
%%------------------------------------------------------------------------------
make_sname(Name, Hostname) ->
  try
    {ok, list_to_atom(Name ++ "@" ++ Hostname)}
  catch
    _C:Reason -> {error, Reason}
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Refreshes the state of Service on Node.
%% @end
-spec refresh_service(node(), module()) -> ok | {badrpc, Reason::term()}.
%%------------------------------------------------------------------------------
refresh_service(Node, Service) ->
  call(Node, Service, refresh, []).


%%------------------------------------------------------------------------------
%% @doc
%% Loads Mods on Node.
%% @end
-spec remote_load_modules(Node::node(), Mods::[module()]) -> ok.
%%------------------------------------------------------------------------------
remote_load_modules(Node, Mods) ->
  lists:foreach(fun(Mod) -> remote_load_module(Node, Mod) end, Mods).

remote_load_module(Node, Mod) ->
  %% Compile code on the remote in case it runs an OTP release that is
  %% incompatible with the binary format of the EDTS node's OTP release.
  %% Kind of ugly to have to use two rpc's but I can't find a better way to
  %% do this.
  {File, _Opts}  = filename:find_src(Mod),
  {ok, Mod, Bin} = remote_compile_module(Node, File),
  {module, Mod}  = remote_load_module(Node, Mod, File, Bin).


%%------------------------------------------------------------------------------
%% @doc
%% Sets each of the Key/Value pairs in KVs in App's application environment on
%% Node.
%% @end
-spec set_app_envs(Node ::node(),
                   App  ::atom(),
                   KVs  :: [{Key::atom(), Value::term()}]) ->
                      [ok | {badrpc, Reason::term()}].
%%------------------------------------------------------------------------------
set_app_envs(Node, App, KVs) ->
  lists:map(fun({Key, Value}) -> set_app_env(Node, App, Key, Value) end, KVs).


%%------------------------------------------------------------------------------
%% @doc
%% Sets the environment variable Key for App on Node to Value.
%% @end
-spec set_app_env(Node :: node(),
                  App::atom(),
                  Key::atom(),
                  Value::term()) ->
                     ok | {badrpc, Reason::term()}.
%%------------------------------------------------------------------------------
set_app_env(Node, App, Key, Value) ->
  rpc:call(Node, application, set_env, [App, Key, Value]).

%%------------------------------------------------------------------------------
%% @doc
%% Starts Service on Node.
%% @end
-spec start_service(node(), module()) -> ok | {badrpc, Reason::term()}.
%%------------------------------------------------------------------------------
start_service(Node, Service) ->
  call(Node, Service, start).


%%%_* Internal functions =======================================================
remote_load_module(Node, Mod, File, Bin) ->
  case call(Node, code, load_binary, [Mod, File, Bin]) of
    {module, Mod} = Res -> Res;
    {error, Rsn}        -> erlang:error(Rsn)
  end.


remote_compile_module(Node, File) ->
  case call(Node, compile, file, [File, [debug_info, binary, report]]) of
    {ok, _, _} = Res -> Res;
    {error, Rsn}     -> erlang:error(Rsn)
  end.


%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
