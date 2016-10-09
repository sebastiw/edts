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
         init_node/2,
         load_all/1,
         load_app/2,
         make_sname/1,
         make_sname/2,
         refresh_service/2,
         remote_load_modules/2,
         set_cookie/2,
         start_service/2]).

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
%% @equiv call(Node, Mod, Fun, []).
%% @end
-spec call(Node::node(), Mod::atom(), Fun::atom()) ->
              term() | {badrpc, term()}.
%%------------------------------------------------------------------------------
call(Node, Mod, Fun) ->
  rpc:call(Node, Mod, Fun, []).


%%------------------------------------------------------------------------------
%% @doc
%% Calls Mod:Fun with Args remotely on Node
%% @end
-spec call(Node::node(), Mod::atom(), Fun::atom(), Args::[term()]) ->
              term() | {badrpc, term()}.
%%------------------------------------------------------------------------------
call(Node, Mod, Fun, Args) ->
  Self = self(),
  Pid = spawn(fun() -> do_call(Self, Node, Mod, Fun, Args) end),
  receive
    {Pid, {badrpc, {'EXIT', Rsn}}}      -> error(Rsn);
    {Pid, {badrpc, Err}}                -> error(Err);
    {Pid, Res}                          -> Res
  end.


do_call(Parent, Node, Mod, Fun, Args) ->
  Res =
    try
      try_set_remote_group_leader(Node),
      rpc:call(Node, Mod, Fun, Args)
    catch
      _:Err -> {badrpc, Err}
    end,
  Parent ! {self(), Res}.

%% @doc Set the group leader to get all tty output on the remote nade.
try_set_remote_group_leader(Node) ->
  case rpc:call(Node, erlang, whereis, [user]) of
    undefined            -> ok;
    Pid when is_pid(Pid) ->
      Info = rpc:call(Node, erlang, process_info, [Pid]),
      RemoteGroupLeader = proplists:get_value(group_leader, Info),
      group_leader(RemoteGroupLeader, self());
    {badrpc, Err} -> error(Err)
  end.


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
%% Initialize procect node Node with AppEnvs.
%% @end
-spec init_node(node(), [{Key::atom(), Value::term()}]) -> ok.
%%------------------------------------------------------------------------------
init_node(Node, AppEnvs) ->
  call(Node, edts_code, init, [AppEnvs]).


%%------------------------------------------------------------------------------
%% @doc
%% Load application spec AppSpec on Node.
%% @end
-spec load_app(node(), term()) -> ok | {error, term()}.
%%------------------------------------------------------------------------------
load_app(Node, AppSpec) ->
  call(Node, application, load, [AppSpec]).


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
remote_load_modules(Node, _Mods) when Node =:= node() -> ok;
remote_load_modules(Node, Mods)                       ->
  lists:foreach(fun(Mod) -> remote_load_module(Node, Mod) end, Mods).


remote_load_module(Node, Mod) ->
  %% Compile code on the remote in case it runs an OTP release that is
  %% incompatible with the binary format of the EDTS node's OTP release.
  %% Kind of ugly to have to use two rpc's but I can't find a better way to
  %% do this.
  case filename:find_src(Mod) of
    {error, Err}  -> erlang:error({Mod, Err});
    {File, _Opts} ->
      {ok, Mod, Bin} = remote_compile_module(Node, File),
      {module, Mod}  = remote_load_module(Node, Mod, Bin)
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Adds LibDirs to the code-path on Node
%% @end
-spec set_cookie(Node :: node(), Cookie :: atom()) ->
              ok | {error, term()}.
%%------------------------------------------------------------------------------
set_cookie(Node, Cookie) ->
  case net_kernel:connect_node(Node) of
    true  -> call(Node, erlang, set_cookie, [node(), Cookie]);
    false -> ok
  end,
  erlang:set_cookie(Node, Cookie),
  case net_kernel:connect_node(Node) of
    true  -> ok;
    false -> {error, {failed_to_connect, Node}}
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Starts Service on Node.
%% @end
-spec start_service(node(), module()) -> ok | {badrpc, Reason::term()}.
%%------------------------------------------------------------------------------
start_service(Node, Service) ->
  call(Node, Service, start).


%%%_* Internal functions =======================================================
remote_load_module(Node, Mod, Bin) ->
  %% Haha, I feel evil now!
  case call(Node, code, load_binary, [Mod, preloaded, Bin]) of
    {module, Mod} = Res -> Res;
    {error, Rsn}        -> erlang:error(Rsn)
  end.


remote_compile_module(Node, File) ->
  Opts = [{d, namespaced_types}, debug_info, binary, return_errors],
  case call(Node, compile, file, [File, Opts]) of
    {ok, _, _} = Res      -> Res;
    {error, Rsns, _Warns} ->
      case imported_predefined_type_p(Rsns) of
        true ->
          case call(Node, compile, file, [File, tl(Opts)]) of
            {ok, _, _} = Res      -> Res;
            {error, Rsns, _Warns} ->
              erlang:error({compile_error, {File, Rsns}})
          end;
        false ->
          erlang:error({compile_error, {File, Rsns}})
      end
  end.

imported_predefined_type_p(Errors) ->
  do_imported_predefined_type_p(lists:concat([E || {_F, E} <- Errors])).

do_imported_predefined_type_p([]) -> false;
do_imported_predefined_type_p([Error|Errors]) ->
  case Error of
    {_, erl_lint, {imported_predefined_type, _}} -> true;
    _ -> imported_predefined_type_p(Errors)
  end.


%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
