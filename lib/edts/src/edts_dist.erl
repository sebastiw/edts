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
-export([ call/3
        , call/4
        , connect/1
        , connect_all/0
        , init_node/1
        , make_sname/1
        , make_sname/2]).

%%%_* Includes =================================================================

%%%_* Defines ==================================================================

%%%_* Types ====================================================================

%%%_* API ======================================================================


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
%% Initialize edts-related services etc. on remote node. Returns a list of keys
%% to promises that are to be fulfilled by the remote node. These keys can later
%% be used in calls to rpc:yield/1, rpc:nbyield/1 and rpc:nbyield/2.
%% @end
-spec init_node(string()) -> [rpc:key()].
%%------------------------------------------------------------------------------
init_node(Node) ->
  remote_load_modules(Node,   [edts_code]),
  remote_start_services(Node, [edts_code]).

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


%%%_* Internal functions =======================================================

remote_load_modules(Node, Mods) ->
  lists:foreach(fun(Mod) -> remote_load_module(Node, Mod) end, Mods).

remote_load_module(Node, Mod0) ->
  {Mod, Bin, File} = code:get_object_code(Mod0),
  {module, Mod0}   = call(Node, code, load_binary, [Mod, File, Bin]).

remote_start_services(Node, Servs) ->
  lists:map(fun(Service) -> remote_start_service(Node, Service) end, Servs).

remote_start_service(Node, Service) ->
  rpc:async_call(Node, Service, start, []).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
