%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
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
-module(edts).

%%%_* Exports ==================================================================

%% API
-export([call/3,
         call/4,
         call/5,
         init_node/7,
         is_node/1,
         node_reachable/1,
         nodes/0]).

%%%_* Includes =================================================================
-include_lib("eunit/include/eunit.hrl").

%%%_* Defines ==================================================================

%%%_* Types ====================================================================

%%%_* API ======================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Initializes a new edts node.
%% @end
%%
-spec init_node(ProjectName    :: string(),
                Node           :: node(),
                ProjectRoot    :: filename:filename(),
                LibDirs        :: [filename:filename()],
                AppIncludeDirs :: [filename:filename()],
                SysIncludeDirs :: [filename:filename()],
                ErlangCookie   :: string()) -> ok.
%%------------------------------------------------------------------------------
init_node(ProjectName,
          Node,
          ProjectRoot,
          LibDirs,
          AppIncludeDirs,
          SysIncludeDirs,
          ErlangCookie) ->
  edts_server:init_node(ProjectName,
                        Node,
                        ProjectRoot,
                        LibDirs,
                        AppIncludeDirs,
                        SysIncludeDirs,
                        ErlangCookie).

%%------------------------------------------------------------------------------
%% @doc
%% Returns true iff Node is registered with this edts instance.
%% @end
%%
-spec is_node(Node::node()) -> boolean().
%%------------------------------------------------------------------------------
is_node(Node) ->
  edts_server:node_registered_p(Node).


%%------------------------------------------------------------------------------
%% @doc
%% Returns true if Node is registerend with the epmd on localhost.
%% @end
%%
-spec node_reachable(Node::node()) -> boolean().
%%------------------------------------------------------------------------------
node_reachable(Node) ->
  net_adm:ping(Node) =:= pong.

%%------------------------------------------------------------------------------
%% @doc
%% Returns a list of the edts_nodes currently registered with this
%% edts-instance.
%% @end
%%
-spec nodes() -> [node()].
%%------------------------------------------------------------------------------
nodes() ->
  edts_server:nodes().

%%%_* Internal functions =======================================================

call(Node, Mod, Fun) ->
  call(Node, Mod, Fun, []).

call(Node, Mod, Fun, Args) ->
  call(Node, Mod, Fun, Args, true).

call(Node, Mod, Fun, Args, LogError) ->
  try {ok, edts_dist:call(Node, Mod, Fun, Args)}
  catch
    error:Err ->
      case LogError of
        false -> ok;
        true  ->
          edts_log:error("Error in remote call ~p:~p/~p on ~p: ~p",
                         [Mod, Fun, length(Args), Node, Err])
      end,
      {error, Err}
  end.

%%%_* Tests ====================================================================

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
