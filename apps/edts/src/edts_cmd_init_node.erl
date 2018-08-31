%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc init_nodes_command
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
-module(edts_cmd_init_node).

%%%_* Exports ==================================================================

%% API
-export([execute/1,
         spec/0]).

%%%_* Includes =================================================================
-include_lib("eunit/include/eunit.hrl").

%%%_* Defines ==================================================================
%%%_* Types ====================================================================
%%%_* API ======================================================================

spec() ->
  [project_name,
   nodename,
   project_root,
   project_lib_dirs,
   app_include_dirs,
   project_include_dirs,
   erlang_cookie].

execute(Ctx) ->
  edts:init_node(orddict:fetch(project_name,         Ctx),
                 orddict:fetch(nodename,             Ctx),
                 orddict:fetch(project_root,         Ctx),
                 orddict:fetch(project_lib_dirs,     Ctx),
                 orddict:fetch(app_include_dirs,     Ctx),
                 orddict:fetch(project_include_dirs, Ctx),
                 orddict:fetch(erlang_cookie,        Ctx)).



%%%_* Internal functions =======================================================
%%%_* Unit tests ===============================================================


%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
