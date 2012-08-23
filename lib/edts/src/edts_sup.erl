%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Top-level edts supervisor.
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
-module(edts_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(EDTS_PORT, 4587).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    DispatchFile   = filename:join(code:priv_dir(edts), "dispatch.conf"),
    {ok, Dispatch} = file:consult(DispatchFile),

    WebmConf = [
                {port, ?EDTS_PORT},
                {dispatch, Dispatch}
               ],
    Webmachine = {webmachine_mochiweb,
                  {webmachine_mochiweb, start, [WebmConf]},
                  permanent, 5000, worker, [webmachine_mochiweb]},
    Edts = {edts_server,
            {edts_server, start_link, []},
            permanent, 5000, worker, [edts_server]},
    Children = [Edts, Webmachine],
    {ok, { {one_for_one, 5, 10}, Children} }.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
