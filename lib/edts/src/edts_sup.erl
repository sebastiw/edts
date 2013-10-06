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

-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-export([dispatch/0]).

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

  WebmConf = [{port,     ?EDTS_PORT},
              {dispatch, dispatch()}],
  WemachineRouter = child_spec(webmachine_router),
  Webmachine      = {webmachine_mochiweb,
                     {webmachine_mochiweb, start, [WebmConf]},
                     permanent, 5000, worker, [webmachine_mochiweb]},
  Edts            = child_spec(edts_server),


  Formatters0     = lists:flatmap(fun edts_plugins:event_formatters/1,
                                  edts_plugins:names()),
  Formatters      = [{{edts, node_down},
                      edts_events_node_down} | Formatters0],
  EdtsEvent       = child_spec(edts_event, [Formatters]),

  PluginServices  = lists:flatmap(fun edts_plugins:edts_server_services/1,
                                  edts_plugins:names()),
  PluginSpecs     = [child_spec(Plugin) || Plugin <- PluginServices],

  Children = [EdtsEvent, Edts, WemachineRouter, Webmachine] ++ PluginSpecs,
  {ok, { {one_for_one, 5, 10}, Children} }.


%%%_* Internal functions =======================================================

child_spec(Name) ->
  child_spec(Name, []).

child_spec(Name, Args) ->
  {Name,
   {Name, start_link, Args},
   permanent, 5000, worker, [Name]}.

dispatch() ->
  DispatchFile       = filename:join(code:priv_dir(edts), "dispatch.conf"),
  {ok, EDTSDispatch} = file:consult(DispatchFile),
  EDTSDispatch.

%%%_* Unit tests ===============================================================

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
