%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc The edts otp-application entry-point.
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
-module(edts_app).
-behaviour(application).

%%%_* Exports ==================================================================

%% API
-export([start/0]).

%% Application callbacks
-export([ start/2
        , stop/1]).

%%%_* Includes =================================================================

%%%_* Defines ==================================================================

%%%_* Types ====================================================================

%%%_* API ======================================================================

%% Start the whole shebang.
start() ->
  %% Webmachine requirements
  ok = ensure_application_started(inets),
  ok = ensure_application_started(crypto),

  %% Lager requirements
  ok = ensure_application_started(compiler),
  ok = ensure_application_started(syntax_tools),
  ok = ensure_application_started(goldrush),
  ok = ensure_application_started(lager),

  ok = ensure_application_started(edts).


%% Application callbacks
start(_StartType, _Start) ->
  edts_sup:start_link().

stop(_State) ->
  ok.

%% Make sure the application is started.  This function will succeed
%% if the application is already started or was successfully started,
%% something that comes in handy when we're running an erlang from a
%% reltools-built release which has already started apps like inets.
ensure_application_started(AppName) ->
  %% In newer Erlang/OTP versions there are functions which would do
  %% this for us, until older versions are dropped from edts we have
  %% to roll our own.
  %%
  %% * application:ensure_started:     first appearance in R16B01
  %% * application:ensure_all_started: first appearance in R16B02
  case application:start(AppName) of
    ok ->
      ok;
    {error, {already_started, AppName}} ->
      ok;
    Other ->
      Other
  end.

%%%_* Internal functions =======================================================

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

