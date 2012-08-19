%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc xref-library for edts.
%%% @end
%%% @author Thomas Järvstrand <tjarvstrand@gmail.com>
%%% @copyright
%%% Copyright 2012 Thomas Järvstrand <tjarvstrand@gmail.com>
%%%
%%% This file is part of EDTS.
%%%
%%% EDTS is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% EDTS is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with EDTS. If not, see <http://www.gnu.org/licenses/>.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration =======================================================
-module(edts_xref).

%%%_* Exports ==================================================================

-export([ exported_functions/1
        , modules/0
        , start/0]).

%%%_* Includes =================================================================

%%%_* Defines ==================================================================
-define(SERVER, ?MODULE).
%%%_* Types ====================================================================

%%%_* API ======================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Returns a list of all functions exported from Module.
%% @end
-spec exported_functions(Module::module()) ->
                            {ok, [{atom(), non_neg_integer()}]}.
%%------------------------------------------------------------------------------
exported_functions(Module) ->
  case xref:q(?SERVER, atom_to_list(Module) ++ " : Mod * X") of
    {error, xref_compiler, {unknown_constant, _}} ->
      {ok, []};
    {ok, Exports} ->
      {ok, [{F, A} || {_M, F, A} <- Exports]}
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Returns a list of all modules known to the edts_xref server.
%% @end
-spec modules() -> [atom()].
%%------------------------------------------------------------------------------
modules() ->
  xref:q(?SERVER, '".*" : Mod').

%%------------------------------------------------------------------------------
%% @doc
%% Starts the edts xref-server on the local node.
%% @end
-spec start() -> ok.
%%------------------------------------------------------------------------------
start() ->
  case xref:start(?SERVER) of
    {ok, _Pid}                       -> init();
    {error, {already_started, _Pid}} -> update()
  end,
  load_cache().

%%%_* Internal functions =======================================================

init() ->
  ok = xref:set_default(?SERVER, [{verbose,false}, {warnings,false}]),
  Paths = code:get_path(),
  ok = xref:set_library_path(?SERVER, Paths),
  lists:foreach(fun(D) ->
                    case xref:add_application(?SERVER, filename:dirname(D)) of
                      {error, _, _} -> xref:add_directory(?SERVER, D);
                      {ok, _}       -> ok
                    end
                end,
                Paths).

update() ->
  {ok, _Modules} = xref:update(?SERVER),
  ok.

%% Query the server to cache values.
load_cache() ->
  modules().

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

