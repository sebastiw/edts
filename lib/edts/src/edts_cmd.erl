%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc node resource
%%% @end
%%% @author Thomas Järvstrand <tjarvstrand@gmail.com>
%%% @copyright
%%% Copyright 2017 Thomas Järvstrand <tjarvstrand@gmail.com>
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
-module(edts_cmd).

%%%_* Exports ==================================================================

%% API
-export([execute/2]).

-export_type([ctx/0]).

%%%_* Includes =================================================================

-include("logger.hrl").

%%%_* Defines ==================================================================
%%%_* Types ====================================================================

-type ctx() :: orddict:orddict(atom(), term()).

%%%_* Behaviour callbacks ======================================================

-callback spec() -> [atom()].

-callback execute(ctx()) -> ok | {ok, term()}.

%%%_* API ======================================================================

-spec execute(Cmd::module(), ctx()) ->
          ok |
          {ok, [{atom(), term()}]} |
          {error, {bad_gateway, term()}} |
          {error, {not_found, [{command, Cmd::module()}]}}.

execute(Cmd, Input) ->
  Module = cmd_module(Cmd),
  case cmd_exists_p(Module) of
    true  -> do_execute(Module, Input);
    false -> {error, {not_found, [{command, Cmd}]}}
  end.

%%%_* Internal functions =======================================================

cmd_exists_p(Cmd) ->
  code:which(Cmd) =/= non_existing.

cmd_module(Cmd) ->
  list_to_atom("edts_cmd_" ++ atom_to_list(Cmd)).

do_execute(Cmd, Input) ->
  ?LOG_DEBUG("Validating input for command ~p:~n~p", [Cmd, Input]),
  {ok, Ctx} = edts_cmd_lib:validate(Input, Cmd:spec()),
  ?LOG_DEBUG("Running Command ~p with Ctx ~p", [Cmd, Input]),
  try Cmd:execute(Ctx) of
    Result ->
      ?LOG_DEBUG("Command ~p returned ~p", [Cmd, Result]),
      Result
  catch
    error:Err ->
      ?LOG_ERROR(#{command => Cmd, input => Input, reason => Err}),
      {error, {bad_gateway, Err}}
  end.

%%%_* Unit tests ===============================================================

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
