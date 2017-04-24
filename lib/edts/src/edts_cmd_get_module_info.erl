%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc get_module_info command
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
-module(edts_cmd_get_module_info).

%%%_* Exports ==================================================================

%% API
-export([spec/0,
         execute/1]).

%%%_* Includes =================================================================
%%%_* Defines ==================================================================
%%%_* Types ====================================================================
%%%_* API ======================================================================

spec() ->
  [nodename, module, info_level].

execute(Ctx) ->
    Node   = orddict:fetch(nodename, Ctx),
    Module = orddict:fetch(module, Ctx),
    Level  = orddict:fetch(info_level, Ctx),
    {ok, Info} = edts:call(Node, edts_code, get_module_info, [Module, Level]),
    {ok, format(Info)}.

%%%_* Internal functions =======================================================

format(Info) ->
  lists:foldl(fun format/2, [], Info).

format({exports, Exports}, Acc) ->
  [{exports, Exports}|Acc];
format({source, Source}, Acc) ->
  [{source, list_to_binary(Source)}|Acc];
format({time, {{Y, Mo, D}, {H, Mi, S}}}, Acc) ->
  Fmt = "~b-~2.10.0b-~2.10.0b ~2.10.0b:~2.10.0b:~2.10.0b",
  Str = lists:flatten(io_lib:format(Fmt, [Y, Mo, D, H, Mi, S])),
  [{time, list_to_binary(Str)}|Acc];
format({records, Recs0}, Acc) ->
  Recs = [lists:map(fun format_element/1, Rec) || Rec <- Recs0],
  [{records, Recs}|Acc];
format({functions, Funs0}, Acc) ->
  Funs = [lists:map(fun format_element/1, Fun) || Fun <- Funs0],
  [{functions, Funs}|Acc];
format({imports, Imports}, Acc) ->
  [{imports, Imports}|Acc];
format({includes, Includes}, Acc) ->
  [{includes, [list_to_binary(I) || I <- Includes]}|Acc];
format({module, _} = Module, Acc) ->
  [Module|Acc];
format(_, Acc) ->
  Acc.

format_element({source, Source}) -> {source, list_to_binary(Source)};
format_element(Attr)             -> Attr.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
