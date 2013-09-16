%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% @end
%%% @author Thomas Järvstrand <tjarvstrand@gmail.com>
%%% @copyright
%%% Copyright 2012 Thomas Järvstrand <tjarvstrand@gmail.com>
%%%
%%% This file is part of foo.
%%%
%%% foo is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% foo is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with foo. If not, see <http://www.gnu.org/licenses/>.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration =======================================================
-module(edts_events_debug).

-behaviour(edts_events).

%%%_* Exports ==================================================================

%% API
-export([format_info/1]).

%%%_* Includes =================================================================

%%%_* Defines ==================================================================

%%%_* Types ====================================================================

%%%_* API ======================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
-spec format_info(node()) -> [{atom(), string()}].
%%------------------------------------------------------------------------------
format_info({interpret, Mod}) ->
  [{type, interpret},
   {module, Mod}];
format_info({no_interpret, Mod}) ->
  [{type, no_interpret},
   {module, Mod}];
format_info({new_process, Pid, Fun, Status, Info}) ->
  [{type,     new_process},
   {pid,      edts_util:pid2atom(Pid)},
   {function, Fun},
   {status,   Status},
   {info,     Info}];
format_info({new_status, Pid, Status, Info}) ->
  [{type,   new_status},
   {pid,    edts_util:pid2atom(Pid)},
   {status, Status},
   {info,   Info}];
format_info({new_break, {{Mod, Line}, Options}}) ->
  [{type,   new_break},
   {module, Mod},
   {line,   Line},
   {options, Options}];
format_info({delete_break, {Mod, Line}}) ->
  [{type,   delete_break},
   {module, Mod},
   {line,   Line}];
format_info({no_break, Mod}) ->
  [{type,   delete_break},
   {module, Mod}].


%%%_* Internal functiyons ======================================================

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

