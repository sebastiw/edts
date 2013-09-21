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
-export([format_info/3]).

%%%_* Includes =================================================================

%%%_* Defines ==================================================================

%%%_* Types ====================================================================

%%%_* API ======================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
-spec format_info(edts_event:class(), edts_event:type(), term()) -> term().
%%------------------------------------------------------------------------------
format_info(edts_debug, starting, _) ->
  [{type, starting}];
format_info(edts_debug, _Type, Event) ->
  format_info(Event).


%%%_* Internal functiyons ======================================================

format_info({Type, Mod}) when Type =:= interpret orelse
                                  Type =:= no_interpret ->
  [{type, Type},
   {module, Mod}];
format_info({new_process, {Pid, {Mod, Fun, Args}, Status, Info}}) ->
  [{type,     new_process},
   {pid,      edts_util:pid2atom(Pid)},
   {module,   Mod},
   {function, Fun},
   {args,     [list_to_binary(lists:flatten(io_lib:format("~w", [A]))) ||
                A <- Args]},
   {status,   Status},
   {info,     case is_tuple(Info) of
                true  -> tuple_to_list(Info);
                false -> Info
              end}];
format_info({new_status, Pid, Status, Info}) ->
  InfoProps = case Info of
                {Mod, Line} -> [{module, Mod}, {line, Line}];
                Reason when is_atom(Reason) -> [{reason, Reason}];
                {} -> []
              end,
  [{type,   new_status},
   {pid,    edts_util:pid2atom(Pid)},
   {status, Status}] ++ InfoProps;
format_info({Type, {{Mod, Line}, Options}}) when Type =:= new_break orelse
                                                 Type =:= break_options ->
  [{type,   Type},
   {module, Mod},
   {line,   Line},
   {options, Options}];
format_info({delete_break, {Mod, Line}}) ->
  [{type,   delete_break},
   {module, Mod},
   {line,   Line}];
format_info({no_break, Mod}) ->
  [{type,   no_break},
   {module, Mod}].


%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

