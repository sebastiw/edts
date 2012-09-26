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

%% API
-export([add_module/2,
         add_application/2,
         add_directory/2,
         q/2,
         replace_module/3,
         set_default/2,
         set_library_path/2,
         start/1,
         update/1
        ]).

%% Internal exports.
-export([do_start/1]).

%%%_* Includes =================================================================

%%%_* Defines ==================================================================

%%%_* Types ====================================================================

%%%_* API ======================================================================

%% Forwarding to xref.
add_application(Srv, File)   -> xref:add_application(Srv, File).
add_directory(Srv, File)     -> xref:add_directory(Srv, File).
add_module(Srv, File)        -> xref:add_module(Srv, File).
q(Srv, Q)                    -> xref:q(Srv, Q).
replace_module(Srv, M, File) -> xref:replace_module(Srv, M, File).
set_default(Srv, Opts)       -> xref:set_default(Srv, Opts).
set_library_path(Srv, Opts)  -> xref:set_library_path(Srv, Opts).

%% Extended API
start(Srv) ->
  R = proc_lib:start(?MODULE, do_start, [Srv]),
  error_logger:info_msg("Done"),
  R.

update(Srv) ->
  Res = xref:update(Srv),
  {status, _, _, [_, _, _, _, Misc]} = sys:get_status(Srv),
  State = proplists:get_value("State", lists:append([D || {data, D} <- Misc])),
  file:write_file("foo", term_to_binary(State)),
  Res.


%%%_* Internal functions =======================================================
do_start(Srv) ->
  error_logger:info_msg("init"),
  case file:read_file("foo") of
    {ok, BinState} ->
      error_logger:info_msg("file read"),
      erlang:register(Srv, self()),
      proc_lib:init_ack({ok, self()}),
      gen_server:enter_loop(xref, [], binary_to_term(BinState));
    {error, _} ->
      proc_lib:init_ack({ok, self()}),
      xref:start(Srv)
  end.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

