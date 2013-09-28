%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Debugger resource
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
-module(edts_debug_resource_processes).

%%%_* Exports ==================================================================

%% API
%% Webmachine callbacks
-export([ allowed_methods/2
        , content_types_provided/2
        , init/1
        , malformed_request/2
        , resource_exists/2]).

%% Handlers
-export([ to_json/2]).

%%%_* Includes =================================================================
-include_lib("webmachine/include/webmachine.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%_* Defines ==================================================================

%%%_* Types ====================================================================
%%%_* API ======================================================================


%% Webmachine callbacks
init(_Config) ->
  edts_log:debug("Call to ~p", [?MODULE]),
  {ok, orddict:new()}.

allowed_methods(ReqData, Ctx) ->
  {['GET'], ReqData, Ctx}.

content_types_provided(ReqData, Ctx) ->
  Map = [ {"application/json", to_json}
        , {"text/html",        to_json}
        , {"text/plain",       to_json}],
  {Map, ReqData, Ctx}.

malformed_request(ReqData, Ctx) ->
  edts_resource_lib:validate(ReqData, Ctx, [nodename]).

resource_exists(ReqData, Ctx) ->
  {edts_resource_lib:exists_p(ReqData, Ctx, [nodename]), ReqData, Ctx}.

to_json(ReqData, Ctx) ->
  Node        = orddict:fetch(nodename, Ctx),
  {ok, Procs} = edts:call(Node, edts_debug, processes),
  Data        = [format_proc(P) || P <- Procs],
  Body        = mochijson2:encode([{processes, Data}]),
  {Body, ReqData, Ctx}.

format_proc(Proc) ->
  orddict:map(fun format/2, Proc).

format(pid, Pid) ->
  PidStr0 = pid_to_list(Pid),
  PidStr = string:sub_string(PidStr0, 2, length(PidStr0) - 1),
  list_to_binary(PidStr);
format(init, Init) ->
  term_to_bin_str(Init);
format(info, Info) ->
  case Info of
    {} -> <<>>;
    _  -> term_to_bin_str(Info)
  end;
format(bindings, Bindings) ->
  [{Var, term_to_bin_str(Val)} || {Var, Val} <- Bindings];
format(_K, V) ->
  V.


%%%_* Internal functions =======================================================
term_to_bin_str(Term) ->
  list_to_binary(lists:flatten(io_lib:format("~p", [Term]))).

%%%_* Unit tests ===============================================================
init_test() ->
  ?assertEqual({ok, orddict:new()}, init(foo)).

allowed_methods_test() ->
  ?assertEqual({['GET'], foo, bar}, allowed_methods(foo, bar)).

content_types_provided_test() ->
  ?assertEqual({[ {"application/json", to_json}
                , {"text/html",        to_json}
                , {"text/plain",       to_json} ], foo, bar},
              content_types_provided(foo, bar)).

to_json_test() ->
  meck:unload(),
  meck:new(edts_debug),
  meck:expect(edts_debug, interpreted_modules, fun(true) -> {ok, [foo]}
                                                end),
  Dict1 = orddict:from_list([{nodename, true}]),
  Res = to_json(req_data, Dict1),
  ?assertMatch({_, req_data, Dict1}, Res),

  JSON = element(1, Res),
  ?assertEqual({struct, [{<<"modules">>,[<<"foo">>]}]},
               mochijson2:decode(JSON)),
  meck:unload().


%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
