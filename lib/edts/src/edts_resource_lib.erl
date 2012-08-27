%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Convenience library for resources
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
-module(edts_resource_lib).

%%%_* Exports ==================================================================

%% Application callbacks
-export([ make_nodename/1
        , validate/3]).

%%%_* Includes =================================================================

%%%_* Defines ==================================================================

%%%_* Types ====================================================================

%%%_* API ======================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Validate ReqData
%% @end
-spec validate(wrq:req_data(), orddict:orddict(), [atom]) ->
               {boolean(), wrq:req_data(), orddict:orddict()}.
%%------------------------------------------------------------------------------
validate(ReqData0, Ctx0, Keys) ->
  F = fun(Key, {ReqData, Ctx}) ->
          case (map_fun(Key))(ReqData, Ctx) of
            {ok, Value} ->
              {ReqData, orddict:store(Key, Value, Ctx)};
            error       ->
              throw({error, Key})
          end
      end,
  try
    {ReqData, Ctx} = lists:foldl(F, {ReqData0, Ctx0}, Keys),
    {false, ReqData, Ctx}
  catch throw:{error, _} = E -> {true, ReqData0, orddict:store(error, E, Ctx0)}
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Try to construct a node sname from a string.
%% @end
-spec make_nodename(string()) -> node().
%%------------------------------------------------------------------------------
make_nodename(NameStr) ->
  [_Name, Host] = string:tokens(atom_to_list(node()), "@git "),
  list_to_atom(hd(string:tokens(NameStr, "@")) ++ "@" ++ Host).

%%%_* Internal functions =======================================================

map_fun(arity)      -> fun arity/2;
map_fun(exported)   -> fun exported/2;
map_fun(function)   -> fun function/2;
map_fun(info_level) -> fun info_level/2;
map_fun(module)     -> fun module/2;
map_fun(nodename)   -> fun nodename/2.

%%------------------------------------------------------------------------------
%% @doc
%% Validate arity
%% @end
-spec arity(wrq:req_data(), orddict:orddict()) ->
               {ok, non_neg_integer()} | error.
%%------------------------------------------------------------------------------
arity(ReqData, _Ctx) ->
  try
    case list_to_integer(wrq:path_info(arity, ReqData)) of
      Arity when Arity >= 0 -> {ok, Arity};
      _ -> error
    end
  catch error:badarg -> error
  end.


%%------------------------------------------------------------------------------
%% @doc
%% Validate export parameter
%% @end
-spec exported(wrq:req_data(), orddict:orddict()) ->
                    {ok,  boolean() | all} | error.
%%------------------------------------------------------------------------------
exported(ReqData, _Ctx) ->
  case wrq:get_qs_value("exported", ReqData) of
    undefined -> {ok, all};
    "all"     -> {ok, all};
    "true"    -> {ok, true};
    "false"   -> {ok, false};
    _         -> error
  end.


%%------------------------------------------------------------------------------
%% @doc
%% Validate function
%% @end
-spec function(wrq:req_data(), orddict:orddict()) ->
                    {ok, module()} | error.
%%------------------------------------------------------------------------------
function(ReqData, _Ctx) ->
  {ok, list_to_atom(wrq:path_info(function, ReqData))}.


%%------------------------------------------------------------------------------
%% @doc
%% Validate arity
%% @end
-spec info_level(wrq:req_data(), orddict:orddict()) ->
                    {ok, basic | detailed} | error.
%%------------------------------------------------------------------------------
info_level(ReqData, _Ctx) ->
  case wrq:get_qs_value("info_level", ReqData) of
    undefined  -> {ok, basic};
    "basic"    -> {ok, basic};
    "detailed" -> {ok, detailed};
    _          -> error
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Validate module
%% @end
-spec module(wrq:req_data(), orddict:orddict()) ->
                    {ok, module()} | error.
%%------------------------------------------------------------------------------
module(ReqData, _Ctx) ->
  {ok, list_to_atom(wrq:path_info(module, ReqData))}.


%%------------------------------------------------------------------------------
%% @doc
%% Validate nodename
%% @end
-spec nodename(wrq:req_data(), orddict:orddict()) ->
               {ok, node()} | error.
%%------------------------------------------------------------------------------
nodename(ReqData, _Ctx) ->
  Nodename = make_nodename(wrq:path_info(nodename, ReqData)),
  case edts:node_reachable(Nodename) of
    true  -> {ok, Nodename};
    false -> error
  end.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

