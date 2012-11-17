%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc syntax-analysis library for edts.
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
-module(edts_syntax).

%%%_* Includes =================================================================
-include_lib("eunit/include/eunit.hrl").

%%%_* Exports ==================================================================

-export([parse_expression/1,
         parse_forms/1]).

%%%_* Defines ==================================================================

%%%_* Types ====================================================================

%%%_* API ======================================================================
%%------------------------------------------------------------------------------
%% @doc
%% Tokenize and parse String as a sequence of forms.
%% @end
-spec parse_forms(string()) -> Forms::erl_parse:abstract_form().
%%------------------------------------------------------------------------------
parse_forms(String) -> parse(scan(String)).

%%------------------------------------------------------------------------------
%% @doc
%% Tokenize and parse String as a single expression.
%% @end
-spec parse_expression(string()) -> Forms::erl_parse:abstract_form().
%%------------------------------------------------------------------------------
parse_expression(String) ->
  {ok, Forms} = erl_parse:parse_exprs(scan(String)),
  Forms.

%%%_* Internal functions =======================================================

%% Tokenize String
scan(String) ->
  case erl_scan:string(String) of
    {ok, Toks, _}        -> Toks;
    {error, Reason, Loc} -> erlang:error({scanner_error, Loc, Reason})
  end.

parse(Toks) ->
  parse(Toks, []).

%% Separate
parse([Tok = {dot, _}| T], Unparsed) ->
  [get_form(lists:reverse([Tok | Unparsed])) | parse(T, [])];
parse([Tok | T], Unparsed) -> parse(T, [Tok | Unparsed]);
parse([], []) -> [];
parse([], _) -> erlang:error({parser_error, no_trailing_dot}).

get_form(Toks) ->
  case erl_parse:parse_form(Toks) of
    {ok, Forms} -> Forms;
    {error, Reason} -> erlang:error({parser_error, Reason})
  end.

%%%_* Unit tests ===============================================================

%%%_* Test helpers =============================================================

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

