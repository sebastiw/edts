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

-export([free_vars/1,
         free_vars/2,
         parse_expressions/1,
         parse_forms/1,
         parse_term/1]).

%%%_* Defines ==================================================================

%%%_* Types ====================================================================

%%%_* API ======================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Equivalent to free_vars(Snippet, 1).
%% @end
-spec free_vars(Text::string()) -> {ok, FreeVars::[atom()]} |
                                   {error, term()}.
%% @equiv free_vars(Text, 1)
%%------------------------------------------------------------------------------
free_vars(Snippet) -> free_vars(Snippet, 1).

%%------------------------------------------------------------------------------
%% @doc
%% Return a list of free variables in Snippet.
%% @end
-spec free_vars(Text::string(), pos_integer()) -> {ok, FreeVars::[atom()]} |
                                                  {error, term()}.
%% @equiv free_vars(Text, 1)
%%------------------------------------------------------------------------------
free_vars(Text, StartLine) ->
  %% StartLine/EndLine may be useful in error messages.
  {ok, Ts, EndLine} = erl_scan:string(Text, StartLine),
  %%Ts1 = reverse(strip(reverse(Ts))),
  Ts2 = [{'begin', 1}] ++ Ts ++ [{'end', EndLine}, {dot, EndLine}],
  case erl_parse:parse_exprs(Ts2) of
    {ok, Es} ->
      E = erl_syntax:block_expr(Es),
      E1 = erl_syntax_lib:annotate_bindings(E, ordsets:new()),
      {value, {free, Vs}} =
        lists:keysearch(free, 1, erl_syntax:get_ann(E1)),
      {ok, Vs};
    {error, {_Line, erl_parse, _Reason}} = Err -> Err
    end.


%%------------------------------------------------------------------------------
%% @doc
%% Tokenize and parse String as a sequence of forms.
%% @end
-spec parse_forms(string()) -> {ok, Forms::erl_parse:abstract_form()} |
                               {error, term()}.
%%------------------------------------------------------------------------------
parse_forms(String) -> parse(scan(String)).

%%------------------------------------------------------------------------------
%% @doc
%% Tokenize and parse String as a sequence of expressions.
%% @end
-spec parse_expressions(string()) -> {ok, Forms::erl_parse:abstract_form()} |
                                     {error, term()}.
%%------------------------------------------------------------------------------
parse_expressions(String) ->
  case erl_parse:parse_exprs(scan(String)) of
    {ok, _}    = Res -> Res;
    {error, _} = Err -> Err
  end.


%%------------------------------------------------------------------------------
%% @doc
%% Tokenize and parse String as an erlang term
%% @end
-spec parse_term(string()) -> {ok, term()}.
%%------------------------------------------------------------------------------
parse_term(String) ->
  {ok, Exprs} = parse_expressions(String),
  {value, Term, _} = erl_eval:exprs(Exprs, erl_eval:new_bindings()),
  {ok, Term}.


%%%_* Internal functions =======================================================

%% Tokenize String
scan(String) ->
  case erl_scan:string(String) of
    {ok, Toks, _}       -> Toks;
    {error, _, _} = Err -> Err
  end.

parse(Toks) ->
  case parse(Toks, []) of
    {error, _} = Err -> Err;
    Res              -> {ok, Res}
  end.

%% Separate
parse([Tok = {dot, _}| T], Unparsed) ->
  [get_form(lists:reverse([Tok | Unparsed])) | parse(T, [])];
parse([Tok | T], Unparsed) -> parse(T, [Tok | Unparsed]);
parse([], []) -> [];
parse([], Unparsed) -> get_form(lists:reverse(Unparsed)).

get_form(Toks) ->
  case erl_parse:parse_form(Toks) of
    {ok, Forms}      -> Forms;
    {error, _} = Err -> Err
  end.

%%%_* Unit tests ===============================================================

parse_expressions_test_() ->
  [?_assertMatch({error, {_, erl_parse, _}},
                 parse_expressions("foo(fun() -> ok end)")),
   ?_assertMatch({ok, [{call,1, {atom, 1, foo}, [_]}]},
                 parse_expressions("foo(fun() -> ok end)."))
  ].

parse_forms_test_() ->
  [?_assertMatch({error, {_, erl_parse, _}},
                 parse_forms("foo(fun() -> ok end)")),
   ?_assertMatch({ok, [{function, 1, foo, _, [_]}]},
                 parse_forms("foo() -> ok."))
  ].

free_vars_test_() ->
  [?_assertMatch({error, {_, erl_parse, _}}, free_vars("foo sth,")),
   ?_assertEqual({ok, []}, free_vars("ok")),
   ?_assertEqual({ok, ['Bar', 'Baz']}, free_vars("foo(Bar, Baz)"))
  ].

%%%_* Test helpers =============================================================

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

