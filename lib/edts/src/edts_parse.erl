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
-module(edts_parse).

%%%_* Includes =================================================================
-include_lib("eunit/include/eunit.hrl").

%%%_* Exports ==================================================================

-export([context/3,
         from_file/3,
         from_string/3]).

-export([expr/4]).

%%%_* Defines ==================================================================

-define(text(Token), element(3, element(2, Token))).
-define(value(Token), element(4, element(2, Token))).

-define(start_loc(Token), element(1, element(2, Token))).
-define(end_loc(Token), element(2, element(2, Token))).

-define(start_line(Token), element(1, ?start_loc(Token))).
-define(start_column(Token), element(2, ?start_loc(Token))).

-define(end_line(Token), element(1, ?end_loc(Token))).
-define(end_column(Token), element(2, ?end_loc(Token))).

%% true if Loc1 is after Loc2
-define(loc_after_or_at(Loc1, Loc2),
        (element(1, Loc1) > element(1, Loc2) orelse
          (element(1, Loc1) =:= element(1, Loc2) andalso
            (element(2, Loc1) >= element(2, Loc2))))).

-define(starts_after_or_at(Token, Loc),
        (?start_line(Token) > element(1, Loc) orelse
          (?start_line(Token) =:= element(1, Loc) andalso
             ?start_column(Token) >= element(2, Loc)))).

-define(ends_after_or_at(Token, Loc),
        (?end_line(Token) > element(1, Loc) orelse
          (?end_line(Token) =:= element(1, Loc) andalso
             ?end_column(Token) >= element(2, Loc)))).

-define(ends_before_or_at(Token, Loc),
        (?end_line(Token) > element(1, Loc) orelse
          (?end_line(Token) =:= element(1, Loc) andalso
             ?end_column(Token) =< element(2, Loc)))).

-define(starts_after(Token, Loc),
        (?start_line(Token) > element(1, Loc) orelse
          (?start_line(Token) =:= element(1, Loc) andalso
             ?start_column(Token) > element(2, Loc)))).

-define(ends_after(Token, Loc),
        (?end_line(Token) > element(1, Loc) orelse
          (?end_line(Token) =:= element(1, Loc) andalso
             ?end_column(Token) > element(2, Loc)))).

-define(ends_before(Token, Loc),
        (?end_line(Token) < element(1, Loc) orelse
          (?end_line(Token) =:= element(1, Loc) andalso
             ?end_column(Token) < element(2, Loc)))).


-define(closing_delimiter_p(Symbol),
        Symbol =:= ']' orelse
        Symbol =:= '}' orelse
        Symbol =:= ')' orelse
        Symbol =:= '>>').

-define(done(Tokens, Loc),
        (Tokens =:= [] orelse ?starts_after_or_at(hd(Tokens), Loc))).


%%%_* Types ====================================================================

%%%_* API ======================================================================

from_file(Start, File, Loc) ->
  case file:read_file(File) of
    {ok,    Bin}       -> from_string(Start, binary_to_list(Bin), Loc);
    {error, _} = Error -> Error
  end.

from_string(Start, String, Loc) ->
  case erl_scan:string(String, {1, 1}, [return_comments, text]) of
    {ok,    Tokens, _NextScanLoc} ->
      context(Start, Tokens, Loc);
    {error, {ErrStart, erl_scan, {string, TypeChar, Value}}, _Next} ->
      case ?loc_after_or_at(ErrStart, Loc) of
        true ->
          {CutLine, CutCol} = case ErrStart of
                                {Line, 1}      -> {Line - 1, - 1};
                                {Line, Column} -> {Line, Column - 1}
                              end,
          from_string(Start, string_cut({CutLine, CutCol}, String), Loc);
        false ->
          Ctxt =
            case TypeChar of
              $" -> {string, {ErrStart, [TypeChar] ++ Value, Value}};
              $' -> {atom, {ErrStart, [TypeChar] ++ Value, list_to_atom(Value)}}
            end,
          {[Ctxt, {Start, {{1, 1}, undefined, undefined}}], []}
        end;
    {error, _, _} = Error ->
      Error
  end.

context(Start, [], Loc) ->
  {[{Start, {Loc, undefined, undefined}}], undefined, []};
context(Start, Tokens0, Loc) ->
  [{_, {StartLoc, _, _, _}}|_] = Tokens = tokens(Tokens0),
  InitialCtxt = [{Start, {StartLoc, undefined, undefined}}],
  case ?MODULE:Start(Tokens, Loc, InitialCtxt, undefined) of
    {complete, {_Ctxt, undefined, []}} ->
      {error, {end_of_file, {1, 1}}};
    {complete, {_Ctxt, Prev, []}} ->
      {error, {end_of_file, ?end_loc(Prev)}};
    {complete, {_Ctxt, Prev, [Token|_]}} when ?ends_before(Prev, Loc) ->
      ErrAtom = list_to_atom("end_of_" ++ atom_to_list(Start)),
      {error, {ErrAtom, ?start_loc(Token)}};
    {complete, {_Ctxt, undefined, [Token|_]}} ->
      ErrAtom = list_to_atom("end_of_" ++ atom_to_list(Start)),
      {error, {ErrAtom, ?start_loc(Token)}};

    {partial, {_Ctxt, undefined, []}} ->
      {error, {end_of_file, {1, 1}}};
    {partial, {_Ctxt, Prev, []}} when ?ends_before(Prev, Loc) ->
      {error, {end_of_file, ?end_loc(Prev)}};
    {partial, Result} ->
      Result
  end.

%%%_* Internal functions =======================================================


%% TODO
%% - Types
%% - Functions
%% - Local function calls
%% - Qualified function calls
%% - Guards
%% - Matches
%% - Record fields
%% - Maps
%% - Funs
%% - Named funs
%% - Pids/ports/refs
%% - List comprehension
%% - Comments


%% exprs([], _Loc, Ctxt) ->
%%   {Ctxt, []};
%% exprs([Token|_] = Tokens, Loc, Ctxt) when
%%     ?starts_after_p(Token, Loc) ->
%%   {Ctxt, Tokens};
%% exprs([{',', _, _, _, _}|Tokens], Loc, [{binary, _, _, _}|_] = Ctxt) ->
%%   exprs(Tokens, Loc, Ctxt);
%% exprs([{',', _, _, _, _}|_] = Tokens, Loc, [_|Ctxt]) ->
%%   exprs(Tokens, Loc, Ctxt);
%% exprs([{Symbol, _, _, _, _}|_] = Tokens, _Loc, Ctxt) when
%%     Symbol =:= '}' orelse Symbol =:= ']' orelse Symbol =:= dot ->
%%   {Ctxt, Tokens};
%% exprs(Tokens0, Loc, Ctxt0) ->
%%   {Ctxt, Tokens} = bit_expr(Tokens0, Loc, Ctxt0),
%%   exprs(Tokens, Loc, Ctxt).




expr(Tokens, Loc, Ctxt, Prev) when ?done(Tokens, Loc) ->
  {partial, {Ctxt, Prev, Tokens}};

%% Parentesized sub-expression
expr([{'(', {Start, _, Text, _}} = Next|Tokens], Loc, Ctxt, _Prev) ->
  expr(Tokens, Loc, [{expr, {Start, Text, '('}}|Ctxt], Next);
expr([{')', _} = Next|Tokens], Loc, [{expr, {_, _,'('}}|Ctxt], _Prev) ->
  expr(Tokens, Loc, Ctxt, Next);
expr([{')', _}|_] = Tokens, Loc, [_|Ctxt], Prev) ->
  expr(Tokens, Loc, Ctxt, Prev);

%% Record
expr([{'#', {Start, End, _, _}},
      {atom, {_, _, Text0, Value}} = Next|Tokens],
     Loc,
     Ctxt,
     _Prev) when End =/= Loc ->
  expr(Tokens, Loc, [{record, {Start, "#" ++ Text0, Value}}|Ctxt], Next);

expr([{'{', _} = Next| [{_, {FieldStart, _, _, _}}|_] = Tokens],
     Loc,
     [{record, _}|_] = Ctxt,
     _Prev) ->
  expr(Tokens,
       Loc,
       [{field_name, {FieldStart, undefined, undefined}},
        {field, {FieldStart, undefined, 1}}|Ctxt],
      Next);

expr([{'=', _} = Next| [{_, {ValueStart, _, _, _}}|_] = Tokens],
     Loc,
     [{field_name, _}|Ctxt],
     _Prev) ->
  expr(Tokens,
       Loc,
       [{field_value, {ValueStart, undefined, undefined}}|Ctxt],
      Next);

expr([{'[', _}|_] = Tokens0, Loc, Ctxt0, Prev0) ->
  case list(Tokens0, Loc, Ctxt0, Prev0) of
    {partial, _} = Result -> Result;
    {complete, {Ctxt, Prev, Tokens}} -> expr(Tokens, Loc, Ctxt, Prev)
  end;

expr([{'{', _}|_] = Tokens0, Loc, Ctxt0, Prev0) ->
  case tuple(Tokens0, Loc, Ctxt0, Prev0) of
    {partial, _} = Result            -> Result;
    {complete, {Ctxt, Prev, Tokens}} -> expr(Tokens, Loc, Ctxt, Prev)
  end;
expr([{'<<', _}|_] = Tokens0, Loc, Ctxt0, Prev0) ->
  case binary(Tokens0, Loc, Ctxt0, Prev0) of
    {partial, _} = Result -> Result;
    {complete, {Ctxt, Prev, Tokens}} -> expr(Tokens, Loc, Ctxt, Prev)
  end;
expr([{string, {_, End, _, _}} = Next|Tokens], Loc, Ctxt, _Prev) when
    Loc =:= End ->
  expr(Tokens, Loc, Ctxt, Next);
expr([{atom, {_, End, "'" ++ _, _}} = Next|Tokens], Loc, Ctxt, _Prev) when
    Loc =:= End ->
  expr(Tokens, Loc, Ctxt, Next);
expr([{Type, _} = Next|Tokens], Loc, Ctxt0, _Prev) when
    Type =:= string orelse
    (Type =:= atom andalso hd(?text(Next)) =:= $') ->
  case ?ends_after(Next, Loc) of
    true ->
      Ctxt = [{Type, {?start_loc(Next), ?text(Next), ?value(Next)}}|Ctxt0],
      {partial, {Ctxt, Next, Tokens}};
    false ->
      expr(Tokens, Loc, Ctxt0, Next)
  end;
expr([{Type, _} = Next|Tokens], Loc, Ctxt0, _Prev) when
    Type =:= atom orelse
    Type =:= integer orelse
    Type =:= float orelse
    Type =:= char orelse
    Type =:= var orelse
    Type =:= '+' orelse
    Type =:= '++' orelse
    Type =:= '-' orelse
    Type =:= '--' orelse
    Type =:= '<' orelse
    Type =:= '>' orelse
    Type =:= '>=' orelse
    Type =:= '=<' orelse
    Type =:= '/' orelse
    Type =:= '*' orelse
    Type =:= 'div' orelse
    Type =:= 'rem' orelse
    Type =:= 'not' orelse
    Type =:= 'and' orelse
    Type =:= 'or' orelse
    Type =:= 'andalso' orelse
    Type =:= 'orelse' orelse
    Type =:= 'bnot' orelse
    Type =:= 'band' orelse
    Type =:= 'bor' orelse
    Type =:= 'bxor' orelse
    Type =:= '=' orelse
    Type =:= '==' orelse
    Type =:= '/=' orelse
    Type =:= '=:=' orelse
    Type =:= '=/=' orelse
    Type =:= '=:=' orelse
    Type =:= 'bsl' orelse
    Type =:= 'bsr' ->
  case ?ends_after_or_at(Next, Loc) of
    true ->
      Ctxt = [{Type, {?start_loc(Next), ?text(Next), ?value(Next)}}|Ctxt0],
      {partial, {Ctxt, Next, Tokens}};
    false ->
      expr(Tokens, Loc, Ctxt0, Next)
  end;
expr(Tokens, _Loc, Ctxt, Prev) ->
  {complete, {Ctxt, Prev, Tokens}}.



list(Tokens, Loc, Ctxt, Prev) when ?done(Tokens, Loc) ->
  {partial, {Ctxt, Prev, Tokens}};
list([{'[', _} = Next|Tokens0], Loc, Ctxt0, _Prev0) ->
  Ctxt1 = [{expr, {?start_loc(Next), ?text(Next), '['}}|Ctxt0],
  case list_elements(Tokens0, Loc, Ctxt1, Next) of
    {partial, _} = Result -> Result;
    {complete, {Ctxt, Prev, Tokens}} -> list(Tokens, Prev, Ctxt, Prev)
  end;
list([{']', _} = Next|Tokens], _Loc, [{expr, {_, _, '['}}|Ctxt], _Prev) ->
  {complete, {Ctxt, Next, Tokens}};
list([{']', _}|_] = Tokens, Loc, [_|Ctxt], Prev) ->
  list(Tokens, Loc, Ctxt, Prev).


list_elements(Tokens, Loc, Ctxt, Prev) when ?done(Tokens, Loc) ->
  {partial, {Ctxt, Prev, Tokens}};
list_elements([{',', _} = Next|Tokens], Loc, [{expr, {_, _, '['}}|_] = Ctxt, _Prev) ->
  list_elements(Tokens, Loc, Ctxt, Next);
list_elements([{',', _}|_] = Tokens, Loc, [_|Ctxt], Prev) ->
  list_elements(Tokens, Loc, Ctxt, Prev);
list_elements([{'|', _} = Next|Tokens], Loc, [{expr, {_, _, '['}}|_] = Ctxt, _Prev) ->
  list_elements(Tokens, Loc, Ctxt, Next);
list_elements([{'|', _}|_] = Tokens, Loc, [_|Ctxt], Prev) ->
  list_elements(Tokens, Loc, Ctxt, Prev);
list_elements([{']', _}|_] = Tokens, _Loc, Ctxt, Prev) ->
  {complete, {Ctxt, Prev, Tokens}};
list_elements(Tokens0, Loc, [_|_] = Ctxt0, Prev0) ->
  case expr(Tokens0, Loc, Ctxt0, Prev0) of
    {partial, _} = Result            -> Result;
    {complete, {Ctxt, Prev, Tokens}} -> list_elements(Tokens, Loc, Ctxt, Prev)
  end.


tuple(Tokens, Loc, Ctxt, Prev) when ?done(Tokens, Loc) ->
  {partial, {Ctxt, Prev, Tokens}};
tuple([{'{', {Start, _, Text, _}} = Next|Tokens0], Loc, Ctxt0, _Prev0) ->
  Ctxt1 = [{expr, {Start, Text, '{'}}|Ctxt0],
  case tuple_elements(Tokens0, Loc, Ctxt1, Next) of
    {partial, _} = Result            -> Result;
    {complete, {Ctxt, Prev, Tokens}} -> tuple(Tokens, Loc, Ctxt, Prev)
  end;
tuple([{'}', _} = Next|Tokens], _Loc, [{expr, {_, _, '{'}}|Ctxt], _Prev) ->
  {complete, {Ctxt, Next, Tokens}};
tuple([{'}', _}|_] = Tokens, Loc, [_|Ctxt], Prev) ->
  tuple(Tokens, Loc, Ctxt, Prev).

tuple_elements(Tokens, Loc, Ctxt, Prev) when ?done(Tokens, Loc) ->
  {partial, {Ctxt, Prev, Tokens}};
tuple_elements([{',', _} = Next|Tokens], Loc, [{expr, {_, _, '{'}}|_] = Ctxt, _Prev) ->
  tuple_elements(Tokens, Loc, Ctxt, Next);
tuple_elements([{',', _}|_] = Tokens, Loc, [_|Ctxt], Prev) ->
  tuple_elements(Tokens, Loc, Ctxt, Prev);
tuple_elements([{'}', _}|_] = Tokens, _Loc, Ctxt, Prev) ->
  {complete, {Ctxt, Prev, Tokens}};
tuple_elements(Tokens0, Loc, [_|_] = Ctxt0, Prev0) ->
  case expr(Tokens0, Loc, Ctxt0, Prev0) of
    {partial, _} = Result            -> Result;
    {complete, {Ctxt, Prev, Tokens}} -> tuple_elements(Tokens, Loc, Ctxt, Prev)
  end.

binary(Tokens, Loc, Ctxt, Prev) when ?done(Tokens, Loc) ->
  {partial, {Ctxt, Prev, Tokens}};
binary([{'<<', {Start, _, Text, _}} = Next|Tokens0], Loc, Ctxt0, _Prev0) ->
  Ctxt1 = [{expr, {Start, Text, binary}}|Ctxt0],
  case binary_elements(Tokens0, Loc, Ctxt1, Next) of
    {partial, _} = Result            -> Result;
    {complete, {Ctxt, Prev, Tokens}} -> binary(Tokens, Loc, Ctxt, Prev)
  end;
binary([{'>>', _} = Next|Tokens], _Loc, [{expr, {_, _, binary}}|_] = Ctxt, _Prev) ->
  {complete, {Ctxt, Next, Tokens}};
binary([{'>>', _}|_] = Tokens, Loc, [_|Ctxt], Prev) ->
  binary(Tokens, Loc, Ctxt, Prev).

binary_elements(Tokens, Loc, Ctxt, Prev) when ?done(Tokens, Loc) ->
  {partial, {Ctxt, Prev, Tokens}};
binary_elements([{',', _} = Next|Tokens], Loc, [{expr, {_, _, binary}}|_] = Ctxt, _Prev) ->
  binary_elements(Tokens, Loc, Ctxt, Next);
binary_elements([{',', _}|_] = Tokens, Loc, [_|Ctxt], Prev) ->
  binary_elements(Tokens, Loc, Ctxt, Prev);
binary_elements([{'>>', _}|_] = Tokens, _Loc, Ctxt, Prev) ->
  {complete, {Ctxt, Prev, Tokens}};
binary_elements(Tokens0, Loc, Ctxt0, Prev0) ->
  case binary_element(Tokens0, Loc, Ctxt0, Prev0) of
    {partial, _} = Result            -> Result;
    {complete, {Ctxt, Prev, Tokens}} -> binary_elements(Tokens, Loc, Ctxt, Prev)
  end.

binary_element(Tokens, Loc, Ctxt, Prev) when ?done(Tokens, Loc) ->
  {partial, {Ctxt, Prev, Tokens}};
binary_element(Tokens0, Loc, Ctxt0, Prev0) ->
  case binary_expr(Tokens0, Loc, Ctxt0, Prev0) of
    {partial, _} = Result ->
      Result;
    {complete, {_Ctxt, _Prev, []}} = Result ->
      Result;
    {complete, {Ctxt, Prev, [{':', _}|_] = Tokens}} ->
      binary_element_suffix(Tokens, Loc, Ctxt, Prev);
    {complete, {Ctxt, Prev, [{'/', _}|_] = Tokens}} ->
      binary_element_suffix(Tokens, Loc, Ctxt, Prev);
    {complete, _} = Result ->
      Result
  end.

binary_expr(Tokens, Loc, Ctxt, Prev) when ?done(Tokens, Loc) ->
  {partial, {Ctxt, Prev, Tokens}};
binary_expr([{'(', {Start, _, Text, _}} = Next|Tokens], Loc, Ctxt, _Prev) ->
  expr(Tokens, Loc, [{expr, {Start, Text, '('}}|Ctxt], Next);
binary_expr([{')', _} = Next|Tokens], Loc, [{expr, {_, _,'('}}|Ctxt], _Prev) ->
  expr(Tokens, Loc, Ctxt, Next);
binary_expr([{')', _}|_] = Tokens, Loc, [_|Ctxt], Prev) ->
  expr(Tokens, Loc, Ctxt, Prev);
binary_expr([{Type, _} = Next|Tokens], Loc, Ctxt0, _Prev) when
    Type =:= integer orelse
    Type =:= char orelse
    Type =:= var ->
  case ?ends_after_or_at(Next, Loc) of
    true ->
      Ctxt = [{Type, {?start_loc(Next), ?text(Next), ?value(Next)}}|Ctxt0],
      {partial, {Ctxt, Next, Tokens}};
    false ->
      {complete, {Ctxt0, Next, Tokens}}
  end;
binary_expr([{string, _} = Next|Tokens], Loc, Ctxt0, _Prev) ->
  case ?ends_after(Next, Loc) of
    true ->
      Ctxt = [{string, {?start_loc(Next), ?text(Next), ?value(Next)}}|Ctxt0],
      {partial, {Ctxt, Next, Tokens}};
    false ->
      {complete, {Ctxt0, Next, Tokens}}
  end;
binary_expr(Tokens, _Loc, Ctxt, Prev) ->
  {complete, {Ctxt, Prev, Tokens}}.

binary_element_suffix(Tokens, Loc, Ctxt, Prev) when ?done(Tokens, Loc) ->
  {partial, {Ctxt, Prev, Tokens}};
binary_element_suffix([{':', _}|_] = Tokens0, Loc, Ctxt0, Prev0) ->
  case binary_size(Tokens0, Loc, Ctxt0, Prev0) of
    {partial, _} = Result ->
      Result;
    {complete, {_Ctxt, _Prev, []}} = Result ->
      Result;
    {complete, {Ctxt, Prev, [{'/', _}|_] = Tokens}} ->
      binary_type(Tokens, Loc, Ctxt, Prev);
    {complete, _} = Result ->
      Result
  end;
binary_element_suffix([{'/', _}|_] = Tokens0, Loc, Ctxt0, Prev0) ->
  binary_type(Tokens0, Loc, Ctxt0, Prev0).

binary_size(Tokens, Loc, Ctxt, Prev) when ?done(Tokens, Loc) ->
  {partial, {Ctxt, Prev, Tokens}};
binary_size([{':', _} = Next|Tokens0], Loc, Ctxt0, _Prev0) ->
  Ctxt1 = [{expr, {?start_loc(Next), ":", bin_size}}|Ctxt0],
  case binary_size_expr(Tokens0, Loc, Ctxt1, Next) of
    {partial, _} = Result ->
      Result;
    {complete, {[_|Ctxt], Prev, Tokens}} ->
      {complete, {Ctxt, Prev, Tokens}}
  end.

binary_size_expr(Tokens, Loc, Ctxt, Prev) when ?done(Tokens, Loc) ->
  {partial, {Ctxt, Prev, Tokens}};
binary_size_expr([{'(', {Start, _, Text, _}} = Next|Tokens], Loc, Ctxt, _Prev) ->
  expr(Tokens, Loc, [{expr, {Start, Text, '('}}|Ctxt], Next);
binary_size_expr([{')', _} = Next|Tokens], Loc, [{expr, {_, _,'('}}|Ctxt], _Prev) ->
  expr(Tokens, Loc, Ctxt, Next);
binary_size_expr([{')', _}|_] = Tokens, Loc, [_|Ctxt], Prev) ->
  expr(Tokens, Loc, Ctxt, Prev);
binary_size_expr([{Type, _} = Next|Tokens], Loc, Ctxt0, _Prev) when
    Type =:= integer orelse
    Type =:= var ->
  case ?ends_after_or_at(Next, Loc) of
    true ->
      Ctxt = [{Type, {?start_loc(Next), ?text(Next), ?value(Next)}}|Ctxt0],
      {partial, {Ctxt, Next, Tokens}};
    false ->
      {complete, {Ctxt0, Next, Tokens}}
  end;
binary_size_expr([{',', _}|_] = Tokens, _Loc, Ctxt, Prev) ->
  {complete, {Ctxt, Prev, Tokens}}.


binary_type(Tokens, Loc, Ctxt, Prev) when ?done(Tokens, Loc) ->
  {partial, {Ctxt, Prev, Tokens}};
binary_type([{'/', _} = Next|_] = Tokens0, Loc, Ctxt0, _Prev0) ->
  case binary_type_list(Tokens0, Loc, Ctxt0, Next) of
    {partial, _} = Result ->
      Result;
    {complete, {_Ctxt, _Prev, []}} = Result ->
      Result;
    {complete, {[_|Ctxt], Prev, Tokens}} ->
      {complete, {Ctxt, Prev, Tokens}}
  end.

binary_type_list(Tokens, Loc, Ctxt, Prev) when ?done(Tokens, Loc) ->
  {partial, {Ctxt, Prev, Tokens}};
binary_type_list([{'/', _} = Next|Tokens0], Loc, Ctxt0, _Prev) ->
  Ctxt1 = [{expr, {?start_loc(Next), ?text(Next), bin_type_list}}|Ctxt0],
  case binary_type_elements(Tokens0, Loc, Ctxt1, Next) of
    {partial, _} = Result ->
      Result;
    {complete, {[_|Ctxt], Prev, Tokens}} ->
      {complete, {Ctxt, Prev, Tokens}}
  end.

binary_type_elements(Tokens, Loc, Ctxt, Prev) when ?done(Tokens, Loc) ->
  {partial, {Ctxt, Prev, Tokens}};
binary_type_elements([{'-', _} = Next|Tokens], Loc, [{expr, {_, _, bin_type_list}}|_] = Ctxt, _Prev) ->
  binary_type_elements(Tokens, Loc, Ctxt, Next);
binary_type_elements([{'-', _}|_] = Tokens, Loc, [_|Ctxt], Prev) ->
  binary_type_elements(Tokens, Loc, Ctxt, Prev);
binary_type_elements([{atom, _}|_] = Tokens0, Loc, [_|_] = Ctxt0, Prev0) ->
  case binary_type_element(Tokens0, Loc, Ctxt0, Prev0) of
    {partial, _} = Result            -> Result;
    {complete, {Ctxt, Prev, Tokens}} -> binary_type_elements(Tokens, Loc, Ctxt, Prev)
  end;
binary_type_elements(Tokens, _Loc, Ctxt, Prev) ->
  {complete, {Ctxt, Prev, Tokens}}.


%% TODO unit binary type specifier
binary_type_element(Tokens, Loc, Ctxt, Prev) when ?done(Tokens, Loc) ->
  {partial, {Ctxt, Prev, Tokens}};
binary_type_element([{atom, _} = Next|Tokens], Loc, Ctxt0, _Prev) when
    %% Type
    ?value(Next) =:= integer orelse
    ?value(Next) =:= float orelse
    ?value(Next) =:= binary orelse
    ?value(Next) =:= bytes orelse
    ?value(Next) =:= bitstring orelse
    ?value(Next) =:= bits orelse
    ?value(Next) =:= utf8 orelse
    ?value(Next) =:= utf16 orelse
    ?value(Next) =:= utf32 orelse
    %% Signedness
    ?value(Next) =:= signed orelse
    ?value(Next) =:= unsigned orelse
    %% Endianness
    ?value(Next) =:= big orelse
    ?value(Next) =:= little orelse
    ?value(Next) =:= native ->
  case ?ends_after_or_at(Next, Loc) of
    true ->
      Ctxt = [{atom, {?start_loc(Next), ?text(Next), ?value(Next)}}|Ctxt0],
      {partial, {Ctxt, Next, Tokens}};
    false ->
      {complete, {Ctxt0, Next, Tokens}}
  end;
binary_type_element(Tokens, _Loc, Ctxt, Prev) ->
  {complete, {Ctxt, Prev, Tokens}}.

string_cut({Line, Column}, String) ->
  cut_after_col(Column, cut_after_line(Line, String)).

cut_after_col(Line, String) ->
  cut_after_col(Line, String, []).

cut_after_col(_Line, [], Acc) ->
  lists:reverse(Acc);
cut_after_col(0,    _String, Acc) ->
  lists:reverse(Acc);
cut_after_col(Col, [C|String], Acc) ->
  cut_after_col(Col - 1, String, [C|Acc]).

cut_after_line(Line, String) ->
  cut_after_line(Line, String, []).

cut_after_line(0,     _String,      Acc) ->
  lists:reverse(Acc);
cut_after_line(_Line, [],           Acc) ->
  lists:reverse(Acc);
cut_after_line( Line, [$\n|String], Acc) ->
  cut_after_line(Line - 1, String, [$\n|Acc]);
cut_after_line( Line, [C|String],   Acc) ->
  cut_after_line(Line, String, [C|Acc]).

tokens(Tokens) ->
  lists:map(fun token/1, Tokens).

token({Type, Properties}) ->
  token({Type, Properties, undefined});
token({Type, Properties, Value}) ->
  {text, Text} = lists:keyfind(text, 1, Properties),
  {Start, End} = token_boundaries(Properties),
  {Type, {Start, End, Text, Value}}.

token_boundaries(Properties) ->
  StartLine = proplists:get_value(line, Properties),
  StartColumn = proplists:get_value(column, Properties),
  Text = proplists:get_value(text, Properties),
  {{StartLine, StartColumn}, text_end_loc(Text, StartLine, StartColumn)}.

% Newline characters are included in the column count for each line.
text_end_loc(Text, StartLine, StartColumn) ->
  case lists:reverse(re:split(Text, "\\R", [{newline, any}])) of
    [<<>>, <<>>] -> % A single newline
      {StartLine, StartColumn + 1};
    Lines0       ->
      case lists:dropwhile(fun(Line) -> Line =:= <<>> end, Lines0) of
        [Line]           -> {StartLine, StartColumn + size(Line)};
        [Line|_] = Lines -> {StartLine + length(Lines) - 1, size(Line)}
      end
  end.


%%%_* Unit tests ===============================================================

cut_after_col_test_() ->
  [?_assertEqual("", cut_after_col(-1, "")),
   ?_assertEqual("", cut_after_col(0, "")),
   ?_assertEqual("", cut_after_col(1, "")),
   ?_assertEqual("", cut_after_col(2, "")),
   ?_assertEqual("a", cut_after_col(-1, "a")),
   ?_assertEqual("", cut_after_col(0, "a")),
   ?_assertEqual("a", cut_after_col(1, "a")),
   ?_assertEqual("a", cut_after_col(2, "a")),
   ?_assertEqual("ab", cut_after_col(-1, "ab")),
   ?_assertEqual("", cut_after_col(0, "ab")),
   ?_assertEqual("a", cut_after_col(1, "ab")),
   ?_assertEqual("ab", cut_after_col(2, "ab"))
  ].

cut_lines_test_() ->
  [?_assertEqual("", cut_after_line(0, "")),
   ?_assertEqual("", cut_after_line(1, "")),
   ?_assertEqual("", cut_after_line(2, "")),
   ?_assertEqual("", cut_after_line(0, "a")),
   ?_assertEqual("a", cut_after_line(1, "a")),
   ?_assertEqual("a", cut_after_line(2, "a")),
   ?_assertEqual("", cut_after_line(0, "a\n")),
   ?_assertEqual("a\n", cut_after_line(1, "a\n")),
   ?_assertEqual("a\n", cut_after_line(2, "a\n")),
   ?_assertEqual("", cut_after_line(0, "a\nb")),
   ?_assertEqual("a\n", cut_after_line(1, "a\nb")),
   ?_assertEqual("a\nb", cut_after_line(2, "a\nb")),
   ?_assertEqual("", cut_after_line(0, "a\nb\n")),
   ?_assertEqual("a\n", cut_after_line(1, "a\nb\n")),
   ?_assertEqual("a\nb\n", cut_after_line(2, "a\nb\n")),
   ?_assertEqual("", cut_after_line(0, "a\nb\nc")),
   ?_assertEqual("a\n", cut_after_line(1, "a\nb\nc")),
   ?_assertEqual("a\nb\n", cut_after_line(2, "a\nb\nc"))
  ].

start_line_test_() ->
  [?_assertEqual(1,
                 ?start_line({string, {{1, 2}, {3, 4}, "a\n  b", "a\n  b"}}))
  ].

start_column_test_() ->
  [?_assertEqual(2,
                 ?start_column({string, {{1, 2}, {3, 4}, "a\n  b", "a\n  b"}}))
  ].

end_line_test_() ->
  [?_assertEqual(3,
                 ?end_line({string, {{1, 2}, {3, 4}, "a\n  b", "a\n  b"}}))
  ].

end_column_test_() ->
  [?_assertEqual(4,
                 ?end_column({string, {{1, 2}, {3, 4}, "a\n  b", "a\n  b"}}))
  ].

%%%_* Test helpers =============================================================

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

