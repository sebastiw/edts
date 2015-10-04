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
-module(edts_parse_tests).

%%%_* Exports ==================================================================

%%%_* Includes =================================================================
-include_lib("eunit/include/eunit.hrl").


%%%_* Defines ==================================================================

%%%_* Types ====================================================================

%%%_* Tests ====================================================================

no_tokens_test_() ->
  [?_assertEqual({[{expr, {{1, 1}, undefined, undefined}}],
                  undefined,
                  []},
                 edts_parse:from_string(expr, "", {1, 1})),

   ?_assertEqual({[{expr, {{1, 2}, undefined, undefined}}],
                  undefined,
                  []},
                 edts_parse:from_string(expr, "", {1, 2})),


   ?_assertEqual({[{expr, {{2, 1}, undefined, undefined}}],
                  undefined,
                  []},
                 edts_parse:from_string(expr, "\n", {2, 1})),

   ?_assertEqual({[{expr, {{2, 2}, undefined, undefined}}],
                  undefined,
                  []},
                 edts_parse:from_string(expr, "\n", {2, 2}))
  ].

char_expr_test_() ->
  [?_assertEqual({[{char, {{1, 1}, "$A", 65}},
                   {expr, {{1, 1}, undefined, undefined}}],
                  {char, {{1, 1}, {1, 3}, "$A", 65}},
                  []},
                edts_parse:from_string(expr, "$A", {1, 2}))
  ].


integer_expr_test_() ->
  [?_assertEqual({[{integer, {{1, 1}, "123", 123}},
                   {expr, {{1, 1}, undefined, undefined}}],
                  {integer, {{1, 1}, {1, 4}, "123", 123}},
                  []},
                 edts_parse:from_string(expr, "123", {1, 2}))
  ].

float_expr_test_() ->
  [?_assertEqual({[{float, {{1, 1}, "1.23", 1.23}},
                   {expr, {{1, 1}, undefined, undefined}}],
                  {float, {{1, 1}, {1, 5}, "1.23", 1.23}},
                  []},
                 edts_parse:from_string(expr, "1.23", {1, 2}))
  ].

string_expr_test_() ->
  [?_assertEqual({[{string, {{1, 1}, "\"123\"", "123"}},
                   {expr, {{1, 1}, undefined, undefined}}],
                  {string, {{1, 1}, {1, 6}, "\"123\"", "123"}},
                  []},
                 edts_parse:from_string(expr, "\"123\"", {1, 2})),

   ?_assertEqual({[{atom, {{1, 1}, "a", a}},
                   {expr, {{1, 1}, undefined, undefined}}],
                  {atom, {{1, 1}, {1, 2}, "a", a}},
                  []},
                 edts_parse:from_string(expr, "a \"123", {1, 2})),

   ?_assertEqual({[{atom, {{1, 1}, "a", a}},
                   {expr, {{1, 1}, undefined, undefined}}],
                  {atom, {{1, 1}, {1, 2}, "a", a}},
                  []},
                 edts_parse:from_string(expr, "a \n\"123", {1, 2})),

   ?_assertEqual({[{string, {{1, 3}, "\"123", "123"}},
                   {expr, {{1, 1}, undefined, undefined}}],
                  []},
                 edts_parse:from_string(expr, "a \"123", {1, 4})),

   ?_assertEqual({[{expr, {{1, 1}, undefined, undefined}}],
                  {string, {{1, 1}, {1, 6}, "\"123\"", "123"}},
                  []},
                 edts_parse:from_string(expr, "\"123\"", {1, 6}))

  ].


atom_expr_test_() ->
  [
   ?_assertEqual({[{atom, {{1, 1}, "abc", abc}},
                   {expr, {{1, 1}, undefined, undefined}}],
                  {atom, {{1, 1}, {1, 4}, "abc", abc}},
                  []},
                 edts_parse:from_string(expr, "abc", {1, 2})),

   ?_assertEqual({[{atom, {{1, 1}, "'123'", '123'}},
                   {expr, {{1, 1}, undefined, undefined}}],
                  {atom, {{1, 1}, {1, 6}, "'123'", '123'}},
                  []},
                 edts_parse:from_string(expr, "'123'", {1, 2})),

   ?_assertEqual({[{atom, {{1, 1}, "a", a}},
                   {expr, {{1, 1}, undefined, undefined}}],
                  {atom, {{1, 1}, {1, 2}, "a", a}},
                  []},
                 edts_parse:from_string(expr, "a '123", {1, 2})),

   ?_assertEqual({[{atom, {{1, 1}, "a", a}},
                   {expr, {{1, 1}, undefined, undefined}}],
                  {atom, {{1, 1}, {1, 2}, "a", a}},
                  []},
                 edts_parse:from_string(expr, "a \n'123", {1, 2})),

   ?_assertEqual({[{atom, {    {1, 3}, "'123", '123'}},
                   {expr, {{1, 1}, undefined, undefined}}],
                  []},
                 edts_parse:from_string(expr, "a '123", {1, 4})),

   ?_assertEqual({[{expr, {{1, 1}, undefined, undefined}}],
                  {atom, {{1, 1}, {1, 6}, "'123'", '123'}},
                  []},
                 edts_parse:from_string(expr, "'123'", {1, 6}))
  ].


binary_expr_test_() ->
  [?_assertEqual({[{binary, {{1, 1}, "<<", undefined}},
                   {expr, {{1, 1}, undefined, undefined}}],
                  {'<<', {{1, 1}, {1, 3}, "<<", undefined}},
                  [{string, {{1, 3}, {1, 8}, "\"123\"", "123"}},
                   {'>>', {{1, 8}, {1, 10}, ">>", undefined}}]},
                 edts_parse:from_string(expr, "<<\"123\">>", {1, 2})),

   ?_assertEqual({[{string, {{1, 3}, "\"123\"", "123"}},
                   {binary, {{1, 1}, "<<", undefined}},
                   {expr, {{1, 1}, undefined, undefined}}],
                  {string, {{1, 3}, {1, 8}, "\"123\"", "123"}},
                  [{'>>', {{1, 8}, {1, 10}, ">>", undefined}}]},
                 edts_parse:from_string(expr, "<<\"123\">>", {1, 4})),

   ?_assertEqual({[{binary, {{1, 1}, "<<", undefined}},
                   {expr, {{1, 1}, undefined, undefined}}],
                  {string, {{1, 3}, {1, 8}, "\"123\"", "123"}},
                  [{'>>', {{1, 8}, {1, 10}, ">>", undefined}}]},
                 edts_parse:from_string(expr, "<<\"123\">>", {1, 8})),

   ?_assertEqual({[{bin_size, {{1, 8}, ":", undefined}},
                   {binary, {{1,1}, "<<", undefined}},
                   {expr, {{1,1}, undefined, undefined}}],
                  {':', {{1, 8}, {1, 9}, ":", undefined}},
                  [{integer, {{1,9}, {1,12}, "128", 128}},
                   {'>>', {{1,12}, {1,14}, ">>", undefined}}]},
                 edts_parse:from_string(expr, "<<\"123\":128>>", {1, 9})),

   ?_assertEqual({[{integer, {{1,9}, "128", 128}},
                   {bin_size, {{1, 8}, ":", undefined}},
                   {binary, {{1,1}, "<<", undefined}},
                   {expr, {{1,1}, undefined, undefined}}],
                  {integer, {{1,9}, {1,12}, "128", 128}},
                  [{'>>', {{1,12}, {1,14}, ">>", undefined}}]},
                 edts_parse:from_string(expr,
                                        "<<\"123\":128>>",
                                        {1, 10})),

   ?_assertEqual({[{bin_type, {{1, 12}, "/", undefined}},
                   {binary, {{1,1}, "<<", undefined}},
                   {expr, {{1,1}, undefined, undefined}}],
                  {'/', {{1, 12}, {1, 13}, "/", undefined}},
                  [{atom, {{1, 13}, {1, 19}, "signed", signed}},
                   {'-', {{1, 19}, {1, 20}, "-", undefined}},
                   {atom, {{1, 20}, {1, 24}, "bits", bits}},
                   {'>>', {{1,24}, {1,26}, ">>", undefined}}]},
                 edts_parse:from_string(expr,
                                        "<<\"123\":128/signed-bits>>",
                                        {1, 13})),

   ?_assertEqual({[{atom, {{1, 13}, "signed", signed}},
                   {bin_type, {{1, 12}, "/", undefined}},
                   {binary, {{1,1}, "<<", undefined}},
                   {expr, {{1,1}, undefined, undefined}}],
                  {atom, {{1, 13}, {1, 19}, "signed", signed}},
                  [{'-', {{1, 19}, {1, 20}, "-", undefined}},
                   {atom, {{1, 20}, {1, 24}, "bits", bits}},
                   {'>>', {{1,24}, {1,26}, ">>", undefined}}]},
                 edts_parse:from_string(expr,
                                        "<<\"123\":128/signed-bits>>",
                                        {1, 14})),

   ?_assertEqual({[{bin_type, {{1, 19}, "-", undefined}},
                   {binary, {{1,1}, "<<", undefined}},
                   {expr, {{1,1}, undefined, undefined}}],
                  {'-', {{1, 19}, {1, 20}, "-", undefined}},
                  [{atom, {{1, 20}, {1, 24}, "bits", bits}},
                   {'>>', {{1,24}, {1,26}, ">>", undefined}}]},
                 edts_parse:from_string(expr,
                                        "<<\"123\":128/signed-bits>>",
                                        {1,20})),

   ?_assertEqual({[{atom, {{1, 20}, "bits", bits}},
                   {bin_type, {{1, 19}, "-", undefined}},
                   {binary, {{1,1}, "<<", undefined}},
                   {expr, {{1,1}, undefined, undefined}}],
                  {atom, {{1, 20}, {1, 24}, "bits", bits}},
                  [{'>>', {{1,24}, {1,26}, ">>", undefined}}]},
                 edts_parse:from_string(expr,
                                        "<<\"123\":128/signed-bits>>",
                                        {1, 21})),

   ?_assertEqual({[{integer, {{1, 10}, "128", 128}},
                   {expr, {{1, 9}, "(", '('}},
                   {bin_size, {{1, 8}, ":", undefined}},
                   {binary, {{1,1}, "<<", undefined}},
                   {expr, {{1,1}, undefined, undefined}}],
                  {integer, {{1, 10}, {1, 13}, "128", 128}},
                  [{'+', {{1, 14}, {1, 15}, "+", undefined}},
                   {integer, {{1, 16}, {1, 17}, "4", 4}},
                   {')', {{1, 17}, {1, 18}, ")", undefined}},
                   {'/', {{1, 18}, {1, 19}, "/", undefined}},
                   {atom, {{1, 19}, {1, 25}, "signed", signed}},
                   {'-', {{1, 25}, {1, 26}, "-", undefined}},
                   {atom, {{1, 26}, {1, 30}, "bits", bits}},
                   {'>>', {{1,30}, {1,32}, ">>", undefined}}]},
                 edts_parse:from_string(expr,
                                        "<<\"123\":(128 + 4)/signed-bits>>",
                                        {1, 13})),

   ?_assertEqual({[{integer, {{1, 5}, "2", 2}},
                   {binary, {{1,1}, "<<", undefined}},
                   {expr, {{1,1}, undefined, undefined}}],
                  {integer, {{1, 5}, {1, 6}, "2", 2}},
                  [{'>>', {{1,6}, {1,8}, ">>", undefined}}]},
                 edts_parse:from_string(expr,
                                        "<<1,2>>",
                                        {1, 6})),

   ?_assertEqual({[{binary, {{1,1}, "<<", undefined}},
                   {expr, {{1,1}, undefined, undefined}}],
                  {'>>', {{1, 6}, {1, 8}, ">>", undefined}},
                  []},
                 edts_parse:from_string(expr,
                                        "<<1,2>>",
                                        {1, 7})),

   ?_assertEqual({[{integer, {{1, 19}, "2", 2}},
                   {binary, {{1,1}, "<<", undefined}},
                   {expr, {{1,1}, undefined, undefined}}],
                  {integer, {{1, 19}, {1, 20}, "2", 2}},
                  [{':', {    {1, 20}, {1, 21}, ":",     undefined}},
                   {integer, {{1, 21}, {1, 22}, "2",      2}},
                   {'/', {    {1, 22}, {1, 23}, "/",      undefined}},
                   {atom, {   {1, 23}, {1, 29}, "signed", signed}},
                   {'-', {    {1, 29}, {1, 30}, "-",      undefined}},
                   {atom, {   {1, 30}, {1, 34}, "bits",   bits}},
                   {'>>', {   {1, 34}, {1, 36}, ">>",     undefined}}]},
                 edts_parse:from_string(expr,
                                        "<<1:1/signed-bits,2:2/signed-bits>>",
                                        {1, 20}))


  ].

list_expr_test_() ->
  [?_assertEqual({[{element, {{1, 2}, undefined, 1}},
                   {expr, {{1, 1}, "[", '['}},
                   {expr, {{1, 1}, undefined, undefined}}],
                  {'[', {{1, 1}, {1, 2}, "[", undefined}},
                  [{']', {{1, 2}, {1, 3}, "]", undefined}}]},
                 edts_parse:from_string(expr, "[]", {1, 2})),

   ?_assertEqual({[{expr, {{1, 1}, undefined, undefined}}],
                  {']', {{1, 2}, {1, 3}, "]", undefined}},
                  []},
                 edts_parse:from_string(expr, "[]", {1, 3})),

   ?_assertEqual({[{element, {{1, 2}, undefined, 1}},
                   {expr, {{1, 1}, "[", '['}},
                   {expr, {{1, 1}, undefined, undefined}}],
                  {'[', {{1, 1}, {1, 2}, "[", undefined}},
                  [{atom, {{1, 2}, {1, 3}, "a", a}},
                   {']', {{1, 3}, {1, 4}, "]", undefined}}]},
                 edts_parse:from_string(expr, "[a]", {1, 2})),


   ?_assertEqual({[{element, {{1, 2}, undefined, 1}},
                   {expr, {{1, 1}, "[", '['}},
                   {expr, {{1, 1}, undefined, undefined}}],
                  {'[', {{1, 1}, {1, 2}, "[", undefined}},
                  [{atom, {{1, 2}, {1, 3}, "a", a}},
                   {']', {{1, 3}, {1, 4}, "]", undefined}}]},
                 edts_parse:from_string(expr, "[a]", {1, 2})),

   ?_assertEqual({[{atom, {{1, 2}, "a", a}},
                   {element, {{1, 2}, undefined, 1}},
                   {expr, {{1, 1}, "[", '['}},
                   {expr, {{1, 1}, undefined, undefined}}],
                  {atom, {{1, 2}, {1, 3}, "a", a}},
                  [{']', {{1, 3}, {1, 4}, "]", undefined}}]},
                 edts_parse:from_string(expr, "[a]", {1, 3})),

   ?_assertEqual({[{element, {{1, 4}, undefined, 2}},
                   {expr, {{1, 1}, "[", '['}},
                   {expr, {{1, 1}, undefined, undefined}}],
                  {',', {{1, 3}, {1, 4}, ",", undefined}},
                  [{atom, {{1, 4}, {1, 5}, "b", b}},
                   {']', {{1, 5}, {1, 6}, "]", undefined}}]},
                 edts_parse:from_string(expr, "[a,b]", {1, 4})),

   ?_assertEqual({[{atom, {{1, 4}, "b", b}},
                   {element, {{1, 4}, undefined, 2}},
                   {expr, {{1, 1}, "[", '['}},
                   {expr, {{1, 1}, undefined, undefined}}],
                  {atom, {{1, 4}, {1, 5}, "b", b}},
                  [{']', {{1, 5}, {1, 6}, "]", undefined}}]},
                 edts_parse:from_string(expr, "[a,b]", {1, 5})),

   ?_assertEqual({[{element, {{1, 4}, undefined, tail}},
                   {expr, {{1, 1}, "[", '['}},
                   {expr, {{1, 1}, undefined, undefined}}],
                  {'|', {{1, 3}, {1, 4}, "|", undefined}},
                  [{atom, {{1, 4}, {1, 5}, "b", b}},
                   {']', {{1, 5}, {1, 6}, "]", undefined}}]},
                 edts_parse:from_string(expr, "[a|b]", {1, 4})),

   ?_assertEqual({[{atom, {{1, 4}, "b", b}},
                   {element, {{1, 4}, undefined, tail}},
                   {expr, {{1, 1}, "[", '['}},
                   {expr, {{1, 1}, undefined, undefined}}],
                  {atom, {{1, 4}, {1, 5}, "b", b}},
                  [{']', {{1, 5}, {1, 6}, "]", undefined}}]},
                 edts_parse:from_string(expr, "[a|b]", {1, 5})),

   ?_assertEqual({[{expr, {{1, 1}, undefined, undefined}}],
                  {']', {{1, 5}, {1, 6}, "]", undefined}},
                  []},
                 edts_parse:from_string(expr, "[a|b]", {1, 6}))

  ].

tuple_expr_test_() ->
  [
   ?_assertEqual({[{tuple, {{1, 1}, "{", '{'}},
                   {expr, {{1, 1}, undefined, undefined}}],
                  {'{', {{1, 1}, {1, 2}, "{", undefined}},
                  [{'}', {{1, 2}, {1, 3}, "}", undefined}}]},
                 edts_parse:from_string(expr, "{}", {1, 2})),

   ?_assertEqual({[{expr, {{1, 1}, undefined, undefined}}],
                  {'}', {{1, 2}, {1, 3}, "}", undefined}},
                  []},
                 edts_parse:from_string(expr, "{}", {1, 3})),

   ?_assertEqual({[{tuple, {{1, 1}, "{", '{'}},
                   {expr, {{1, 1}, undefined, undefined}}],
                  {'{', {{1, 1}, {1, 2}, "{", undefined}},
                  [{atom, {{1, 2}, {1, 3}, "a", a}},
                   {'}', {{1, 3}, {1, 4}, "}", undefined}}]},
                 edts_parse:from_string(expr, "{a}", {1, 2})),

   ?_assertEqual({[{atom, {{1, 2}, "a", a}},
                   {tuple, {{1, 1}, "{", '{'}},
                   {expr, {{1, 1}, undefined, undefined}}],
                  {atom, {{1, 2}, {1, 3}, "a", a}},
                  [{'}', {{1, 3}, {1, 4}, "}", undefined}}]},
                 edts_parse:from_string(expr, "{a}", {1, 3})),

   ?_assertEqual({[{tuple, {{1, 1}, "{", '{'}},
                   {expr, {{1, 1}, undefined, undefined}}],
                  {',', {{1, 3}, {1, 4}, ",", undefined}},
                  [{atom, {{1, 4}, {1, 5}, "b", b}},
                   {'}', {{1, 5}, {1, 6}, "}", undefined}}]},
                 edts_parse:from_string(expr, "{a,b}", {1, 4})),

   ?_assertEqual({[{atom, {{1, 4}, "b", b}},
                   {tuple, {{1, 1}, "{", '{'}},
                   {expr, {{1, 1}, undefined, undefined}}],
                  {atom, {{1, 4}, {1, 5}, "b", b}},
                  [{'}', {{1, 5}, {1, 6}, "}", undefined}}]},
                 edts_parse:from_string(expr, "{a,b}", {1, 5}))
  ].



record_expr_test_() ->
  [
   ?_assertEqual({[{'#', {{1, 1}, "#", undefined}},
                   {expr, {{1, 1}, undefined, undefined}}],
                  {'#', {{1, 1}, {1, 2}, "#", undefined}},
                  [{atom, {{1, 2}, {1, 5}, "foo", foo}},
                   {'{', {{1, 5}, {1, 6}, "{", undefined}},
                   {'}', {{1, 6}, {1, 7}, "}", undefined}}]},
                edts_parse:from_string(expr, "#foo{}", {1, 2})),

   ?_assertEqual({[{record, {{1, 1}, "#foo", foo}},
                   {expr, {{1, 1}, undefined, undefined}}],
                  {atom, {{1, 2}, {1, 5}, "foo", foo}},
                  [{'{', {{1, 5}, {1, 6}, "{", undefined}},
                   {'}', {{1, 6}, {1, 7}, "}", undefined}}]},
                 edts_parse:from_string(expr, "#foo{}", {1, 3})),

   ?_assertEqual({[{field_name, {{1, 6}, undefined, undefined}},
                   {field, {{1, 6}, undefined, 1}},
                   {record, {{1, 1}, "#foo", foo}},
                   {expr, {{1, 1}, undefined, undefined}}],
                  {'{', {{1, 5}, {1, 6}, "{", undefined}},
                  [{'}', {{1, 6}, {1, 7}, "}", undefined}}]},
                 edts_parse:from_string(expr, "#foo{}", {1, 6})),

   ?_assertEqual({[{atom, {{1, 6}, "a", a}},
                   {field_name, {{1, 6}, undefined, undefined}},
                   {field, {{1, 6}, undefined, 1}},
                   {record, {{1, 1}, "#foo", foo}},
                   {expr, {{1, 1}, undefined, undefined}}],
                  {atom, {{1, 6}, {1, 7}, "a", a}},
                  [{'=', {{1, 7}, {1, 8}, "=", undefined}},
                   {atom, {{1, 8}, {1, 9}, "b", b}},
                   {'}', {{1, 9}, {1, 10}, "}", undefined}}]},
                 edts_parse:from_string(expr, "#foo{a=b}", {1, 7})),

   ?_assertEqual({[{field_value, {{1, 8}, undefined, undefined}},
                   {field, {{1, 6}, undefined, 1}},
                   {record, {{1, 1}, "#foo", foo}},
                   {expr, {{1, 1}, undefined, undefined}}],
                  {'=', {{1, 7}, {1, 8}, "=", undefined}},
                  [{atom, {{1, 8}, {1, 9}, "b", b}},
                   {'}', {{1, 9}, {1, 10}, "}", undefined}}]},
                 edts_parse:from_string(expr, "#foo{a=b}", {1, 8}))

   %% ?_assertEqual({[{field_name, {{1, 10}, undefined, undefined}},
   %%                 {field, {{1, 6}, undefined, 2}},
   %%                 {record, {{1, 1}, "#foo", foo}},
   %%                 {expr, {{1, 1}, undefined, undefined}}],
   %%               [{atom, {{1, 8}, {1, 9}, "c", c}},
   %%                {'=', {{1, 7}, {1, 8}, "=", undefined}},
   %%                {atom, {{1, 8}, {1, 9}, "d", d}},
   %%                {'}', {{1, 9}, {1, 10}, "}", undefined}}]},
   %%              edts_parse:from_string(expr, "#foo{a=b,c=d}", {1, 10}))
  ].


basic_expr_test_() ->
  [
   ?_assertEqual({[{expr, {{1, 1}, undefined, undefined}}],
                  undefined,
                  [{integer,{{1,1},{1,2}, "1", 1}},
                   {'+',{{1,3},{1,4}, "+", undefined}},
                   {integer,{{1,5},{1,6}, "1", 1}},
                   {',',{{1,6},{1,7}, ",", undefined}},
                   {integer,{{1,8},{1,9}, "2", 2}}]},
                 edts_parse:from_string(expr, "1 + 1, 2", {1, 1})),

   ?_assertEqual({[{integer, {{1, 1}, "1", 1}},
                   {expr, {{1, 1}, undefined, undefined}}],
                  {integer,{{1,1},{1,2}, "1", 1}},
                  [{'+',{{1,3},{1,4}, "+", undefined}},
                   {integer,{{1,5},{1,6}, "1", 1}},
                   {',',{{1,6},{1,7}, ",", undefined}},
                   {integer,{{1,8},{1,9}, "2", 2}}]},
                 edts_parse:from_string(expr, "1 + 1, 2", {1, 2})),

   ?_assertMatch({[{expr, {{1, 1}, undefined, undefined}}],
                  {integer,{{1,1},{1,2}, "1", 1}},
                  [{'+',{{1,3},{1,4}, "+", undefined}},
                   {integer,{{1,5},{1,6}, "1", 1}},
                   {',',{{1,6},{1,7}, ",", undefined}},
                   {integer,{{1,8},{1,9}, "2", 2}}]},
                 edts_parse:from_string(expr, "1 + 1, 2", {1, 3})),

   ?_assertEqual({[{'+', {{1, 3}, "+", undefined}},
                   {expr, {{1, 1}, undefined, undefined}}],
                  {'+',{{1,3},{1,4}, "+", undefined}},
                  [{integer,{{1,5},{1,6}, "1", 1}},
                   {',',{{1,6},{1,7}, ",", undefined}},
                   {integer,{{1,8},{1,9}, "2", 2}}]},
                 edts_parse:from_string(expr, "1 + 1, 2", {1, 4})),

   ?_assertEqual({[{expr, {{1, 1}, undefined, undefined}}],
                  {'+',{{1,3},{1,4}, "+", undefined}},
                  [{integer,{{1,5},{1,6}, "1", 1}},
                   {',',{{1,6},{1,7}, ",", undefined}},
                   {integer,{{1,8},{1,9}, "2", 2}}]},
                 edts_parse:from_string(expr, "1 + 1, 2", {1, 5})),

   ?_assertEqual({[{integer, {{1, 5}, "1", 1}},
                   {expr, {{1, 1}, undefined, undefined}}],
                  {integer,{{1,5},{1,6}, "1", 1}},
                  [{',',{{1,6},{1,7}, ",", undefined}},
                   {integer,{{1,8},{1,9}, "2", 2}}]},
                 edts_parse:from_string(expr, "1 + 1, 2", {1, 6})),

   ?_assertEqual({error,
                  {end_of_expr,
                   {',', {{1,6}, {1,7}, ",", undefined}}}},
                 edts_parse:from_string(expr, "1 + 1, 2", {1, 7})),

   ?_assertEqual({error,
                  {end_of_expr,
                   {dot, {{1,6}, {1,8}, ". ", undefined}}}},
                 edts_parse:from_string(expr, "1 + 1. 2", {1, 7})),

   ?_assertEqual({error,
                  {end_of_expr,
                   {',', {{1,1}, {1,2}, ",", undefined}}}},
                 edts_parse:from_string(expr, ", 2", {1, 7})),

   ?_assertEqual({[{expr, {{1, 1}, undefined, undefined}}],
                  {integer, {{1, 1}, {1, 2}, "2", 2}},
                  []},
                 edts_parse:from_string(expr, "2", {1, 3}))
  ].

parenthesis_subexpr_test_() ->
  [?_assertEqual({[{expr, {{1, 1}, "(", '('}},
                   {expr, {{1, 1}, undefined, undefined}}],
                  {'(', {{1, 1}, {1, 2}, "(", undefined}},
                 [{integer, { {1, 2}, {1,3}, "1", 1}},
                   {'+', {    {1, 4}, {1,5}, "+", undefined}},
                   {integer, {{1, 6}, {1,7}, "2", 2}},
                   {')', {    {1, 7}, {1,8}, ")", undefined}}]},
                edts_parse:from_string(expr, "(1 + 2)", {1, 2})),

   ?_assertEqual({[{expr, {{1, 1}, undefined, undefined}}],
                  {')', {{1, 7}, {1, 8}, ")", undefined}},
                  [{'+', {    {1, 9}, {1, 10}, "+", undefined}},
                   {integer, {{1, 11}, {1, 12}, "1", 1}}]},
                 edts_parse:from_string(expr, "(1 + 2) + 1", {1, 8}))
  ].


%%%_* Test Helpers =============================================================

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
