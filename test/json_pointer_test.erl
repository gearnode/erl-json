%% Copyright (c) 2020 Nicolas Martyanoff <khaelin@gmail.com>.
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
%% REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
%% AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
%% INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
%% LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
%% OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
%% PERFORMANCE OF THIS SOFTWARE.

-module(json_pointer_test).

-include_lib("eunit/include/eunit.hrl").

parse_test_() ->
  Parse = fun json_pointer:parse/1,
  [?_assertEqual({ok, []},
                 Parse(<<"">>)),
   ?_assertEqual({ok, [<<"foo">>]},
                 Parse(<<"/foo">>)),
   ?_assertEqual({ok, [<<"foo">>, <<"bar">>]},
                 Parse(<<"/foo/bar">>)),
   ?_assertEqual({ok, [<<"a">>, <<"b">>, <<"c">>]},
                 Parse(<<"/a/b/c">>)),
   ?_assertEqual({ok, [<<"xy">>, <<"">>, <<"z">>, <<"">>, <<"">>]},
                 Parse(<<"/xy//z//">>)),
   ?_assertEqual({ok, [<<"foo/bar">>, <<"~hello">>]},
                 Parse(<<"/foo~1bar/~0hello">>)),
   ?_assertEqual({ok, [<<"~1">>, <<"/0">>]},
                 Parse(<<"/~01/~10">>)),
   ?_assertEqual({error, invalid_format},
                 Parse(<<"foo">>)),
   ?_assertEqual({error, {invalid_escape_sequence, <<"~2">>}},
                 Parse(<<"/~2">>)),
   ?_assertEqual({error, {invalid_escape_sequence, <<"~~">>}},
                 Parse(<<"/~~1">>)),
   ?_assertEqual({error, truncated_escape_sequence},
                 Parse(<<"/foo/~">>)),
   ?_assertEqual({error, truncated_escape_sequence},
                 Parse(<<"/~/foo">>))].

serialize_test_() ->
  Serialize = fun json_pointer:serialize/1,
  [?_assertEqual(<<"">>,
                 Serialize([])),
   ?_assertEqual(<<"/a">>,
                 Serialize([<<"a">>])),
   ?_assertEqual(<<"/a/b/c">>,
                 Serialize([<<"a">>, <<"b">>, <<"c">>])),
   ?_assertEqual(<<"//foo//">>,
                 Serialize([<<"">>, <<"foo">>, <<"">>, <<"">>])),
   ?_assertEqual(<<"/a~1b/~0c">>,
                 Serialize([<<"a/b">>, <<"~c">>]))].

eval_test_() ->
  Eval = fun json_pointer:eval/2,
  [?_assertEqual({error, invalid_format},
                 Eval(<<"invalid">>, #{})),
   ?_assertEqual({ok, true},
                 Eval(<<"">>, true)),
   ?_assertEqual({error, {invalid_pointer, [<<"a">>], true}},
                 Eval(<<"/a">>, true)),
   ?_assertEqual({error, {invalid_pointer, [<<"a">>], #{}}},
                 Eval(<<"/a">>, #{})),
   ?_assertEqual({ok, 42},
                 Eval(<<"/a">>, #{<<"a">> => 42})),
   ?_assertEqual({error, {invalid_array_index, <<"a">>}},
                 Eval(<<"/a">>, [])),
   ?_assertEqual({error, {invalid_array_index, -3}},
                 Eval(<<"/-3">>, [])),
   ?_assertEqual({error, {invalid_pointer, [<<"1">>], []}},
                 Eval(<<"/1">>, [])),
   ?_assertEqual({error, {invalid_pointer, [<<"-">>], []}},
                 Eval(<<"/-">>, [])),
   ?_assertEqual({ok, false},
                 Eval(<<"/1">>, [true, false])),
   ?_assertEqual({error, {invalid_pointer, [<<"c">>], #{<<"b">> => []}}},
                 Eval(<<"/a/c">>, #{<<"a">> => #{<<"b">> => []}})),
   ?_assertEqual({ok, []},
                 Eval(<<"/a/5">>, #{<<"a">> => #{<<"5">> => []}}))].

eval_rfc_test_() ->
  %% See RFC 6901 5.
  Value = #{<<"foo">> => [<<"bar">>, <<"baz">>],
            <<"">> => 0,
            <<"a/b">> => 1,
            <<"c%d">> => 2,
            <<"e^f">> => 3,
            <<"g|h">> => 4,
            <<"i\\j">> => 5,
            <<"k\"l">> => 6,
            <<" ">> => 7,
            <<"m~n">> => 8},
  Eval = fun (Pointer) -> json_pointer:eval(Pointer, Value) end,
  [?_assertEqual({ok, Value},
                 Eval(<<"">>)),
   ?_assertEqual({ok, [<<"bar">>, <<"baz">>]},
                 Eval(<<"/foo">>)),
   ?_assertEqual({ok, <<"bar">>},
                 Eval(<<"/foo/0">>)),
   ?_assertEqual({ok, 0},
                 Eval(<<"/">>)),
   ?_assertEqual({ok, 1},
                 Eval(<<"/a~1b">>)),
   ?_assertEqual({ok, 2},
                 Eval(<<"/c%d">>)),
   ?_assertEqual({ok, 3},
                 Eval(<<"/e^f">>)),
   ?_assertEqual({ok, 4},
                 Eval(<<"/g|h">>)),
   ?_assertEqual({ok, 5},
                 Eval(<<"/i\\j">>)),
   ?_assertEqual({ok, 6},
                 Eval(<<"/k\"l">>)),
   ?_assertEqual({ok, 7},
                 Eval(<<"/ ">>)),
   ?_assertEqual({ok, 8},
                 Eval(<<"/m~0n">>))].
