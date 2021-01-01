%% Copyright (c) 2020-2021 Nicolas Martyanoff <khaelin@gmail.com>.
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
%% SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
%% IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(json_pointer_test).

-include_lib("eunit/include/eunit.hrl").

parent_test_() ->
  Parent = fun json_pointer:parent/1,
  [?_assertEqual([], Parent([<<"a">>])),
   ?_assertEqual([<<"a">>], Parent([<<"a">>, <<"b">>])),
   ?_assertEqual([<<"a">>, <<"b">>], Parent([<<"a">>, <<"b">>, <<"c">>]))].

child_test_() ->
  Child = fun json_pointer:child/2,
  [?_assertEqual([<<"a">>], Child([], <<"a">>)),
   ?_assertEqual([<<"a">>], Child([<<"a">>], [])),
   ?_assertEqual([<<"a">>, <<"b">>], Child([], [<<"a">>, <<"b">>])),
   ?_assertEqual([<<"a">>, <<"b">>], Child([<<"a">>], <<"b">>)),
   ?_assertEqual([<<"a">>, <<"b">>, <<"c">>],
                 Child([<<"a">>], [<<"b">>, <<"c">>]))].

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

find_test_() ->
  Find = fun json_pointer:find/2,
  [?_assertEqual({error, invalid_format},
                 Find(<<"invalid">>, #{})),
   ?_assertEqual({ok, true},
                 Find(<<"">>, true)),
   ?_assertEqual({error, invalid_pointer},
                 Find(<<"/a">>, true)),
   ?_assertEqual({error, invalid_pointer},
                 Find(<<"/a">>, #{})),
   ?_assertEqual({ok, 42},
                 Find(<<"/a">>, #{<<"a">> => 42})),
   ?_assertEqual({error, {invalid_array_index, <<"a">>}},
                 Find(<<"/a">>, [])),
   ?_assertEqual({error, invalid_pointer},
                 Find(<<"/-3">>, [])),
   ?_assertEqual({error, invalid_pointer},
                 Find(<<"/1">>, [])),
   ?_assertEqual({error, invalid_pointer},
                 Find(<<"/-">>, [])),
   ?_assertEqual({ok, false},
                 Find(<<"/1">>, [true, false])),
   ?_assertEqual({error, invalid_pointer},
                 Find(<<"/a/c">>, #{<<"a">> => #{<<"b">> => []}})),
   ?_assertEqual({ok, []},
                 Find(<<"/a/5">>, #{<<"a">> => #{<<"5">> => []}}))].

find_rfc_test_() ->
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
  Find = fun (Pointer) -> json_pointer:find(Pointer, Value) end,
  [?_assertEqual({ok, Value},
                 Find(<<"">>)),
   ?_assertEqual({ok, [<<"bar">>, <<"baz">>]},
                 Find(<<"/foo">>)),
   ?_assertEqual({ok, <<"bar">>},
                 Find(<<"/foo/0">>)),
   ?_assertEqual({ok, 0},
                 Find(<<"/">>)),
   ?_assertEqual({ok, 1},
                 Find(<<"/a~1b">>)),
   ?_assertEqual({ok, 2},
                 Find(<<"/c%d">>)),
   ?_assertEqual({ok, 3},
                 Find(<<"/e^f">>)),
   ?_assertEqual({ok, 4},
                 Find(<<"/g|h">>)),
   ?_assertEqual({ok, 5},
                 Find(<<"/i\\j">>)),
   ?_assertEqual({ok, 6},
                 Find(<<"/k\"l">>)),
   ?_assertEqual({ok, 7},
                 Find(<<"/ ">>)),
   ?_assertEqual({ok, 8},
                 Find(<<"/m~0n">>))].

insert_test_() ->
  Insert = fun json_pointer:insert/3,
  [?_assertEqual({ok, true},
                 Insert(<<"">>, 42, true)),
   ?_assertEqual({ok, -1},
                 Insert(<<"">>, [1, 2, 3], -1)),
   ?_assertEqual({error, invalid_pointer},
                 Insert(<<"/a">>, 42, 43)),
   ?_assertEqual({ok, #{<<"a">> => 43}},
                 Insert(<<"/a">>, #{<<"a">> => 42}, 43)),
   ?_assertEqual({ok, #{<<"a">> => #{<<"b">> => 43}}},
                 Insert(<<"/a/b">>, #{<<"a">> => #{<<"b">> => 42}}, 43)),
   ?_assertEqual({ok, #{<<"a">> => #{<<"b">> => 43, <<"c">> => 1}}},
                 Insert(<<"/a/b">>, #{<<"a">> => #{<<"c">> => 1}}, 43)),
   ?_assertEqual({error, invalid_pointer},
                 Insert(<<"/2">>, [], 43)),
   ?_assertEqual({error, invalid_pointer},
                 Insert(<<"/1/2">>, [[], []], 43)),
   ?_assertEqual({ok, [42, 1, 2, 3]},
                 Insert(<<"/0">>, [1, 2, 3], 42)),
   ?_assertEqual({ok, [1, 42, 2, 3]},
                 Insert(<<"/1">>, [1, 2, 3], 42)),
   ?_assertEqual({ok, [1, 2, 42, 3]},
                 Insert(<<"/2">>, [1, 2, 3], 42)),
   ?_assertEqual({ok, [1, 2, 3, 42]},
                 Insert(<<"/3">>, [1, 2, 3], 42)),
   ?_assertEqual({ok, [[], [1], [1, 42, 2]]},
                 Insert(<<"/2/1">>, [[], [1], [1, 2]], 42)),
   ?_assertEqual({ok, [[42], [1], [1, 2]]},
                 Insert(<<"/0/-">>, [[], [1], [1, 2]], 42)),
   ?_assertEqual({ok, [[], [1, 42], [1, 2]]},
                 Insert(<<"/1/-">>, [[], [1], [1, 2]], 42)),
   ?_assertEqual({ok, [[], [1], [1, 2, 42]]},
                 Insert(<<"/2/-">>, [[], [1], [1, 2]], 42)),
   ?_assertEqual({error, invalid_pointer},
                 Insert(<<"/-/0">>, [[], [1], [1, 2]], 42))].

replace_test_() ->
  Replace = fun json_pointer:replace/3,
  [?_assertEqual({ok, true},
                 Replace(<<"">>, 42, true)),
   ?_assertEqual({ok, -1},
                 Replace(<<"">>, [1, 2, 3], -1)),
   ?_assertEqual({error, invalid_pointer},
                 Replace(<<"/a">>, 42, 43)),
   ?_assertEqual({ok, #{<<"a">> => 43}},
                 Replace(<<"/a">>, #{<<"a">> => 42}, 43)),
   ?_assertEqual({ok, #{<<"a">> => #{<<"b">> => 43}}},
                 Replace(<<"/a/b">>, #{<<"a">> => #{<<"b">> => 42}}, 43)),
   ?_assertEqual({ok, #{<<"a">> => #{<<"b">> => 43, <<"c">> => 1}}},
                 Replace(<<"/a/b">>, #{<<"a">> => #{<<"c">> => 1}}, 43)),
   ?_assertEqual({error, invalid_pointer},
                 Replace(<<"/2">>, [], 43)),
   ?_assertEqual({error, invalid_pointer},
                 Replace(<<"/1/2">>, [[], []], 43)),
   ?_assertEqual({ok, [42, 2, 3]},
                 Replace(<<"/0">>, [1, 2, 3], 42)),
   ?_assertEqual({ok, [1, 42, 3]},
                 Replace(<<"/1">>, [1, 2, 3], 42)),
   ?_assertEqual({ok, [1, 2, 42]},
                 Replace(<<"/2">>, [1, 2, 3], 42)),
   ?_assertEqual({error, invalid_pointer},
                 Replace(<<"/3">>, [1, 2, 3], 42)),
   ?_assertEqual({ok, [[], [1], [1, 42]]},
                 Replace(<<"/2/1">>, [[], [1], [1, 2]], 42)),
   ?_assertEqual({error, invalid_pointer},
                 Replace(<<"/0/-">>, [[], [1], [1, 2]], 42)),
   ?_assertEqual({error, invalid_pointer},
                 Replace(<<"/1/-">>, [[], [1], [1, 2]], 42)),
   ?_assertEqual({error, invalid_pointer},
                 Replace(<<"/2/-">>, [[], [1], [1, 2]], 42)),
   ?_assertEqual({error, invalid_pointer},
                 Replace(<<"/-/0">>, [[], [1], [1, 2]], 42))].

remove_test_() ->
  Remove = fun json_pointer:remove/2,
  [?_assertEqual({error, invalid_pointer},
                 Remove(<<"">>, 42)),
   ?_assertEqual({error, invalid_pointer},
                 Remove(<<"">>, [1, 2, 3])),
   ?_assertEqual({error, invalid_pointer},
                 Remove(<<"/a">>, 42)),
   ?_assertEqual({ok, #{}},
                 Remove(<<"/a">>, #{<<"a">> => 42})),
   ?_assertEqual({ok, #{<<"a">> => #{}}},
                 Remove(<<"/a/b">>, #{<<"a">> => #{<<"b">> => 42}})),
   ?_assertEqual({error, invalid_pointer},
                 Remove(<<"/a/b">>, #{<<"a">> => #{<<"c">> => 1}})),
   ?_assertEqual({error, invalid_pointer},
                 Remove(<<"/2">>, [])),
   ?_assertEqual({error, invalid_pointer},
                 Remove(<<"/1/2">>, [[], []])),
   ?_assertEqual({ok, [2, 3]},
                 Remove(<<"/0">>, [1, 2, 3])),
   ?_assertEqual({ok, [1, 3]},
                 Remove(<<"/1">>, [1, 2, 3])),
   ?_assertEqual({ok, [1, 2]},
                 Remove(<<"/2">>, [1, 2, 3])),
   ?_assertEqual({error, invalid_pointer},
                 Remove(<<"/3">>, [1, 2, 3])),
   ?_assertEqual({ok, [[], [1], [1]]},
                 Remove(<<"/2/1">>, [[], [1], [1, 2]])),
   ?_assertEqual({error, invalid_pointer},
                 Remove(<<"/0/-">>, [[], [1], [1, 2]])),
   ?_assertEqual({error, invalid_pointer},
                 Remove(<<"/1/-">>, [[], [1], [1, 2]])),
   ?_assertEqual({error, invalid_pointer},
                 Remove(<<"/2/-">>, [[], [1], [1, 2]])),
   ?_assertEqual({error, invalid_pointer},
                 Remove(<<"/-/0">>, [[], [1], [1, 2]]))].
