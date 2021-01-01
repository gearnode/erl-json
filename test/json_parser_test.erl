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

-module(json_parser_test).

-include_lib("eunit/include/eunit.hrl").

parse_invalid_test_() ->
  [?_assertEqual({error, #{position => {1, 1},
                           reason => no_value}},
                 parse(<<"">>)),
   ?_assertEqual({error, #{position => {1, 4},
                           reason => no_value}},
                 parse(<<"   ">>)),
   ?_assertEqual({error, #{position => {1, 1},
                           reason => invalid_element}},
                 parse(<<"foo">>)),
   ?_assertEqual({error, #{position => {1, 1},
                           reason => invalid_element}},
                 parse(<<"nul">>)),
   ?_assertEqual({error, #{position => {1, 3},
                           reason => {unexpected_trailing_data, <<"foo">>}}},
                 parse(<<"42foo">>)),
   ?_assertEqual({error, #{position => {1, 6},
                           reason => {unexpected_trailing_data, <<"\"">>}}},
                 parse(<<"\"foo\"\"">>)),
   ?_assertEqual({error, #{position => {1, 6},
                           reason => {unexpected_trailing_data, <<".">>}}},
                 parse(<<"true .">>))].

parse_null_test_() ->
  [?_assertEqual({ok, null},
                 parse(<<"null">>)),
   ?_assertEqual({ok, null},
                 parse(<<" \nnull\n ">>))].

parse_booleans_test_() ->
  [?_assertEqual({ok, true},
                 parse(<<"true">>)),
   ?_assertEqual({ok, false},
                 parse(<<"false">>))].

parse_numbers_test_() ->
  [?_assertEqual({ok, 0},
                 parse(<<"0">>)),
   ?_assertEqual({ok, -1},
                 parse(<<"-1">>)),
   ?_assertEqual({ok, -42},
                 parse(<<"-42">>)),
   ?_assertEqual({ok, 18446744073709551615},
                 parse(<<"18446744073709551615">>)),
   ?_assertEqual({ok, -18446744073709551616},
                 parse(<<"-18446744073709551616">>)),
   ?_assertEqual({ok, 0.0},
                 parse(<<"0e0">>)),
   ?_assertEqual({ok, 0.0},
                 parse(<<"0e1">>)),
   ?_assertEqual({ok, 1000.0},
                 parse(<<"1e3">>)),
   ?_assertEqual({ok, 1000.0},
                 parse(<<"1e+3">>)),
   ?_assertEqual({ok, -100.0},
                 parse(<<"-1e2">>)),
   ?_assertEqual({ok, -0.01},
                 parse(<<"-1e-2">>)),
   ?_assertEqual({ok, 0.0},
                 parse(<<"0.0">>)),
   ?_assertEqual({ok, 0.1},
                 parse(<<"0.1">>)),
   ?_assertEqual({ok, -3.25},
                 parse(<<"-3.25">>)),
   ?_assertEqual({ok, 123.456},
                 parse(<<"123.456">>)),
   ?_assertEqual({ok, -123.0},
                 parse(<<"-123.0">>)),
   ?_assertEqual({ok, -0.00000000000000001},
                 parse(<<"-0.00000000000000001">>)),
   ?_assertEqual({ok, 0.0},
                 parse(<<"0.0e+0">>)),
   ?_assertEqual({ok, 125.0},
                 parse(<<"1.25e2">>)),
   ?_assertEqual({ok, -3000.0},
                 parse(<<"-0.3e4">>)),
   ?_assertEqual({ok, -0.00000000123456},
                 parse(<<"-123.456e-11">>))].

parse_strings_test_() ->
  [?_assertEqual({ok, <<"">>},
                 parse(<<"\"\"">>)),
   ?_assertEqual({ok, <<"hello">>},
                 parse(<<"\"hello\"">>)),
   ?_assertEqual({ok, <<" abc def ">>},
                 parse(<<"\" abc def \"">>)),
   ?_assertEqual({ok, <<"élément"/utf8>>},
                 parse(<<"\"élément\""/utf8>>)),
   ?_assertEqual({ok, <<"\"foo\"end">>},
                 parse(<<"\"\\\"foo\\\"end\"">>)),
   ?_assertEqual({ok, <<"/\\">>},
                 parse(<<"\"\\/\\\\\"">>)),
   ?_assertEqual({ok, <<"\b\f\n\r\t">>},
                 parse(<<"\"\\b\\f\\n\\r\\t\"">>)),
   ?_assertEqual({ok, <<"\\">>},
                 parse(<<"\"\\u005c\"">>)),
   ?_assertEqual({ok, <<"élément"/utf8>>},
                 parse(<<"\"\\u00E9l\\U00e9ment\"">>)),
   ?_assertEqual({ok, <<"foo 𝄞 bar"/utf8>>},
                 parse(<<"\"foo \\uD834\\uDD1E bar\"">>)),
   ?_assertEqual({error, #{position => {1, 2},
                           reason => truncated_string}},
                 parse(<<"\"">>)),
   ?_assertEqual({error, #{position => {1, 5},
                           reason => truncated_string}},
                 parse(<<"\"foo">>)),
   ?_assertEqual({error, #{position => {1, 5},
                           reason => invalid_escape_sequence}},
                 parse(<<"\"foo\\x\"">>)),
   ?_assertEqual({error, #{position => {1, 5},
                           reason => truncated_escape_sequence}},
                 parse(<<"\"foo\\">>)),
   ?_assertEqual({error, #{position => {1, 5},
                           reason => truncated_escape_sequence}},
                 parse(<<"\"foo\\u123">>)),
   ?_assertEqual({error, #{position => {1, 11},
                           reason => truncated_utf16_surrogate_pair}},
                 parse(<<"\"foo\\ud834">>)),
   ?_assertEqual({error, #{position => {1, 11},
                           reason => truncated_escape_sequence}},
                 parse(<<"\"foo\\ud834\\">>)),
   ?_assertEqual({error, #{position => {1, 11},
                           reason => truncated_escape_sequence}},
                 parse(<<"\"foo\\ud834\\u">>)),
   ?_assertEqual({error, #{position => {1, 11},
                           reason => truncated_escape_sequence}},
                 parse(<<"\"foo\\ud834\\udd1">>))].

parse_arrays_test_() ->
  [?_assertEqual({ok, []},
                 parse(<<"[]">>)),
   ?_assertEqual({ok, []},
                 parse(<<"[   ]">>)),
   ?_assertEqual({ok, [1, 2, 3]},
                 parse(<<"[ 1 ,2, 3]">>)),
   ?_assertEqual({ok, [<<"foo">>, null, true]},
                 parse(<<"[\"foo\" , null,true]">>)),
   ?_assertEqual({ok, [[]]},
                 parse(<<"[[]]">>)),
   ?_assertEqual({ok, [[[]], [[]]]},
                 parse(<<"[[ [  ]], [[] ]]">>)),
   ?_assertEqual({ok, [1, [[2, 3]], [4]]},
                 parse(<<"[1, [[2, 3]], [4]]">>)),
   ?_assertEqual({error, #{position => {1, 2},
                           reason => truncated_array}},
                 parse(<<"[">>)),
   ?_assertEqual({error, #{position => {1, 3},
                           reason => truncated_array}},
                 parse(<<"[1">>)),
   ?_assertEqual({error, #{position => {1, 4},
                           reason => truncated_array}},
                 parse(<<"[1,">>)),
   ?_assertEqual({error, #{position => {1, 6},
                           reason => truncated_array}},
                 parse(<<"[1,2,">>)),
   ?_assertEqual({error, #{position => {1, 6},
                           reason => {unexpected_character, $]}}},
                 parse(<<"[1,2,]">>)),
   ?_assertEqual({error, #{position => {1, 6},
                           reason => {unexpected_character, $,}}},
                 parse(<<"[1,2,,]">>)),
   ?_assertEqual({error, #{position => {1, 6},
                           reason => {unexpected_character, $,}}},
                 parse(<<"[1,2,,3]">>))].

parse_objects_test_() ->
  [?_assertEqual({ok, #{}},
                 parse(<<"{}">>)),
   ?_assertEqual({ok, #{<<"">> => <<"">>}},
                 parse(<<"{\"\": \"\"}">>)),
   ?_assertEqual({ok, #{<<"a">> => 1}},
                 parse(<<"{\"a\":1}">>)),
   ?_assertEqual({ok, #{<<"abc">> => 2, <<"def">> => <<"ghi">>}},
                 parse(<<"{ \"abc\"	:2 , \"def\":  \"ghi\"}">>)),
   ?_assertEqual({ok, #{<<"a">> => #{}, <<"b">> => #{<<"c">> => 1}}},
                 parse(<<"{\"a\": {}, \"b\": {\"c\": 1}}">>)),
   ?_assertEqual({ok, #{<<"a">> => 3, <<"b">> => 2}},
                 parse(<<"{\"a\": 1, \"b\": 2, \"a\": 3}">>)),
   ?_assertEqual({ok, #{<<"a">> => 1, <<"b">> => 2}},
                 parse(<<"{\"a\": 1, \"b\": 2, \"a\": 3}">>,
                            #{duplicate_key_handling => first})),
   ?_assertEqual({ok, #{<<"a">> => 3, <<"b">> => 2}},
                 parse(<<"{\"a\": 1, \"b\": 2, \"a\": 3}">>,
                            #{duplicate_key_handling => last})),
   ?_assertEqual({error, #{position => {1, 18},
                           reason => {duplicate_key, <<"a">>}}},
                 parse(<<"{\"a\": 1, \"b\": 2, \"a\": 3}">>,
                            #{duplicate_key_handling => error})),
   ?_assertEqual({error, #{position => {1, 2},
                           reason => truncated_object}},
              parse(<<"{">>)),
   ?_assertEqual({error, #{position => {1, 2},
                           reason => {unexpected_character, $:}}},
              parse(<<"{: 42}">>)),
   ?_assertEqual({error, #{position => {1, 7},
                           reason => truncated_object}},
              parse(<<"{\"foo\"">>)),
   ?_assertEqual({error, #{position => {1, 9},
                           reason => truncated_object}},
              parse(<<"{\"foo\": ">>)),
   ?_assertEqual({error, #{position => {1, 11},
                           reason => truncated_object}},
              parse(<<"{\"foo\": 42">>)),
   ?_assertEqual({error, #{position => {1, 12},
                           reason => truncated_object}},
              parse(<<"{\"foo\": 42,">>)),
   ?_assertEqual({error, #{position => {1, 7},
                           reason => {unexpected_character, $}}}},
                 parse(<<"{\"foo\"}">>)),
   ?_assertEqual({error, #{position => {1, 8},
                           reason => {unexpected_character, $,}}},
                 parse(<<"{\"foo\":,">>)),
   ?_assertEqual({error, #{position => {1, 8},
                           reason => {unexpected_character, $}}}},
                 parse(<<"{\"foo\":}">>))].

parse_depth_test_() ->
  [?_assertEqual({ok, 42},
                 parse(<<"42">>, #{depth_limit => 0})),
   ?_assertEqual({error, #{position => {1, 2},
                           reason => depth_limit_reached}},
                 parse(<<"[42]">>, #{depth_limit => 0})),
   ?_assertEqual({ok, [42]},
                 parse(<<"[42]">>, #{depth_limit => 1})),
   ?_assertEqual({error, #{position => {1, 2},
                           reason => depth_limit_reached}},
                 parse(<<"{\"a\": 1}">>, #{depth_limit => 0})),
   ?_assertEqual({ok, #{<<"a">> => 1}},
                 parse(<<"{\"a\": 1}">>, #{depth_limit => 1})),
   ?_assertEqual({error, #{position => {1, 3},
                           reason => depth_limit_reached}},
                 parse(<<"[[1, 2]]">>, #{depth_limit => 1})),
   ?_assertEqual({ok, [[1, 2]]},
                 parse(<<"[[1, 2]]">>, #{depth_limit => 2})),
   ?_assertEqual({error, #{position => {1, 8},
                           reason => depth_limit_reached}},
                 parse(<<"{\"a\": [1, 2, 3]}">>, #{depth_limit => 1})),
   ?_assertEqual({ok, #{<<"a">> => [1, 2, 3]}},
                 parse(<<"{\"a\": [1, 2, 3]}">>, #{depth_limit => 2}))].

-spec parse(binary()) -> {ok, json:value()} | {error, term()}.
parse(Data) ->
  parse(Data, #{}).

-spec parse(binary() , json:parsing_options()) ->
        {ok, json:value()} | {error, json:error()}.
parse(Data, Options) ->
  json_parser:parse(Data, Options).
