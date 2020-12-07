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

-module(json_serializer_test).

-include_lib("eunit/include/eunit.hrl").

serialize_null_test_() ->
  [?_assertEqual(<<"null">>,
                 serialize(null))].

serialize_boolean_test_() ->
  [?_assertEqual(<<"true">>,
                 serialize(true)),
   ?_assertEqual(<<"false">>,
                 serialize(false))].

serialize_numbers_test_() ->
  [?_assertEqual(<<"0">>,
                 serialize(0)),
   ?_assertEqual(<<"-1">>,
                 serialize(-1)),
   ?_assertEqual(<<"18446744073709551615">>,
                 serialize(18446744073709551615)),
   ?_assertEqual(<<"-18446744073709551616">>,
                 serialize(-18446744073709551616)),
   ?_assertEqual(<<"0.0">>,
                 serialize(0.0)),
   ?_assertEqual(<<"-3.25">>,
                 serialize(-3.25)),
   ?_assertEqual(<<"-123.5">>,
                 serialize(-123.5)),
   ?_assertEqual(<<"-0.00000000000000001">>,
                 serialize(-0.00000000000000001)),
   ?_assertEqual(<<"-0.00000000123456">>,
                 serialize(-123.456e-11))].

serialize_strings_test_() ->
  [?_assertEqual(<<"\"\"">>,
                 serialize(<<"">>)),
   ?_assertEqual(<<"\"hello\"">>,
                 serialize(<<"hello">>)),
   ?_assertEqual(<<"\" abc def \"">>,
                 serialize(<<" abc def ">>)),
   ?_assertEqual(<<"\"élément\""/utf8>>,
                 serialize(<<"élément"/utf8>>)),
   ?_assertEqual(<<"\"\\\"foo\\\"end\"">>,
                 serialize(<<"\"foo\"end">>)),
   ?_assertEqual(<<"\"\\/\\\\\"">>,
                 serialize(<<"/\\">>)),
   ?_assertEqual(<<"\"\\b\\f\\n\\r\\t\"">>,
                 serialize(<<"\b\f\n\r\t">>)),
   ?_assertEqual(<<"\"\\u0000\\u0001\\u001f\"">>,
                 serialize(<<0, 1, 31>>)),
   ?_assertEqual(<<"\"\\\\\"">>,
                 serialize(<<"\\">>)),
   ?_assertEqual(<<"\"foo 𝄞 bar\""/utf8>>,
                 serialize(<<"foo 𝄞 bar"/utf8>>))].

serialize_arrays_test_() ->
  [?_assertEqual(<<"[]">>,
                 serialize([])),
   ?_assertEqual(<<"[1,2,3]">>,
                 serialize([1, 2, 3])),
   ?_assertEqual(<<"[[],[1],[2,3]]">>,
                 serialize([[], [1], [2, 3]]))].

serialize_objects_test_() ->
  [?_assertEqual(<<"{}">>,
                 serialize(#{})),
   ?_assertEqual(<<"{\"a\":1}">>,
                 serialize(#{<<"a">> => 1})),
   ?_assertEqual(<<"{\"a\":1,\"bcd\":[2,3]}">>,
                 serialize(#{<<"a">> => 1, <<"bcd">> => [2, 3]})),
   ?_assertEqual(<<"{\"a\":{},\"b\":{\"c\":[]}}">>,
                 serialize(#{<<"a">> => #{}, <<"b">> => #{<<"c">> => []}})),
   ?_assertEqual(<<"{\"a\":1,\"fooBar\":2}">>,
                 serialize(#{a => 1, 'fooBar' => 2})),
   ?_assertEqual(<<"{\"a\":1,\"fooBar\":2}">>,
                 serialize(#{"a" => 1, "fooBar" => 2}))].

serialize_invalid_test_() ->
  [?_assertError({invalid_value, {}},
                 serialize({})),
   ?_assertError({invalid_value, foo},
                 serialize([foo]))].

serializers_data_test_() ->
  [?_assertEqual(<<"42">>, serialize({data, <<"42">>})),
   ?_assertEqual(<<"[1,2,3]">>, serialize({data, <<"[1,2,3]">>}))].

serializers_date_test_() ->
  [?_assertEqual(<<"\"2020-10-21\"">>, serialize({date, {2020, 10, 21}}))].

serializers_time_test_() ->
  [?_assertEqual(<<"\"21:16:35\"">>, serialize({time, {21, 16, 35}}))].

serializers_datetime_test_() ->
  [?_assertEqual(<<"\"2020-10-21T21:16:35Z\"">>,
                 serialize({datetime, {{2020, 10, 21}, {21, 16, 35}}}))].

-spec serialize(json:value()) -> iodata().
serialize(Data) ->
  serialize(Data, #{}).

-spec serialize(json:value() , json:serialization_options()) -> iodata().
serialize(Data, Options) ->
  Options2 = maps:merge(Options, #{return_binary => true}),
  json_serializer:serialize(Data, Options2).
