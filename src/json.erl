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

-module(json).

-export([parse/1, parse/2, serialize/1, serialize/2,
         default_serializers/0,
         format_error/1, format_error_reason/1]).

-export_type([value/0, array/0, object/0, key/0,
              error/0, error_reason/0,
              position/0,
              parsing_options/0, duplicate_key_handling/0,
              serialization_options/0,
              serialization_fun/0, serializers/0,
              formatter/0, character/0]).

-type value() :: null
               | boolean()
               | number()
               | binary()
               | array()
               | object()
               | {atom(), term()}.

-type array() :: [value()].
-type object() :: #{key() := value()}.

-type key() :: binary() | string() | atom().

-type error() :: #{reason := error_reason(),
                   position := position()}.

-type error_reason() ::
        no_value
      | {unexpected_trailing_data, binary()}
      | {unexpected_character, non_neg_integer}
      | invalid_element
      | truncated_string
      | {invalid_string_character, non_neg_integer()}
      | invalid_utf8_sequence
      | truncated_escape_sequence
      | truncated_utf16_surrogate_pair
      | invalid_escape_sequence
      | truncated_array
      | truncated_object
      | {invalid_key, value()}
      | {duplicate_key, binary()}
      | invalid_number.

-type position() :: {Line :: pos_integer(), Column :: pos_integer()}.

-type parsing_options() :: #{duplicate_key_handling =>
                               duplicate_key_handling()}.

-type duplicate_key_handling() :: first | last | error.

-type serialization_options() :: #{return_binary => boolean(),
                                   serializers => serializers(),
                                   indent => boolean(),
                                   indent_string => binary() | string(),
                                   formatter => formatter()}.

-type serialization_fun() ::
        fun((term()) -> {data, iodata()} | {value, json:value()}).
-type serializers() :: #{atom() := serialization_fun()}.

-spec default_serializers() -> serializers().
default_serializers() ->
  #{data => fun json_serializer:serialize_data/1,
    date => fun json_serializer:serialize_date/1,
    time => fun json_serializer:serialize_time/1,
    datetime => fun json_serializer:serialize_datetime/1}.

-type formatter() ::
        fun((json:value() | {key, binary()} | {character, character()}) ->
               {Before :: iodata(), After :: iodata()}).

-type character() :: $[ | $] | ${ | $} | $, | $" | $:.

-spec parse(binary()) -> {ok, value()} | {error, error()}.
parse(Data) ->
  parse(Data, #{}).

-spec parse(binary() , parsing_options()) ->
        {ok, value()} | {error, error()}.
parse(Data, Options) ->
  json_parser:parse(Data, Options).

-spec serialize(value()) -> iodata().
serialize(Data) ->
  serialize(Data, #{}).

-spec serialize(value(), serialization_options()) -> iodata().
serialize(Data, Options) ->
  json_serializer:serialize(Data, Options).

-spec format_error(error()) -> unicode:chardata().
format_error(#{reason := Reason, position := {Row,Col}}) ->
  io_lib:format("~b:~b: ~ts", [Row, Col, format_error_reason(Reason)]).

-spec format_error_reason(error_reason()) -> unicode:chardata().
format_error_reason(no_value) ->
  "no value";
format_error_reason({unexpected_trailing_data, _}) ->
  "unexpected trailing data after value";
format_error_reason({unexpected_character, _}) ->
  "unexpected character"; % TODO char
format_error_reason(invalid_element) ->
  "invalid element";
format_error_reason(truncated_string) ->
  "truncated string";
format_error_reason({invalid_string_character, _}) ->
  "invalid string character"; % TODO char
format_error_reason(invalid_utf8_sequence) ->
  "invalid utf-8 sequence";
format_error_reason(truncated_escape_sequence) ->
  "truncated escape sequence";
format_error_reason(truncated_utf16_surrogate_pair) ->
  "truncate utf-16 surrogate pair";
format_error_reason(invalid_escape_sequence) ->
  "invalid escape sequence";
format_error_reason(truncated_array) ->
  "truncated array";
format_error_reason(truncated_object) ->
  "truncated object";
format_error_reason({invalid_key, Key}) ->
  io_lib:format("invalid object key ~ts", [json:serialize(Key)]);
format_error_reason({duplicate_key, Key}) ->
  io_lib:format("duplicate object key \"~ts\"", [Key]);
format_error_reason(Reason) ->
  io_lib:format("~0tp", [Reason]).
