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

-module(json).

-export([parse/1, parse/2, serialize/1, serialize/2]).

-export_type([value/0,
              error/0, error_reason/0,
              position/0,
              parsing_options/0, duplicate_key_handling/0,
              serialization_options/0]).

-type value() :: null
               | true | false
               | number()
               | binary()
               | [value()]
               | #{binary() := value()}.

-type error() :: #{reason => term(),
                   position => position()}.

-type error_reason() :: no_value
                      | depth_limit_reached
                      | {unexpected_trailing_data, binary()}
                      | {unexpected_character, integer()}
                      | invalid_element
                      | invalid_number
                      | invalid_string
                      | truncated_string
                      | invalid_escape_sequence
                      | truncated_escape_sequence
                      | truncated_utf16_surrogate_pair
                      | invalid_array
                      | truncated_array
                      | invalid_object
                      | truncated_object
                      | {invalid_key, value()}
                      | {duplicate_key, binary()}.

-type position() :: {Line :: pos_integer(), Column :: pos_integer()}.

-type parsing_options() :: #{depth_limit => non_neg_integer(),
                             duplicate_key_handling =>
                               duplicate_key_handling()}.

-type duplicate_key_handling() :: first | last | error.

-type serialization_options() :: #{return_binary => boolean()}.

-spec parse(binary()) -> {ok, value()} | {error, term()}.
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
