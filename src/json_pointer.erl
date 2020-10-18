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

-module(json_pointer).

-export([parent/1, child/2,
         parse/1, serialize/1, eval/2, eval_pointer/2]).

-export_type([pointer/0, reference_token/0]).

-type pointer() :: [reference_token()].
-type reference_token() :: binary().

-type error_reason() :: invalid_format
                      | truncated_escape_sequence
                      | {invalid_escape_sequence, binary()}
                      | {invalid_pointer, binary(), json:value()}
                      | {invalid_array_index, reference_token() | integer()}.

-spec parent(pointer()) -> pointer().
parent([]) ->
  error(badarg);
parent(Pointer) ->
  lists:droplast(Pointer).

-spec child(pointer(), reference_token() | [reference_token()]) ->
        pointer().
child(Pointer, Tokens) when is_list(Tokens) ->
  Pointer ++ Tokens;
child(Pointer, Token) ->
  Pointer ++ [Token].

-spec parse(binary()) -> {ok, pointer()} | {error, error_reason()}.
parse(<<>>) ->
  {ok, []};
parse(<<$/, Data/binary>>) ->
  Parts = binary:split(Data, <<"/">>, [global]),
  try
    Tokens = lists:map(fun (P) -> parse_reference_token(P, <<>>) end,
                       Parts),
    {ok, Tokens}
  catch
    throw:{error, Reason} ->
      {error, Reason}
  end;
parse(_) ->
  {error, invalid_format}.

-spec parse_reference_token(binary(), Acc :: binary()) -> reference_token().
parse_reference_token(<<>>, Acc) ->
  Acc;
parse_reference_token(<<$~, $1, Data/binary>>, Acc) ->
  parse_reference_token(Data, <<Acc/binary, $/>>);
parse_reference_token(<<$~, $0, Data/binary>>, Acc) ->
  parse_reference_token(Data, <<Acc/binary, $~>>);
parse_reference_token(<<$~, C, _/binary>>, _) ->
  throw({error, {invalid_escape_sequence, <<$~, C>>}});
parse_reference_token(<<$~>>, _) ->
  throw({error, truncated_escape_sequence});
parse_reference_token(<<C, Data/binary>>, Acc) ->
  parse_reference_token(Data, <<Acc/binary, C>>).

-spec serialize(pointer()) -> binary().
serialize([]) ->
  <<"">>;
serialize(Pointer) ->
  Tokens = lists:map(fun (T) -> serialize_reference_token(T, <<>>) end,
                     Pointer),
  Data =[$/, lists:join($/, Tokens)],
  iolist_to_binary(Data).

-spec serialize_reference_token(reference_token(), Acc :: binary()) -> binary().
serialize_reference_token(<<>>, Acc) ->
  Acc;
serialize_reference_token(<<$/, Token/binary>>, Acc) ->
  serialize_reference_token(Token, <<Acc/binary, $~, $1>>);
serialize_reference_token(<<$~, Token/binary>>, Acc) ->
  serialize_reference_token(Token, <<Acc/binary, $~, $0>>);
serialize_reference_token(<<C, Token/binary>>, Acc) ->
  serialize_reference_token(Token, <<Acc/binary, C>>).

-spec eval(binary(), json:value())
          -> {ok, json:value()} | {error, error_reason()}.
eval(Data, Value) ->
  case parse(Data) of
    {ok, Pointer} ->
      eval_pointer(Pointer, Value);
    {error, Reason} ->
      {error, Reason}
  end.

-spec eval_pointer(pointer(), json:value())
                  -> {ok, json:value()} | {error, error_reason()}.
eval_pointer([], Value) ->
  {ok, Value};
eval_pointer(Pointer = [Token | Tokens], Value) when is_map(Value) ->
  case maps:find(Token, Value) of
    {ok, Child} ->
      eval_pointer(Tokens, Child);
    error ->
      {error, {invalid_pointer, serialize(Pointer), Value}}
  end;
eval_pointer(Pointer = [<<"-">> | _], Value) when is_list(Value) ->
  {error, {invalid_pointer, serialize(Pointer), Value}};
eval_pointer(Pointer = [Token | Tokens], Value) when is_list(Value) ->
  try
    binary_to_integer(Token)
  of
    I when I >= 0, I < length(Value) ->
      eval_pointer(Tokens, lists:nth(I+1, Value));
    I when I >= 0 ->
      {error, {invalid_pointer, serialize(Pointer), Value}};
    I ->
      {error, {invalid_array_index, I}}
  catch
    error:badarg ->
      {error, {invalid_array_index, Token}}
  end;
eval_pointer(Pointer, Value) ->
  {error, {invalid_pointer, serialize(Pointer), Value}}.
