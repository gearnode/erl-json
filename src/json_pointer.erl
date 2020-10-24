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
         parse/1, serialize/1, find/2, insert/3, replace/3]).

-export_type([pointer/0, reference_token/0,
              error_reason/0,
              parsing_error_reason/0, evaluation_error_reason/0]).

-type pointer() :: [reference_token()].
-type reference_token() :: binary().

-type error_reason() :: parsing_error_reason() | evaluation_error_reason().

-type parsing_error_reason() :: invalid_format
                              | truncated_escape_sequence
                              | {invalid_escape_sequence, binary()}.

-type evaluation_error_reason() :: invalid_pointer
                                 | {invalid_array_index, reference_token()}.

-type update_fun() ::
        fun(({root, json:value()} |
             {object, Parent :: json:value(), json:key()} |
             {array, Parent :: json:value(), non_neg_integer()} |
             {array_end, Parent :: json:value()}) ->
               json:value()).

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

-spec parse(binary()) -> {ok, pointer()} | {error, parsing_error_reason()}.
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

-spec find(binary() | pointer(), json:value())
           -> {ok, json:value()} | {error, error_reason()}.
find(PointerString, Value) when is_binary(PointerString) ->
  case parse(PointerString) of
    {ok, Pointer} ->
      find(Pointer, Value);
    {error, Reason} ->
      {error, Reason}
  end;
find([], Value) ->
  {ok, Value};
find([Token | Tokens], Value) when is_map(Value) ->
  case maps:find(Token, Value) of
    {ok, Child} ->
      find(Tokens, Child);
    error ->
      {error, invalid_pointer}
  end;
find([<<"-">> | _], Value) when is_list(Value) ->
  {error, invalid_pointer};
find([Token | Tokens], Value) when is_list(Value) ->
  try
    binary_to_integer(Token)
  of
    I when I >= 0, I < length(Value) ->
      find(Tokens, lists:nth(I+1, Value));
    _ ->
      {error, invalid_pointer}
  catch
    error:badarg ->
      {error, {invalid_array_index, Token}}
  end;
find(_Pointer, _Value) ->
  {error, invalid_pointer}.

-spec update(binary() | pointer(), json:value(), update_fun())
            -> {ok, json:value()} | {error, error_reason()}.
update(PointerString, Value, F) when is_binary(PointerString) ->
  case parse(PointerString) of
    {ok, Pointer} ->
      update(Pointer, Value, F);
    {error, Reason} ->
      {error, Reason}
  end;
update(Pointer, Value, F) ->
  try
    {ok, do_update(Pointer, Value, F)}
  catch
    throw:{error, Reason} ->
      {error, Reason}
  end.

-spec do_update(pointer(), json:value(), update_fun()) -> json:value().
do_update([], Value, F) ->
  F({root, Value});
do_update([Token], Value, F) when is_map(Value) ->
  F({object, Value, Token});
do_update([Token | Tokens], Value, F) when is_map(Value) ->
  case maps:find(Token, Value) of
    {ok, Child} ->
      Value#{Token => do_update(Tokens, Child, F)};
    error ->
      throw({error, invalid_pointer})
  end;
do_update([<<"-">> | []], Value, F) when is_list(Value) ->
  F({array_end, Value});
do_update([<<"-">> | _], Value, _F) when is_list(Value) ->
  throw({error, invalid_pointer});
do_update([Token | Tokens], Value, F) when is_list(Value) ->
  try
    binary_to_integer(Token)
  of
    I when I >= 0, I < length(Value) ->
      case Tokens of
        [] ->
          F({array, Value, I});
        _ ->
          NewValue = do_update(Tokens, lists:nth(I+1, Value), F),
          {Before, [_ | After]} = lists:split(I, Value),
          Before ++ [NewValue | After]
      end;
    I when I == length(Value) ->
      case Tokens of
        [] ->
          F({array_end, Value});
        _ ->
          throw({error, invalid_pointer})
      end;
    _ ->
      throw({error, invalid_pointer})
  catch
    error:badarg ->
      throw({error, {invalid_array_index, Token}})
  end;
do_update(_Pointer, _Value, _F) ->
  throw({error, invalid_pointer}).

-spec insert(pointer(), json:value(), NewValue :: json:value()) ->
        json:value().
insert(Pointer, Value, NewValue) ->
  F = fun
        ({root, _}) ->
          NewValue;
        ({object, Parent, Key}) ->
          Parent#{Key => NewValue};
        ({array, Parent, I}) ->
          {Before, After} = lists:split(I, Parent),
          Before ++ [NewValue] ++ After;
        ({array_end, Parent}) ->
          Parent ++ [NewValue]
      end,
  update(Pointer, Value, F).

-spec replace(pointer(), json:value(), NewValue :: json:value()) ->
        json:value().
replace(Pointer, Value, NewValue) ->
  F = fun
        ({root, _}) ->
          NewValue;
        ({object, Parent, Key}) ->
          Parent#{Key => NewValue};
        ({array, Parent, I}) ->
          {Before, [_ | After]} = lists:split(I, Parent),
          Before ++ [NewValue | After];
        ({array_end, _}) ->
          throw({error, invalid_pointer})
      end,
  update(Pointer, Value, F).
