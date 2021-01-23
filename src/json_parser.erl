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

-module(json_parser).

-export([parse/2]).

-type state() :: initial
               | whitespace
               | value
               | null
               | true
               | false
               | string
               | array_start
               | array_element_or_end
               | array_separator_or_end
               | array_element
               | array_end
               | object_start
               | object_key_or_end
               | object_key
               | object_key_value_separator
               | object_value
               | object_member_separator_or_end
               | object_member_separator
               | object_end
               | number
               | final.

-type stack_element() :: json:value()
                       | {json:position(), undefined, undefined}
                       | {json:position(), binary(), undefined}
                       | {json:position(), binary(), json:value()}.
-type stack() :: [stack_element()].

-spec parse(binary(), json:parsing_options()) ->
        {ok, json:value()} | {error, json:error()}.
parse(Data, Options) ->
  case parse(initial, [value, final], Data, [], {1,1}, Options) of
    {ok, Value, <<>>, _} ->
      {ok, Value};
    {ok, _, TrailingData, Pos} ->
      {error, #{reason => {unexpected_trailing_data, TrailingData},
                position => Pos}};
    {error, Error} ->
      {error, Error}
  end.

-spec parse(Current :: state(), Nexts :: [state()], binary(), stack(),
            json:position(), json:parsing_options()) ->
        {ok, json:value(), binary(), json:position()} | {error, json:error()}.

%% Initial state.
parse(initial, Nexts, Data, Stack, Pos, Options) ->
  parse(whitespace, Nexts, Data, Stack, Pos, Options);

%% Whitespace
parse(whitespace, Nexts, <<$\n, Data/binary>>, Stack, {R, _}, Options) ->
  parse(whitespace, Nexts, Data, Stack, {R+1,1}, Options);
parse(whitespace, Nexts, <<B, Data/binary>>, Stack, {R,C}, Options) when
    B =:= $\s; B =:= $\t; B =:= $\r ->
  parse(whitespace, Nexts, Data, Stack, {R,C+1}, Options);
parse(whitespace, [Next | Nexts], Data, Stack, Pos, Options) ->
  parse(Next, Nexts, Data, Stack, Pos, Options);

%% Main value dispatch
parse(value, Next, Data = <<$n, _/binary>>, Stack, Pos, Options) ->
  parse(null, Next, Data, Stack, Pos, Options);
parse(value, Next, Data = <<$t, _/binary>>, Stack, Pos, Options) ->
  parse(true, Next, Data, Stack, Pos, Options);
parse(value, Next, Data = <<$f, _/binary>>, Stack, Pos, Options) ->
  parse(false, Next, Data, Stack, Pos, Options);
parse(value, Next, Data = <<$", _/binary>>, Stack, Pos, Options) ->
  parse(string, Next, Data, Stack, Pos, Options);
parse(value, Next, Data = <<$[, _/binary>>, Stack, Pos, Options) ->
  parse(array_start, Next, Data, Stack, Pos, Options);
parse(value, Next, Data = <<${, _/binary>>, Stack, Pos, Options) ->
  parse(object_start, Next, Data, Stack, Pos, Options);
parse(value, Next, Data = <<B, _/binary>>, Stack, Pos, Options) when
    B =:= $-; B >= $0, B =< $9->
  parse(number, Next, Data, Stack, Pos, Options);
parse(value, _, <<B, _/binary>>, _, Pos, _) ->
  {error, #{reason => {unexpected_character, B}, position => Pos}};
parse(value, _, <<>>, [], Pos, _) ->
  {error, #{reason => no_value, position => Pos}};
parse(value, _, <<>>, [Value | _], Pos, _) when is_list(Value) ->
  {error, #{reason => truncated_array, position => Pos}};
parse(value, _, <<>>, [Value | _], Pos, _) when is_tuple(Value) ->
  {error, #{reason => truncated_object, position => Pos}};
parse(value, _, <<>>, [Value | _], Pos, _) when is_map(Value) ->
  {error, #{reason => truncated_object, position => Pos}};

%% Null
parse(null, Nexts, <<"null", Data/binary>>, Stack, {R,C}, Options) ->
  Stack2 = stack_merge(null, Stack),
  parse(whitespace, Nexts, Data, Stack2, {R,C+4}, Options);
parse(null, _, _, _, Pos, _) ->
  {error, #{reason => invalid_element, position => Pos}};

%% True
parse(true, Nexts, <<"true", Data/binary>>, Stack, {R,C}, Options) ->
  Stack2 = stack_merge(true, Stack),
  parse(whitespace, Nexts, Data, Stack2, {R,C+4}, Options);
parse(true, _, _, _, Pos, _) ->
  {error, #{reason => invalid_element, position => Pos}};

%% False
parse(false, Nexts, <<"false", Data/binary>>, Stack, {R,C}, Options) ->
  Stack2 = stack_merge(false, Stack),
  parse(whitespace, Nexts, Data, Stack2, {R,C+5}, Options);
parse(false, _, _, _, Pos, _) ->
  {error, #{reason => invalid_element, position => Pos}};

%% String
parse(string, Nexts, Data = <<$", _/binary>>, Stack, {R,C}, Options) ->
  case parse_string(Data) of
    {ok, String, N, Rest} ->
      Stack2 = stack_merge(String, Stack),
      parse(whitespace, Nexts, Rest, Stack2, {R,C+N}, Options);
    {error, Reason, N} ->
      {error, #{reason => Reason, position => {R,C+N}}}
  end;

%% Array
parse(array_start, Nexts, <<$[, Data/binary>>, Stack, {R,C}, Options) ->
  parse(whitespace, [array_element_or_end | Nexts],
        Data, [[] | Stack], {R,C+1}, Options);

parse(array_element_or_end, Nexts, Data = <<$], _/binary>>,
      Stack, Pos, Options) ->
  parse(array_end, Nexts, Data, Stack, Pos, Options);
parse(array_element_or_end, Nexts, Data, Stack, Pos, Options) ->
  parse(array_element, Nexts, Data, Stack, Pos, Options);

parse(array_element, Nexts, Data, Stack, Pos, Options) ->
  parse(value, [array_separator_or_end | Nexts], Data, Stack, Pos, Options);

parse(array_separator_or_end, Nexts, Data = <<$], _/binary>>,
      Stack, Pos, Options) ->
  parse(array_end, Nexts, Data, Stack, Pos, Options);
parse(array_separator_or_end, Nexts, Data = <<$,, _/binary>>,
      Stack, Pos, Options) ->
  parse(array_separator, Nexts, Data, Stack, Pos, Options);
parse(array_separator_or_end, _, <<>>, _, Pos, _) ->
  {error, #{reason => truncated_array, position => Pos}};

parse(array_separator, Nexts, <<$,, Data/binary>>, Stack, {R,C}, Options) ->
  parse(whitespace, [array_element | Nexts], Data, Stack, {R,C+1}, Options);

parse(array_end, Nexts, <<$], Data/binary>>,
      [Value | Stack], {R,C}, Options) ->
  Stack2 = stack_merge(lists:reverse(Value), Stack),
  parse(whitespace, Nexts, Data, Stack2, {R,C+1}, Options);

%% Object
parse(object_start, Nexts, <<${, Data/binary>>, Stack, {R,C}, Options) ->
  parse(whitespace, [object_key_or_end | Nexts],
        Data, [#{} | Stack], {R,C+1}, Options);

parse(object_key_or_end, Nexts, Data = <<$}, _/binary>>,
      Stack, Pos, Options) ->
  parse(object_end, Nexts, Data, Stack, Pos, Options);
parse(object_key_or_end, Nexts, Data, Stack, Pos, Options) ->
  parse(object_key, Nexts, Data, Stack, Pos, Options);

parse(object_key, Nexts, Data, Stack, Pos, Options) ->
  Stack2 = [{Pos, undefined, undefined} | Stack],
  parse(value, [object_key_value_separator | Nexts],
        Data, Stack2, Pos, Options);

parse(object_key_value_separator, Nexts, <<$:, Data/binary>>,
      Stack, {R,C}, Options) ->
  parse(whitespace, [object_value | Nexts], Data, Stack, {R,C+1}, Options);
parse(object_key_value_separator, _, <<B, _/binary>>, _, Pos, _) ->
  {error, #{reason => {unexpected_character, B}, position => Pos}};
parse(object_key_value_separator, _, <<>>, _, Pos, _) ->
  {error, #{reason => truncated_object, position => Pos}};

parse(object_value, Nexts, Data, Stack, Pos, Options) ->
  parse(value, [object_member_separator_or_end | Nexts],
        Data, Stack, Pos, Options);

parse(object_member_separator_or_end, Nexts,
      Data = <<$}, _/binary>>, [Value | Stack], Pos, Options) ->
  case stack_merge_member(Value, Stack, Options) of
    {ok, Stack2} ->
      parse(object_end, Nexts, Data, Stack2, Pos, Options);
    {error, Reason} ->
      {KeyPos, _, _} = Value,
      {error, #{reason => Reason, position => KeyPos}}
  end;
parse(object_member_separator_or_end, Nexts,
      Data = <<$,, _/binary>>, [Value | Stack], Pos, Options) ->
  case stack_merge_member(Value, Stack, Options) of
    {ok, Stack2} ->
      parse(object_member_separator, Nexts, Data, Stack2, Pos, Options);
    {error, Reason} ->
      {KeyPos, _, _} = Value,
      {error, #{reason => Reason, position => KeyPos}}
  end;
parse(object_member_separator_or_end, _, <<>>, _, Pos, _) ->
  {error, #{reason => truncated_object, position => Pos}};

parse(object_member_separator, Nexts, <<$,, Data/binary>>,
      Stack, {R,C}, Options) ->
  parse(whitespace, [object_key | Nexts], Data, Stack, {R,C+1}, Options);

parse(object_end, Nexts, <<$}, Data/binary>>,
      [Value | Stack], {R,C}, Options) ->
  Stack2 = stack_merge(Value, Stack),
  parse(whitespace, Nexts, Data, Stack2, {R,C+1}, Options);

%% Number
parse(number, Nexts, Data, Stack, {R,C}, Options) ->
  case parse_number(Data) of
    {ok, Number, N, Rest} ->
      Stack2 = stack_merge(Number, Stack),
      parse(whitespace, Nexts, Rest, Stack2, {R,C+N}, Options);
    {error, Reason, N} ->
      {error, #{reason => Reason, position => {R,C+N}}}
  end;

%% Final state.
parse(final, [], Data, [Value], Pos, _) ->
  {ok, Value, Data, Pos}.

-spec parse_string(binary()) ->
        {ok, binary(), non_neg_integer(), binary()} |
        {error, json:error_reason(), non_neg_integer()}.
parse_string(<<$", Data/binary>>) ->
  parse_string(Data, 1, <<>>).

-spec parse_string(binary(), non_neg_integer(), binary()) ->
        {ok, binary(), non_neg_integer(), binary()} |
        {error, json:error_reason(), non_neg_integer()}.
parse_string(<<>>, N, _) ->
  {error, truncated_string, N-1};
parse_string(<<$", Data/binary>>, N, Acc) ->
  {ok, Acc, N+1, Data};
parse_string(Data = <<$\\, Rest/binary>>, N, Acc) ->
  case Rest of
    <<$\", Data2/binary>> ->
      parse_string(Data2, N+2, <<Acc/binary, $">>);
    <<$\\, Data2/binary>> ->
      parse_string(Data2, N+2, <<Acc/binary, $\\>>);
    <<$\/, Data2/binary>> ->
      parse_string(Data2, N+2, <<Acc/binary, $/>>);
    <<$b, Data2/binary>> ->
      parse_string(Data2, N+2, <<Acc/binary, $\b>>);
    <<$f, Data2/binary>> ->
      parse_string(Data2, N+2, <<Acc/binary, $\f>>);
    <<$r, Data2/binary>> ->
      parse_string(Data2, N+2, <<Acc/binary, $\r>>);
    <<$n, Data2/binary>> ->
      parse_string(Data2, N+2, <<Acc/binary, $\n>>);
    <<$t, Data2/binary>> ->
      parse_string(Data2, N+2, <<Acc/binary, $\t>>);
    <<B, _/binary>> when B =:= $u; B =:= $U ->
      case parse_unicode_escape_sequence(Data) of
        {ok, Code1, Data2} when Code1 >= 16#d800, Code1 =< 16#dbff ->
          case parse_unicode_escape_sequence(Data2) of
            {ok, Code2, Data3} ->
              Code = 16#10_000 + (Code1 - 16#d800 bsl 10) + (Code2 - 16#dc00),
              parse_string(Data3, N+12, <<Acc/binary, Code/utf8>>);
            {error, Reason} ->
              {error, Reason, N+6}
          end;
        {ok, Code1, Data2} ->
          parse_string(Data2, N+6, <<Acc/binary, Code1/utf8>>);
        {error, Reason} ->
          {error, Reason, N}
      end;
    <<_, _/binary>> ->
      {error, invalid_escape_sequence, N};
    <<>> ->
      {error, truncated_escape_sequence, N}
  end;
parse_string(<<C/utf8, Data/binary>>, N, Acc) ->
  parse_string(Data, N+1, <<Acc/binary, C/utf8>>).

-spec parse_unicode_escape_sequence(binary()) ->
        {ok, non_neg_integer(), binary()} | {error, json:error_reason()}.
parse_unicode_escape_sequence(<<$\\, B, HexDigits:4/binary,
                                Data/binary>>) when
    B =:= $u; B =:= $U ->
  try
    Code = binary_to_integer(HexDigits, 16),
    {ok, Code, Data}
  catch error:badarg ->
      {error, invalid_escape_sequence}
  end;
parse_unicode_escape_sequence(<<$\\, B, _/binary>>) when
    B =:= $u; B =:= $U ->
  {error, truncated_escape_sequence};
parse_unicode_escape_sequence(<<"\\", _, _/binary>>) ->
  {error, invalid_escape_sequence};
parse_unicode_escape_sequence(<<"\\", _/binary>>) ->
  {error, truncated_escape_sequence};
parse_unicode_escape_sequence(<<_/binary>>) ->
  {error, truncated_utf16_surrogate_pair}.

-spec parse_number(binary()) ->
        {ok, number(), non_neg_integer(), binary()} |
        {error, json:error_reason(), non_neg_integer()}.
parse_number(Data) ->
  parse_number(Data, 0, sign, {1, undefined, 1, undefined}).

-spec parse_number(binary(), non_neg_integer(),
                   State :: sign
                          | integer_part
                          | fractional_part
                          | exponent | exponent_sign | exponent_part
                          | final,
                   {Sign :: -1 | 1,
                    Base :: undefined
                          | {integer, integer()}
                          | {float, float()},
                    ExponentSign :: -1 | 1,
                    Exponent :: undefined | integer()}) ->
        {ok, number(), non_neg_integer(), binary()} |
        {error, json:error_reason(), non_neg_integer()}.
%% Sign
parse_number(<<$-, Data/binary>>, N, sign, {_, B, ES, E}) ->
  parse_number(Data, N+1, integer_part, {-1, B, ES, E});
parse_number(Data, P, sign, Acc) ->
  parse_number(Data, P, integer_part, Acc);
%% Integer part
parse_number(Data = <<C, _/binary>>, N, integer_part, {S, _, ES, E}) when
    C >= $0, C =< $9 ->
  {I, N2, Rest} = parse_integer(Data),
  parse_number(Rest, N+N2, fractional_part, {S, {integer, I}, ES, E});
parse_number(_, N, integer_part, _Acc) ->
  {error, invalid_number, N};
%% Fractional part
parse_number(<<$., Data/binary>>, N, fractional_part, {S, B, ES, E}) ->
  case Data of
    <<C, _/binary>> when C >= $0, C =< $9 ->
      {F0, N2, Rest} = parse_integer(Data),
      F = F0 / math:pow(10, N2),
      I = case B of
            undefined -> 0;
            {integer, I2} -> I2
          end,
      parse_number(Rest, N+N2+1, exponent, {S, {float, I + F}, ES, E});
    _ ->
      {error, invalid_number, N}
  end;
parse_number(Data, N, fractional_part, Acc) ->
  parse_number(Data, N, exponent, Acc);
%% Exponent
parse_number(<<C, Data/binary>>, N, exponent, Acc) when C =:= $e; C =:= $E ->
  parse_number(Data, N+1, exponent_sign, Acc);
parse_number(Data, N, exponent, Acc) ->
  parse_number(Data, N, final, Acc);
%% Exponent sign
parse_number(<<$-, Data/binary>>, N, exponent_sign, {S, B, _, E}) ->
  parse_number(Data, N+1, exponent_part, {S, B, -1, E});
parse_number(<<$+, Data/binary>>, N, exponent_sign, {S, B, _, E}) ->
  parse_number(Data, N+1, exponent_part, {S, B, 1, E});
parse_number(Data, N, exponent_sign, Acc) ->
  parse_number(Data, N, exponent_part, Acc);
%% Exponent part
parse_number(Data = <<C, _/binary>>, N, exponent_part, {S, B, ES, _}) when
    C >= $0, C =< $9 ->
  {E, N2, Rest} = parse_integer(Data),
  parse_number(Rest, N+N2, final, {S, B, ES, E});
parse_number(_, N, exponent_part, _Acc) ->
  {error, invalid_number, N};
%% Final state
parse_number(Data, N, final, {S, {integer, I}, _, undefined}) ->
  {ok, S * I, N, Data};
parse_number(Data, N, final, {S, {integer, I}, ES, E}) ->
  {ok, S * I* math:pow(10, E * ES), N, Data};
parse_number(Data, N, final, {S, {float, F}, _, undefined}) ->
  {ok, S * F, N, Data};
parse_number(Data, N, final, {S, {float, F}, ES, E}) ->
  {ok, S * F * math:pow(10, E * ES), N, Data}.

-spec parse_integer(binary()) ->
        {integer(), non_neg_integer(), binary()}.
parse_integer(Data) ->
  parse_integer(Data, 0, 0).

-spec parse_integer(binary(), non_neg_integer(), integer()) ->
        {integer(), non_neg_integer(), binary()}.
parse_integer(<<>>, N, I) ->
  {I, N, <<>>};
parse_integer(Data = <<C, Rest/binary>>, N, I) ->
  case is_digit(C) of
    true ->
      parse_integer(Rest, N+1, I*10 + C-$0);
    false ->
      {I, N, Data}
  end.

-spec is_digit(integer()) -> boolean().
is_digit(C) when C >= $0, C =< $9 ->
  true;
is_digit(_) ->
  false.

-spec stack_merge(stack_element(), stack()) -> stack().
stack_merge(Value, []) ->
  [Value];
stack_merge(Key, [{Pos, undefined, undefined} | Values]) ->
  [{Pos, Key, undefined} | Values];
stack_merge(Value, [{Pos, Key, undefined} | Values]) ->
  [{Pos, Key, Value} | Values];
stack_merge(Value, [Parent | Values]) when is_list(Parent) ->
  [[Value | Parent] | Values].

-spec stack_merge_member(stack_element(), stack(), json:parsing_options()) ->
        {ok, stack()} | {error, json:error_reason()}.
stack_merge_member({_, Key, Value}, [Parent | Values], Options) when
    is_binary(Key) ->
  case maps:get(duplicate_key_handling, Options, last) of
    last ->
      {ok, [Parent#{Key => Value} | Values]};
    first ->
      case maps:is_key(Key, Parent) of
        true ->
          {ok, [Parent | Values]};
        false ->
          {ok, [Parent#{Key => Value} | Values]}
      end;
    error ->
      case maps:is_key(Key, Parent) of
        true ->
          {error, {duplicate_key, Key}};
        false ->
          {ok, [Parent#{Key => Value} | Values]}
      end
  end;
stack_merge_member({_, Key, _}, _, _) ->
  {error, {invalid_key, Key}}.
