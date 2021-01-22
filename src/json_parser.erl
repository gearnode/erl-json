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

-export([parse/2, parse_value/2]).

-type parser() :: #{options := json:parsing_options(),
                    data := binary(),
                    line := pos_integer(),
                    column := pos_integer(),
                    depth := non_neg_integer()}.

-spec new_parser(binary(), json:parsing_options()) -> parser().
new_parser(Data, Options) ->
  #{options => Options,
    data => Data,
    line => 1,
    column => 1,
    depth => 0}.

-spec parser_error(parser(), term()) -> json:error().
parser_error(#{line := Line, column := Column}, Reason) ->
  #{reason => Reason,
    position => {Line, Column}}.

-spec parse(binary(), json:parsing_options()) ->
        {ok, json:value()} | {error, json:error()}.
parse(Data, Options) ->
  P = new_parser(Data, Options),
  try
    {Value, P2} = parse1(P),
    case P2 of
      #{data := <<>>} ->
        {ok, Value};
      #{data := TrailingData} ->
        {error, parser_error(P2, {unexpected_trailing_data, TrailingData})}
    end
  catch
    throw:{error, Error} ->
      {error, Error}
  end.

-spec parse_value(binary(), json:parsing_options()) ->
        {ok, json:value(), binary()} | {error, json:error()}.
parse_value(Data, Options) ->
  P = new_parser(Data, Options),
  try
    {Value, #{data := TrailingData}} = parse1(P),
    {ok, Value, TrailingData}
  catch
    throw:{error, Error} ->
      {error, Error}
  end.

-spec parse1(parser()) -> {json:value(), parser()}.
parse1(P = #{options := #{depth_limit := DepthLimit}, depth := Depth}) when
    Depth > DepthLimit ->
  throw({error, parser_error(P, depth_limit_reached)});
parse1(P0) ->
  P = skip_whitespace(P0),
  {Value, P2} =
    case P of
      #{data := <<>>} ->
        throw({error, parser_error(P, no_value)});
      #{data := <<$n, _/binary>>} ->
        parse_null(P);
      #{data := <<$t, _/binary>>} ->
        parse_true(P);
      #{data := <<$f, _/binary>>} ->
        parse_false(P);
      #{data := <<$", _/binary>>} ->
        parse_string(P);
      #{data := <<$[, _/binary>>} ->
        parse_array(P);
      #{data := <<${, _/binary>>} ->
        parse_object(P);
      #{data := <<C, _/binary>>} when C =:= $-; C >= $0, C =< $9 ->
        parse_number(P);
      #{data := <<C, _/binary>>} ->
        throw({error, parser_error(P, {unexpected_character, C})})
    end,
  {Value, skip_whitespace(P2)}.

-spec parse_null(parser()) -> {null, parser()}.
parse_null(P = #{data := <<"null", _/binary>>}) ->
  {null, skip(P, 4)};
parse_null(P) ->
  throw({error, parser_error(P, invalid_element)}).

-spec parse_true(parser()) -> {true, parser()}.
parse_true(P = #{data := <<"true", _/binary>>}) ->
  {true, skip(P, 4)};
parse_true(P) ->
  throw({error, parser_error(P, invalid_element)}).

-spec parse_false(parser()) -> {false, parser()}.
parse_false(P = #{data := <<"false", _/binary>>}) ->
  {false, skip(P, 5)};
parse_false(P) ->
  throw({error, parser_error(P, invalid_element)}).

-spec parse_string(parser()) -> {binary(), parser()}.
parse_string(P = #{data := <<$", _/binary>>}) ->
  parse_string(skip1(P), <<>>);
parse_string(P) ->
  throw({error, parser_error(P, invalid_string)}).

-spec parse_string(parser(), Acc :: binary()) -> {binary(), parser()}.
parse_string(P = #{data := <<>>}, _Acc) ->
  throw({error, parser_error(P, truncated_string)});
parse_string(P = #{data := <<$", _/binary>>}, Acc) ->
  {Acc, skip1(P)};
parse_string(P = #{data := <<$\\, $", _/binary>>}, Acc) ->
  parse_string(skip(P, 2), <<Acc/binary, $">>);
parse_string(P = #{data := <<$\\, $\\, _/binary>>}, Acc) ->
  parse_string(skip(P, 2), <<Acc/binary, $\\>>);
parse_string(P = #{data := <<$\\, $/, _/binary>>}, Acc) ->
  parse_string(skip(P, 2), <<Acc/binary, $/>>);
parse_string(P = #{data := <<$\\, $b, _/binary>>}, Acc) ->
  parse_string(skip(P, 2), <<Acc/binary, $\b>>);
parse_string(P = #{data := <<$\\, $f, _/binary>>}, Acc) ->
  parse_string(skip(P, 2), <<Acc/binary, $\f>>);
parse_string(P = #{data := <<$\\, $r, _/binary>>}, Acc) ->
  parse_string(skip(P, 2), <<Acc/binary, $\r>>);
parse_string(P = #{data := <<$\\, $n, _/binary>>}, Acc) ->
  parse_string(skip(P, 2), <<Acc/binary, $\n>>);
parse_string(P = #{data := <<$\\, $t, _/binary>>}, Acc) ->
  parse_string(skip(P, 2), <<Acc/binary, $\t>>);
parse_string(P = #{data := <<$\\, C, _/binary>>}, Acc) when
    C =:= $u; C =:= $U ->
  {Code1, P2} = parse_unicode_escape_sequence(P),
  if
    Code1 >= 16#d800, Code1 =< 16#dbff ->
      {Code2, P3} = parse_unicode_escape_sequence(P2),
      Code = 16#10_000 + (Code1 - 16#d800 bsl 10) + (Code2 - 16#dc00),
      parse_string(P3, <<Acc/binary, Code/utf8>>);
    true ->
      parse_string(P2, <<Acc/binary, Code1/utf8>>)
  end;
parse_string(P = #{data := <<$\\, _, _/binary>>}, _Acc) ->
  throw({error, parser_error(P, invalid_escape_sequence)});
parse_string(P = #{data := <<$\\>>}, _Acc) ->
  throw({error, parser_error(P, truncated_escape_sequence)});
parse_string(P = #{data := (Data = <<C/utf8, Rest/binary>>)}, Acc) ->
  N = byte_size(Data) - byte_size(Rest),
  parse_string(skip(P, N), <<Acc/binary, C/utf8>>).

-spec parse_unicode_escape_sequence(parser()) -> {integer(), parser()}.
parse_unicode_escape_sequence(P = #{data := <<$\\, C, HexCode:4/binary,
                                              _/binary>>}) when
    C =:= $u; C =:= $U ->
  try
    Code = binary_to_integer(HexCode, 16),
    {Code, skip(P, 6)}
  catch error:badarg ->
      throw({error, parser_error(P, invalid_escape_sequence)})
  end;
parse_unicode_escape_sequence(P = #{data := <<$\\, C, _/binary>>}) when
    C =:= $u; C =:= $U ->
  throw({error, parser_error(P, truncated_escape_sequence)});
parse_unicode_escape_sequence(P = #{data := <<$\\>>}) ->
  throw({error, parser_error(P, truncated_escape_sequence)});
parse_unicode_escape_sequence(P) ->
  throw({error, parser_error(P, truncated_utf16_surrogate_pair)}).

-spec parse_array(parser()) -> {[json:value()], parser()}.
parse_array(P = #{data := <<$[, _/binary>>, depth := Depth}) ->
  parse_array(skip_whitespace(skip1(P#{depth => Depth+1})), []);
parse_array(P) ->
  throw({error, parser_error(P, invalid_array)}).

-spec parse_array(parser(), Acc :: [json:value()]) ->
        {[json:value()], parser()}.
parse_array(P = #{data := <<>>}, _Acc) ->
  throw({error, parser_error(P, truncated_array)});
parse_array(P = #{data := <<$], _/binary>>}, Acc) when
    Acc =/= [] ->
  throw({error, parser_error(P, {unexpected_character, $]})});
parse_array(P = #{data := <<$], _/binary>>}, Acc) ->
  {lists:reverse(Acc), skip1(P)};
parse_array(P, Acc) ->
  case parse1(P) of
    {Value, P2 = #{data := <<$,, _/binary>>}} ->
      parse_array(skip_whitespace(skip1(P2)), [Value | Acc]);
    {Value, P2 = #{data := <<$], _/binary>>}} ->
      {lists:reverse([Value | Acc]), skip1(P2)};
    {_, P2 = #{data := <<C, _/binary>>}} ->
      throw({error, parser_error(P2, {unexpected_character, C})});
    {_, P2 = #{data := <<>>}} ->
      throw({error, parser_error(P2, truncated_array)})
  end.

-spec parse_object(parser()) -> {#{binary() := json:value()}, parser()}.
parse_object(P = #{data := <<${, _/binary>>, depth := Depth}) ->
  parse_object(skip_whitespace(skip1(P#{depth => Depth+1})), #{});
parse_object(P) ->
  throw({error, parser_error(P, invalid_object)}).

-spec parse_object(parser(), Acc :: #{binary() := json:value()}) ->
        {#{binary() := json:value()}, parser()}.
parse_object(P = #{data := <<>>}, _Acc) ->
  throw({error, parser_error(P, truncated_object)});
parse_object(P = #{data := <<$}, _/binary>>}, Acc) when
    map_size(Acc) > 0 ->
  throw({error, parser_error(P, {unexpected_character, $}})});
parse_object(P = #{data := <<$}, _/binary>>}, Acc) ->
  {Acc, skip1(P)};
parse_object(P = #{options := Options}, Acc) ->
  case parse_object_member(P) of
    {{Key, Value}, P2} when is_binary(Key) ->
      DuplicateKeyHandling = maps:get(duplicate_key_handling, Options, last),
      Acc2 = case {maps:is_key(Key, Acc), DuplicateKeyHandling} of
               {true, first} ->
                 Acc;
               {true, last} ->
                 Acc#{Key => Value};
               {true, error} ->
                 throw({error, parser_error(P, {duplicate_key, Key})});
               {false, _} ->
                 Acc#{Key => Value}
             end,
      case skip_whitespace(P2) of
        P3 = #{data := <<$,, _/binary>>} ->
          parse_object(skip_whitespace(skip1(P3)), Acc2);
        P3 = #{data := <<$}, _/binary>>} ->
          {Acc2, skip1(P3)};
        P3 = #{data := <<C, _/binary>>} ->
          throw({error, parser_error(P3, {unexpected_character, C})});
        P3 = #{data := <<>>} ->
          throw({error, parser_error(P3, truncated_object)})
      end;
    {{Key, _}, _} ->
      throw({error, parser_error(P, {invalid_key, Key})})
  end.

-spec parse_object_member(parser()) -> {{json:value(), json:value()}, parser()}.
parse_object_member(P) ->
  case parse1(P) of
    {Key, P2 = #{data := <<$:, _/binary>>}} ->
      case skip_whitespace(skip1(P2)) of
        P3 = #{data := <<$}, _/binary>>} ->
          throw({error, parser_error(P3, {unexpected_character, $}})});
        P3 = #{data := <<>>} ->
          throw({error, parser_error(P3, truncated_object)});
        P3 ->
          {Value, P4} = parse1(P3),
          {{Key, Value}, P4}
      end;
    {_, P2 = #{data := <<C, _/binary>>}} ->
      throw({error, parser_error(P2, {unexpected_character, C})});
    {_, P2 = #{data := <<>>}} ->
      throw({error, parser_error(P2, truncated_object)})
  end.

-spec parse_number(parser()) -> {number(), parser()}.
parse_number(P) ->
  parse_number(P, sign, {1, undefined, 1, undefined}).

-spec parse_number(parser(),
                   State :: sign
                          | integer_part
                          | fractional_part
                          | exponent | exponent_sign | exponent_part
                          | final,
                   {Sign :: -1 | 1,
                    Base :: undefined | {integer, integer()} | {float, float()},
                    ExponentSign :: -1 | 1,
                    Exponent :: undefined | integer()}) ->
        {number(), parser()}.
%% Sign
parse_number(P = #{data := <<$-, _/binary>>}, sign, {_, B, ES, E}) ->
  parse_number(skip1(P), integer_part, {-1, B, ES, E});
parse_number(P, sign, Acc) ->
  parse_number(P, integer_part, Acc);
%% Integer part
parse_number(P = #{data := (Data = <<C, _/binary>>)}, integer_part,
             {S, _, ES, E}) when
    C >= $0, C =< $9 ->
  {I, Length} = parse_simple_integer(Data),
  parse_number(skip(P, Length), fractional_part, {S, {integer, I}, ES, E});
parse_number(P, integer_part, _Acc) ->
      throw({error, parser_error(P, invalid_number)});
%% Fractional part
parse_number(P = #{data := <<$., Data/binary>>},
             fractional_part, {S, B, ES, E}) ->
  case Data of
    <<C, _/binary>> when C >= $0, C =< $9 ->
      {F0, Length} = parse_simple_integer(Data),
      F = F0 / math:pow(10, Length),
      I = case B of
            undefined -> 0;
            {integer, I2} -> I2
          end,
      parse_number(skip(P, Length+1), exponent, {S, {float, I + F}, ES, E});
    _ ->
      throw({error, parser_error(P, invalid_number)})
  end;
parse_number(P, fractional_part, Acc) ->
  parse_number(P, exponent, Acc);
%% Exponent
parse_number(P = #{data := <<C, _/binary>>}, exponent, Acc) when
    C =:= $e; C =:= $E ->
  parse_number(skip1(P), exponent_sign, Acc);
parse_number(P, exponent, Acc) ->
  parse_number(P, final, Acc);
%% Exponent sign
parse_number(P = #{data := <<$-, _/binary>>}, exponent_sign, {S, B, _, E}) ->
  parse_number(skip1(P), exponent_part, {S, B, -1, E});
parse_number(P = #{data := <<$+, _/binary>>}, exponent_sign, {S, B, _, E}) ->
  parse_number(skip1(P), exponent_part, {S, B, 1, E});
parse_number(P, exponent_sign, Acc) ->
  parse_number(P, exponent_part, Acc);
%% Exponent part
parse_number(P = #{data := (Data = <<C, _/binary>>)}, exponent_part,
             {S, B, ES, _}) when
    C >= $0, C =< $9 ->
  {E, Length} = parse_simple_integer(Data),
  parse_number(skip(P, Length), final, {S, B, ES, E});
parse_number(P, exponent_part, _Acc) ->
      throw({error, parser_error(P, invalid_number)});
%% Final state
parse_number(P, final, {S, {integer, I}, _, undefined}) ->
  {S * I, P};
parse_number(P, final, {S, {integer, I}, ES, E}) ->
  {S * I* math:pow(10, E * ES), P};
parse_number(P, final, {S, {float, F}, _, undefined}) ->
  {S * F, P};
parse_number(P, final, {S, {float, F}, ES, E}) ->
  {S * F * math:pow(10, E * ES), P}.

-spec parse_simple_integer(binary()) ->
        {I :: integer(), Length :: non_neg_integer()}.
parse_simple_integer(Data) ->
  parse_simple_integer(Data, 0, 0).

-spec parse_simple_integer(binary(), I :: integer(),
                           Length :: non_neg_integer()) ->
        {I :: integer(), Length :: non_neg_integer()}.
parse_simple_integer(<<>>, I, Length) ->
  {I, Length};
parse_simple_integer(<<C, Rest/binary>>, I, Length) ->
  case is_digit(C) of
    true ->
      parse_simple_integer(Rest, I*10 + C-$0, Length+1);
    false ->
      {I, Length}
  end.

-spec is_digit(integer()) -> boolean().
is_digit(C) when C >= $0, C =< $9 -> true;
is_digit(_) -> false.

-spec skip_whitespace(parser()) -> parser().
skip_whitespace(P = #{data := <<C, _/binary>>}) when
    C =:= $\s; C =:= $\t; C =:= $\r; C =:= $\n ->
  skip_whitespace(skip1(P));
skip_whitespace(P) ->
  P.

-spec skip(parser(), non_neg_integer()) -> parser().
skip(P, 0) ->
  P;
skip(P, N) ->
  skip(skip1(P), N-1).

-spec skip1(parser()) -> parser().
skip1(P = #{data := <<$\n, Data/binary>>, line := Line}) ->
  P#{data => Data, line => Line+1, column => 0};
skip1(P = #{data := <<_, Data/binary>>, column := Column}) ->
  P#{data => Data, column => Column+1}.
