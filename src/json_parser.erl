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
                    line := pos_integer(),
                    column := pos_integer(),
                    depth := non_neg_integer()}.

-spec new_parser(json:parsing_options()) -> parser().
new_parser(Options) ->
  #{options => Options,
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
  P = new_parser(Options),
  try
    {Value, Data2, P2} = parse1(Data, P),
    case Data2 of
      <<>> ->
        {ok, Value};
      TrailingData ->
        {error, parser_error(P2, {unexpected_trailing_data, TrailingData})}
    end
  catch
    throw:{error, Error} ->
      {error, Error}
  end.

-spec parse_value(binary(), json:parsing_options()) ->
        {ok, json:value(), binary()} | {error, json:error()}.
parse_value(Data, Options) ->
  P = new_parser(Options),
  try
    {Value, TrailingData, _} = parse1(Data, P),
    {ok, Value, TrailingData}
  catch
    throw:{error, Error} ->
      {error, Error}
  end.

-spec parse1(binary(), parser()) -> {json:value(), binary(), parser()}.
parse1(_, P = #{options := #{depth_limit := DepthLimit}, depth := Depth}) when
    Depth > DepthLimit ->
  throw({error, parser_error(P, depth_limit_reached)});
parse1(Data0, P0) ->
  {Data, P} = skip_whitespace(Data0, P0),
  {Value, Data2, P2} =
    case Data of
      <<>> ->
        throw({error, parser_error(P, no_value)});
      <<$n, _/binary>> ->
        parse_null(Data, P);
      <<$t, _/binary>> ->
        parse_true(Data, P);
      <<$f, _/binary>> ->
        parse_false(Data, P);
      <<$", _/binary>> ->
        parse_string(Data, P);
      <<$[, _/binary>> ->
        parse_array(Data, P);
      <<${, _/binary>> ->
        parse_object(Data, P);
      <<C, _/binary>> when C =:= $-; C >= $0, C =< $9 ->
        parse_number(Data, P);
      <<C, _/binary>> ->
        throw({error, parser_error(P, {unexpected_character, C})})
    end,
  {Data3, P3} = skip_whitespace(Data2, P2),
  {Value, Data3, P3}.

-spec parse_null(binary(), parser()) -> {null, binary(), parser()}.
parse_null(<<"null", Data/binary>>, P = #{column := Column}) ->
  {null, Data, P#{column => Column+4}};
parse_null(_, P) ->
  throw({error, parser_error(P, invalid_element)}).

-spec parse_true(binary(), parser()) -> {true, binary(), parser()}.
parse_true(<<"true", Data/binary>>, P = #{column := Column}) ->
  {true, Data, P#{column => Column+4}};
parse_true(_, P) ->
  throw({error, parser_error(P, invalid_element)}).

-spec parse_false(binary(), parser()) -> {false, binary(), parser()}.
parse_false(<<"false", Data/binary>>, P = #{column := Column}) ->
  {false, Data, P#{column => Column+5}};
parse_false(_, P) ->
  throw({error, parser_error(P, invalid_element)}).

-spec parse_string(binary(), parser()) -> {binary(), binary(), parser()}.
parse_string(<<$", Data/binary>>, P = #{column := Column}) ->
  parse_string(Data, P#{column => Column+1}, <<>>);
parse_string(_, P) ->
  throw({error, parser_error(P, invalid_string)}).

-spec parse_string(binary(), parser(), binary()) ->
        {binary(), binary(), parser()}.
parse_string(<<>>, P, _Acc) ->
  throw({error, parser_error(P, truncated_string)});
parse_string(<<$", Data/binary>>, P = #{column := Column}, Acc) ->
  {Acc, Data, P#{column => Column+1}};
parse_string(<<$\\, $", Data/binary>>, P = #{column := Column}, Acc) ->
  parse_string(Data, P#{column => Column+2}, <<Acc/binary, $">>);
parse_string(<<$\\, $\\, Data/binary>>, P = #{column := Column}, Acc) ->
  parse_string(Data, P#{column => Column+2}, <<Acc/binary, $\\>>);
parse_string(<<$\\, $/, Data/binary>>, P = #{column := Column}, Acc) ->
  parse_string(Data, P#{column => Column+2}, <<Acc/binary, $/>>);
parse_string(<<$\\, $b, Data/binary>>, P = #{column := Column}, Acc) ->
  parse_string(Data, P#{column => Column+2}, <<Acc/binary, $\b>>);
parse_string(<<$\\, $f, Data/binary>>, P = #{column := Column}, Acc) ->
  parse_string(Data, P#{column => Column+2}, <<Acc/binary, $\f>>);
parse_string(<<$\\, $r, Data/binary>>, P = #{column := Column}, Acc) ->
  parse_string(Data, P#{column => Column+2}, <<Acc/binary, $\r>>);
parse_string(<<$\\, $n, Data/binary>>, P = #{column := Column}, Acc) ->
  parse_string(Data, P#{column => Column+2}, <<Acc/binary, $\n>>);
parse_string(<<$\\, $t, Data/binary>>, P = #{column := Column}, Acc) ->
  parse_string(Data, P#{column => Column+2}, <<Acc/binary, $\t>>);
parse_string(Data = <<$\\, C, _/binary>>, P, Acc) when C =:= $u; C =:= $U ->
  {Code1, Data2, P2} = parse_unicode_escape_sequence(Data, P),
  if
    Code1 >= 16#d800, Code1 =< 16#dbff ->
      {Code2, Data3, P3} = parse_unicode_escape_sequence(Data2, P2),
      Code = 16#10_000 + (Code1 - 16#d800 bsl 10) + (Code2 - 16#dc00),
      parse_string(Data3, P3, <<Acc/binary, Code/utf8>>);
    true ->
      parse_string(Data2, P2, <<Acc/binary, Code1/utf8>>)
  end;
parse_string(<<$\\, _, _/binary>>, P, _Acc) ->
  throw({error, parser_error(P, invalid_escape_sequence)});
parse_string(<<$\\>>, P, _Acc) ->
  throw({error, parser_error(P, truncated_escape_sequence)});
parse_string(<<C/utf8, Data/binary>>, P = #{column := Column}, Acc) ->
  parse_string(Data, P#{column => Column+1}, <<Acc/binary, C/utf8>>).

-spec parse_unicode_escape_sequence(binary(), parser()) ->
        {integer(), binary(), parser()}.
parse_unicode_escape_sequence(<<$\\, C, HexCode:4/binary, Data/binary>>,
                              P = #{column := Column}) when
    C =:= $u; C =:= $U ->
  try
    Code = binary_to_integer(HexCode, 16),
    {Code, Data, P#{column => Column+6}}
  catch error:badarg ->
      throw({error, parser_error(P, invalid_escape_sequence)})
  end;
parse_unicode_escape_sequence(<<$\\, C, _/binary>>, P) when
    C =:= $u; C =:= $U ->
  throw({error, parser_error(P, truncated_escape_sequence)});
parse_unicode_escape_sequence(<<$\\>>, P) ->
  throw({error, parser_error(P, truncated_escape_sequence)});
parse_unicode_escape_sequence(_, P) ->
  throw({error, parser_error(P, truncated_utf16_surrogate_pair)}).

-spec parse_array(binary(), parser()) -> {[json:value()], binary(), parser()}.
parse_array(<<$[, Data/binary>>, P = #{column := Column, depth := Depth}) ->
  {Data2, P2} = skip_whitespace(Data,
                                P#{column => Column+1, depth => Depth+1}),
  parse_array(Data2, P2, []);
parse_array(_, P) ->
  throw({error, parser_error(P, invalid_array)}).

-spec parse_array(binary(), parser(), [json:value()]) ->
        {[json:value()], binary(), parser()}.
parse_array(<<>>, P, _Acc) ->
  throw({error, parser_error(P, truncated_array)});
parse_array(<<$], _/binary>>, P, Acc) when Acc =/= [] ->
  throw({error, parser_error(P, {unexpected_character, $]})});
parse_array(<<$], Data/binary>>, P = #{column := Column}, Acc) ->
  {lists:reverse(Acc), Data, P#{column => Column+1}};
parse_array(Data, P, Acc) ->
  case parse1(Data, P) of
    {Value, <<$,, Data2/binary>>, P2 = #{column := Column}} ->
      {Data3, P3} = skip_whitespace(Data2, P2#{column => Column+1}),
      parse_array(Data3, P3, [Value | Acc]);
    {Value, <<$], Data2/binary>>, P2 = #{column := Column}} ->
      {lists:reverse([Value | Acc]), Data2, P2#{column => Column+1}};
    {_, <<C, _/binary>>, P2} ->
      throw({error, parser_error(P2, {unexpected_character, C})});
    {_, <<>>, P2} ->
      throw({error, parser_error(P2, truncated_array)})
  end.

-spec parse_object(binary(), parser()) ->
        {#{binary() := json:value()}, binary(), parser()}.
parse_object(<<${, Data/binary>>, P = #{column := Column, depth := Depth}) ->
  {Data2, P2} = skip_whitespace(Data,
                                P#{column => Column+1, depth => Depth+1}),
  parse_object(Data2, P2, #{});
parse_object(_, P) ->
  throw({error, parser_error(P, invalid_object)}).

-spec parse_object(binary(), parser(), #{binary() := json:value()}) ->
        {#{binary() := json:value()}, binary(), parser()}.
parse_object(<<>>, P, _Acc) ->
  throw({error, parser_error(P, truncated_object)});
parse_object(<<$}, _/binary>>, P, Acc) when map_size(Acc) > 0 ->
  throw({error, parser_error(P, {unexpected_character, $}})});
parse_object(<<$}, Data/binary>>, P = #{column := Column}, Acc) ->
  {Acc, Data, P#{column => Column+1}};
parse_object(Data, P = #{options := Options}, Acc) ->
  case parse_object_member(Data, P) of
    {{Key, Value}, Data2, P2} when is_binary(Key) ->
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
      case skip_whitespace(Data2, P2) of
        {<<$,, Data3/binary>>, P3 = #{column := Column}} ->
          {Data4, P4} = skip_whitespace(Data3, P3#{column => Column+1}),
          parse_object(Data4, P4, Acc2);
        {<<$}, Data3/binary>>, P3 = #{column := Column}} ->
          {Acc2, Data3, P3#{column => Column+1}};
        {<<C, _/binary>>, P3} ->
          throw({error, parser_error(P3, {unexpected_character, C})});
        {<<>>, P3} ->
          throw({error, parser_error(P3, truncated_object)})
      end;
    {{Key, _}, _, _} ->
      throw({error, parser_error(P, {invalid_key, Key})})
  end.

-spec parse_object_member(binary(), parser()) ->
        {{json:value(), json:value()}, binary(), parser()}.
parse_object_member(Data, P) ->
  case parse1(Data, P) of
    {Key, <<$:, Data2/binary>>, P2 = #{column := Column}} ->
      case skip_whitespace(Data2, P2#{column => Column+1}) of
        {<<$}, _/binary>>, P3} ->
          throw({error, parser_error(P3, {unexpected_character, $}})});
        {<<>>, P3} ->
          throw({error, parser_error(P3, truncated_object)});
        {Data3, P3} ->
          {Value, Data4, P4} = parse1(Data3, P3),
          {{Key, Value}, Data4, P4}
      end;
    {_, <<C, _/binary>>, P2} ->
      throw({error, parser_error(P2, {unexpected_character, C})});
    {_, <<>>, P2} ->
      throw({error, parser_error(P2, truncated_object)})
  end.

-spec parse_number(binary(), parser()) -> {number(), binary(), parser()}.
parse_number(Data, P) ->
  parse_number(Data, P, sign, {1, undefined, 1, undefined}).

-spec parse_number(binary(), parser(),
                   State :: sign
                          | integer_part
                          | fractional_part
                          | exponent | exponent_sign | exponent_part
                          | final,
                   {Sign :: -1 | 1,
                    Base :: undefined | {integer, integer()} | {float, float()},
                    ExponentSign :: -1 | 1,
                    Exponent :: undefined | integer()}) ->
        {number(), binary(), parser()}.
%% Sign
parse_number(<<$-, Data/binary>>, P = #{column := Column},
             sign, {_, B, ES, E}) ->
  parse_number(Data, P#{column => Column+1}, integer_part, {-1, B, ES, E});
parse_number(Data, P, sign, Acc) ->
  parse_number(Data, P, integer_part, Acc);
%% Integer part
parse_number(Data = <<C, _/binary>>, P = #{column := Column},
             integer_part, {S, _, ES, E}) when
    C >= $0, C =< $9 ->
  {I, Rest, Length} = parse_simple_integer(Data),
  parse_number(Rest, P#{column => Column+Length},
               fractional_part, {S, {integer, I}, ES, E});
parse_number(_, P, integer_part, _Acc) ->
      throw({error, parser_error(P, invalid_number)});
%% Fractional part
parse_number(<<$., Data/binary>>, P = #{column := Column},
             fractional_part, {S, B, ES, E}) ->
  case Data of
    <<C, _/binary>> when C >= $0, C =< $9 ->
      {F0, Rest, Length} = parse_simple_integer(Data),
      F = F0 / math:pow(10, Length),
      I = case B of
            undefined -> 0;
            {integer, I2} -> I2
          end,
      parse_number(Rest, P#{column => Column+Length+1},
                   exponent, {S, {float, I + F}, ES, E});
    _ ->
      throw({error, parser_error(P, invalid_number)})
  end;
parse_number(Data, P, fractional_part, Acc) ->
  parse_number(Data, P, exponent, Acc);
%% Exponent
parse_number(<<C, Data/binary>>, P = #{column := Column}, exponent, Acc) when
    C =:= $e; C =:= $E ->
  parse_number(Data, P#{column => Column+1}, exponent_sign, Acc);
parse_number(Data, P, exponent, Acc) ->
  parse_number(Data, P, final, Acc);
%% Exponent sign
parse_number(<<$-, Data/binary>>, P = #{column := Column},
             exponent_sign, {S, B, _, E}) ->
  parse_number(Data, P#{column => Column+1}, exponent_part, {S, B, -1, E});
parse_number(<<$+, Data/binary>>, P = #{column := Column},
             exponent_sign, {S, B, _, E}) ->
  parse_number(Data, P#{column => Column+1}, exponent_part, {S, B, 1, E});
parse_number(Data, P, exponent_sign, Acc) ->
  parse_number(Data, P, exponent_part, Acc);
%% Exponent part
parse_number(Data = <<C, _/binary>>, P = #{column := Column},
             exponent_part, {S, B, ES, _}) when
    C >= $0, C =< $9 ->
  {E, Rest, Length} = parse_simple_integer(Data),
  parse_number(Rest, P#{column => Column+Length}, final, {S, B, ES, E});
parse_number(_, P, exponent_part, _Acc) ->
      throw({error, parser_error(P, invalid_number)});
%% Final state
parse_number(Data, P, final, {S, {integer, I}, _, undefined}) ->
  {S * I, Data, P};
parse_number(Data, P, final, {S, {integer, I}, ES, E}) ->
  {S * I* math:pow(10, E * ES), Data, P};
parse_number(Data, P, final, {S, {float, F}, _, undefined}) ->
  {S * F, Data, P};
parse_number(Data, P, final, {S, {float, F}, ES, E}) ->
  {S * F * math:pow(10, E * ES), Data, P}.

-spec parse_simple_integer(binary()) ->
        {I :: integer(), Rest :: binary(), Length :: non_neg_integer()}.
parse_simple_integer(Data) ->
  parse_simple_integer(Data, 0, 0).

-spec parse_simple_integer(binary(), I :: integer(),
                           Length :: non_neg_integer()) ->
        {I :: integer(), Rest :: binary(), Length :: non_neg_integer()}.
parse_simple_integer(<<>>, I, Length) ->
  {I, <<>>, Length};
parse_simple_integer(Data = <<C, Rest/binary>>, I, Length) ->
  case is_digit(C) of
    true ->
      parse_simple_integer(Rest, I*10 + C-$0, Length+1);
    false ->
      {I, Data, Length}
  end.

-spec is_digit(integer()) -> boolean().
is_digit(C) when C >= $0, C =< $9 -> true;
is_digit(_) -> false.

-spec skip_whitespace(binary(), parser()) -> {binary(), parser()}.
skip_whitespace(<<$\n, Data/binary>>, P = #{line := Line}) ->
  skip_whitespace(Data, P#{line => Line+1, column => 1});
skip_whitespace(<<C, Data/binary>>, P = #{column := Column}) when
    C =:= $\s; C =:= $\t; C =:= $\r ->
  skip_whitespace(Data, P#{column => Column+1});
skip_whitespace(Data, P) ->
  {Data, P}.
