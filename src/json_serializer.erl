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

-module(json_serializer).

-export([serialize/2,
         serialize_data/1,
         serialize_date/1, serialize_time/1, serialize_datetime/1]).

-type state() ::
        #{options := json:serialization_options(),
          indent_level := non_neg_integer()}.

-spec serialize(json:value(), json:serialization_options()) -> iodata().
serialize(Value, Options0) ->
  Serializers = maps:get(serializers, Options0, json:default_serializers()),
  Options = Options0#{serializers => Serializers},
  State = #{options => Options,
            indent_level => 0},
  Data = serialize1(Value, State),
  case maps:get(return_binary, Options, false) of
    true ->
      iolist_to_binary(Data);
    false ->
      Data
  end.

-spec serialize1(json:value(), state()) -> iodata().
serialize1(null, State) ->
  maybe_highlight(<<"null">>, null, State);
serialize1(true, State) ->
  maybe_highlight(<<"true">>, true, State);
serialize1(false, State) ->
  maybe_highlight(<<"false">>, false, State);
serialize1(Value, State) when is_integer(Value) ->
  Data = integer_to_binary(Value),
  maybe_highlight(Data, Value, State);
serialize1(Value, State) when is_float(Value) ->
  Data = erlang:float_to_binary(Value, [compact, {decimals, 17}]),
  maybe_highlight(Data, Value, State);
serialize1(Value, State) when is_binary(Value) ->
  Data = [character($", State),
          escape(Value, State, <<>>),
          character($", State)],
  maybe_highlight(Data, Value, State);
serialize1([], State) ->
  [character($[, State), character($], State)];
serialize1(Value, State) when is_list(Value) ->
  State2 = indent(State),
  EOL = maybe_eol(State2),
  Data = [character($[, State), EOL,
          lists:join([character($,, State), EOL],
                     [serialize1(V, State2) || V <- Value]),
          maybe_eol(State),
          character($], State)],
  maybe_highlight(Data, Value, State);
serialize1(Value, State) when is_map(Value), map_size(Value) =:= 0 ->
  [character(${, State), character($}, State)];
serialize1(Value, State) when is_map(Value) ->
  State2 = indent(State),
  EOL = maybe_eol(State2),
  F = fun (K, V, Acc) ->
          [[serialize_key(K, State),
            character($:, State),
            serialize1(V, State2)] | Acc]
      end,
  Members = lists:reverse(maps:fold(F, [], Value)),
  Data = [character(${, State), EOL,
          lists:join([character($,, State), EOL], Members),
          maybe_eol(State),
          character($}, State)],
  maybe_highlight(Data, Value, State);
serialize1({Type, Value},
           State = #{options := #{serializers := Serializers}}) ->
  case maps:find(Type, Serializers) of
    {ok, Serialize} ->
      case Serialize(Value) of
        {data, Data} ->
          Data;
        {value, Value2} ->
          serialize1(Value2, State)
      end;
    error ->
      error({unknown_type, Type})
  end;
serialize1({Type, _}, _) ->
  error({unknown_type, Type});
serialize1(Value, _) ->
  error({invalid_value, Value}).

-spec serialize_key(json:key(), state()) -> iodata().
serialize_key(Key, State) when is_atom(Key) ->
  serialize_key(atom_to_binary(Key), State);
serialize_key(Key, State) ->
  Value = unicode:characters_to_binary(Key),
  Data = [$", escape(Value, State, <<>>), $"],
  maybe_highlight(Data, {key, Value}, State).

-spec escape(binary(), state(), Acc :: binary()) -> binary().
escape(<<>>, _, Acc) ->
  Acc;
escape(<<C, S/binary>>, State, Acc) when
    C =:= $"; C =:= $\\; C =:= $/ ->
  escape(S, State, <<Acc/binary, $\\, C>>);
escape(<<$\b, S/binary>>, State, Acc) ->
  escape(S, State, <<Acc/binary, $\\, $b>>);
escape(<<$\f, S/binary>>, State, Acc) ->
  escape(S, State, <<Acc/binary, $\\, $f>>);
escape(<<$\r, S/binary>>, State, Acc) ->
  escape(S, State, <<Acc/binary, $\\, $r>>);
escape(<<$\n, S/binary>>, State, Acc) ->
  escape(S, State, <<Acc/binary, $\\, $n>>);
escape(<<$\t, S/binary>>, State, Acc) ->
  escape(S, State, <<Acc/binary, $\\, $t>>);
escape(<<C/utf8, S/binary>>, State, Acc) when C =< 16#1f ->
  CData = iolist_to_binary(io_lib:format(<<"\\u~4.16.0b">>, [C])),
  escape(S, State, <<Acc/binary, CData/binary>>);
escape(<<C/utf8, S/binary>>, State, Acc) ->
  escape(S, State, <<Acc/binary, C/utf8>>).

-spec serialize_data(iodata()) -> {data, iodata()}.
serialize_data(Data) ->
  {data, Data}.

-spec serialize_date(calendar:date()) -> {value, binary()}.
serialize_date(Date) ->
  {value, format_date(Date)}.

-spec serialize_time(calendar:time()) -> {value, binary()}.
serialize_time(Time) ->
  {value, format_time(Time)}.

-spec serialize_datetime(calendar:datetime()) -> {value, binary()}.
serialize_datetime(Datetime) ->
  {value, format_datetime(Datetime)}.

-spec format_date(calendar:date()) -> binary().
format_date({Y, M, D}) ->
  iolist_to_binary(io_lib:format(<<"~4..0b-~2..0b-~2..0b">>, [Y, M, D])).

-spec format_time(calendar:time()) -> binary().
format_time({H, M, S}) ->
  iolist_to_binary(io_lib:format(<<"~2..0b:~2..0b:~2..0b">>, [H, M, S])).

-spec format_datetime(calendar:datetime()) -> binary().
format_datetime({Date, Time}) ->
  iolist_to_binary([format_date(Date), $T, format_time(Time), $Z]).

-spec character(integer(), state()) -> iodata().
character(C, State) ->
  maybe_highlight(C, {character, C}, State).

-spec indent(state()) -> state().
indent(State = #{indent_level := Level}) ->
  State#{indent_level => Level+1}.

-spec maybe_eol(state()) -> iodata().
maybe_eol(State = #{options := #{indent := true}}) ->
  [$\n, indent_string(State)];
maybe_eol(_) ->
  [].

-spec indent_string(state()) -> iodata().
indent_string(#{indent_level := 0}) ->
  [];
indent_string(#{indent_level := Level, options := Options}) ->
  String = maps:get(indent_string, Options, <<"  ">>),
  [String || _ <- lists:seq(1, Level)].

-spec maybe_highlight(iodata(), json:value(), state()) -> iodata().
maybe_highlight(Data, Value, #{options := #{highlighter := Highlighter}}) ->
  {Before, After} = Highlighter(Value),
  [Before, Data, After];
maybe_highlight(Data, _, _) ->
  Data.
