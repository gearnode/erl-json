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

-module(json_serializer).

-export([serialize/2,
         serialize_data/1,
         serialize_date/1, serialize_time/1, serialize_datetime/1]).

-spec serialize(json:value(), json:serialization_options()) -> iodata().
serialize(Value, Options0) ->
  Serializers = maps:get(serializers, Options0, json:default_serializers()),
  Options = Options0#{serializers => Serializers},
  Data = serialize1(Value, Options),
  case maps:get(return_binary, Options, false) of
    true ->
      iolist_to_binary(Data);
    false ->
      Data
  end.

-spec serialize1(json:value(), json:serialization_options()) -> iodata().
serialize1(null, _Options) ->
  <<"null">>;
serialize1(true, _Options) ->
  <<"true">>;
serialize1(false, _Options) ->
  <<"false">>;
serialize1(Value, _Options) when is_integer(Value) ->
  integer_to_binary(Value);
serialize1(Value, _Options) when is_float(Value) ->
  erlang:float_to_binary(Value, [compact, {decimals, 17}]);
serialize1(Value, Options) when is_binary(Value) ->
  [$", escape(Value, Options, <<>>), $"];
serialize1(Value, Options) when is_list(Value) ->
  F = fun (V) -> serialize(V, Options) end,
  [$[, lists:join($,, lists:map(F, Value)), $]];
serialize1(Value, Options) when is_map(Value) ->
  F = fun (K, V, Acc) ->
          [[serialize_key(K, Options), $:, serialize1(V, Options)] | Acc]
      end,
  Members = lists:reverse(maps:fold(F, [], Value)),
  [${, lists:join($,, Members), $}];
serialize1({Type, Value}, Options = #{serializers := Serializers}) ->
  case maps:find(Type, Serializers) of
    {ok, Serialize} ->
      case Serialize(Value) of
        {data, Data} ->
          Data;
        {value, Value2} ->
          serialize1(Value2, Options)
      end;
    error ->
      error({unknown_type, Type})
  end;
serialize1({Type, _}, _Options) ->
  error({unknown_type, Type});
serialize1(Value, _Options) ->
  error({invalid_value, Value}).

-spec serialize_key(json:key(), json:serialization_options()) -> iodata().
serialize_key(Key, Options) when is_atom(Key) ->
  serialize_key(atom_to_binary(Key), Options);
serialize_key(Key, Options) ->
  serialize1(unicode:characters_to_binary(Key), Options).

-spec escape(binary(), json:serialization_options(), Acc :: binary()) ->
        binary().
escape(<<>>, _Options, Acc) ->
  Acc;
escape(<<C, S/binary>>, Options, Acc) when
    C =:= $"; C =:= $\\; C =:= $/ ->
  escape(S, Options, <<Acc/binary, $\\, C>>);
escape(<<$\b, S/binary>>, Options, Acc) ->
  escape(S, Options, <<Acc/binary, $\\, $b>>);
escape(<<$\f, S/binary>>, Options, Acc) ->
  escape(S, Options, <<Acc/binary, $\\, $f>>);
escape(<<$\r, S/binary>>, Options, Acc) ->
  escape(S, Options, <<Acc/binary, $\\, $r>>);
escape(<<$\n, S/binary>>, Options, Acc) ->
  escape(S, Options, <<Acc/binary, $\\, $n>>);
escape(<<$\t, S/binary>>, Options, Acc) ->
  escape(S, Options, <<Acc/binary, $\\, $t>>);
escape(<<C/utf8, S/binary>>, Options, Acc) when C =< 16#1f ->
  CData = iolist_to_binary(io_lib:format(<<"\\u~4.16.0b">>, [C])),
  escape(S, Options, <<Acc/binary, CData/binary>>);
escape(<<C/utf8, S/binary>>, Options, Acc) ->
  escape(S, Options, <<Acc/binary, C/utf8>>).

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
