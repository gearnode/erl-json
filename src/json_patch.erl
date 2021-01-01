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

-module(json_patch).

-export([parse/1, serialize/1, execute/2]).

-export_type([operation/0]).

-type operation() :: {add, json_pointer:pointer(), json:value()}
                   | {remove, json_pointer:pointer()}
                   | {replace, json_pointer:pointer(), json:value()}
                   | {move, json_pointer:pointer(), json_pointer:pointer()}
                   | {copy, json_pointer:pointer(), json_pointer:pointer()}
                   | {test, json_pointer:pointer(), json:value()}.
-type patch() :: [operation()].

-spec parse(json:value()) -> {ok, patch()} | {error, term()}.
parse(Value) when is_list(Value) ->
  try
    Operations = lists:foldl(fun (Value2, Acc) ->
                                 [parse_operation(Value2) | Acc]
                             end, [], Value),
    lists:reverse(Operations)
  catch
    throw:{error, Reason} ->
      {error, Reason}
  end;
parse(_) ->
  {error, invalid_format}.

-spec parse_operation(json:value()) -> operation().
parse_operation(V = #{<<"op">> := <<"add">>}) ->
  Pointer = json_operation_pointer(V, <<"path">>),
  Value = json_operation_value(V, <<"value">>),
  {add, Pointer, Value};
parse_operation(V = #{<<"op">> := <<"remove">>}) ->
  Pointer = json_operation_pointer(V, <<"path">>),
  {remove, Pointer};
parse_operation(V = #{<<"op">> := <<"replace">>}) ->
  Pointer = json_operation_pointer(V, <<"path">>),
  Value = json_operation_value(V, <<"value">>),
  {replace, Pointer, Value};
parse_operation(V = #{<<"op">> := <<"move">>}) ->
  Pointer = json_operation_pointer(V, <<"path">>),
  From = json_operation_pointer(V, <<"from">>),
  {move, From, Pointer};
parse_operation(V = #{<<"op">> := <<"copy">>}) ->
  Pointer = json_operation_pointer(V, <<"path">>),
  From = json_operation_pointer(V, <<"from">>),
  {copy, From, Pointer};
parse_operation(V = #{<<"op">> := <<"test">>}) ->
  Pointer = json_operation_pointer(V, <<"path">>),
  Value = json_operation_value(V, <<"value">>),
  {test, Pointer, Value};
parse_operation(#{<<"op">> := Op}) ->
  throw({error, {invalid_op, Op}});
parse_operation(#{}) ->
  throw({error, {missing_member, <<"op">>}});
parse_operation(_) ->
  throw({error, invalid_format}).

-spec json_operation_value(json:value(), json:key()) -> json:value().
json_operation_value(Value, Key) ->
  case maps:find(Key, Value) of
    {ok, Value2} ->
      Value2;
    error ->
      throw({error, {missing_member, Key}})
  end.

-spec json_operation_pointer(json:value(), json:key()) ->
        json_pointer:pointer().
json_operation_pointer(Value, Key) ->
  case json_operation_value(Value, Key) of
    String when is_binary(String) ->
      case json_pointer:parse(String) of
        {ok, Pointer} ->
          Pointer;
        {error, Reason} ->
          throw({error, {invalid_member, Key, {invalid_pointer, Reason}}})
      end;
    _ ->
      throw({error, {invalid_member, Key}})
  end.

-spec serialize(patch()) -> json:value().
serialize(Operations) ->
  lists:map(fun serialize_operation/1, Operations).

-spec serialize_operation(operation()) -> json:value().
serialize_operation({add, Path, Value}) ->
  #{<<"op">> => <<"add">>,
    <<"path">> => json_pointer:serialize(Path),
    <<"value">> => Value};
serialize_operation({remove, Path}) ->
  #{<<"op">> => <<"remove">>,
    <<"path">> => json_pointer:serialize(Path)};
serialize_operation({replace, Path, Value}) ->
  #{<<"op">> => <<"replace">>,
    <<"path">> => json_pointer:serialize(Path),
    <<"value">> => Value};
serialize_operation({move, From, Path}) ->
  #{<<"op">> => <<"move">>,
    <<"path">> => json_pointer:serialize(Path),
    <<"from">> => json_pointer:serialize(From)};
serialize_operation({copy, From, Path}) ->
  #{<<"op">> => <<"copy">>,
    <<"path">> => json_pointer:serialize(Path),
    <<"from">> => json_pointer:serialize(From)};
serialize_operation({test, Path, Value}) ->
  #{<<"op">> => <<"test">>,
    <<"path">> => json_pointer:serialize(Path),
    <<"value">> => Value}.

-spec execute(patch(), json:value()) ->
        {ok, json:value()} | {error, term()}.
execute([], Value) ->
  Value;
execute([Patch | Patchs], Value) ->
  case execute_operation(Patch, Value) of
    {ok, Value2} ->
      execute(Patchs, Value2);
    {error, Reason} ->
      {error, {Reason, Patch}}
  end.

-spec execute_operation(operation(), json:value()) ->
        {ok, json:value()} | {error, term()}.
execute_operation({add, [], NewValue}, _Value) ->
  {ok, NewValue};
execute_operation({add, Path, NewValue}, Value) ->
  json_pointer:insert(Path, Value, NewValue);
execute_operation({remove, Path}, Value) ->
  json_pointer:remove(Path, Value);
execute_operation({replace, Path, NewValue}, Value) ->
  json_pointer:replace(Path, Value, NewValue);
execute_operation({move, From, Path}, Value) ->
  case json_pointer:find(From, Value) of
    {ok, NewValue} ->
      case json_pointer:remove(From, Value) of
        {ok, Value2} ->
          json_pointer:insert(Path, Value2, NewValue);
        {error, Reason} ->
          {error, Reason}
      end;
    {error, Reason} ->
      {error, Reason}
  end;
execute_operation({copy, From, Path}, Value) ->
  case json_pointer:find(From, Value) of
    {ok, NewValue} ->
      json_pointer:insert(Path, Value, NewValue);
    {error, Reason} ->
      {error, Reason}
  end;
execute_operation({test, Path, ExpectedValue}, Value) ->
  case json_pointer:find(Path, Value) of
    {ok, ExpectedValue} ->
      {ok, ExpectedValue};
    {ok, _} ->
      {error, test_failure};
    {error, Reason} ->
      {error, Reason}
  end.
