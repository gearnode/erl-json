% erl-json

# Introduction
The erl-json project is an implementation of the [JSON](https://www.json.org)
data format.

# Conformance
The erl-json project tries to follow [RFC
8259](https://tools.ietf.org/html/rfc8259) as much as possible.

# Interface
## Parsing
The `json:parse/1` and `json:parse/2` functions are used to parse a JSON
binary and return an Erlang term.

For example, `json:parse(<<"[1, 2, 3]">>)` returns `{ok,[1,2,3]}`.

### Options
Parsing options are represented as a map. The following options are available:
- `depth_limit`: a positive integer indicating the maximal JSON element depth;
  when the limit is reached, an error with the `depth_limit_reached` reason is
  returned.
- `duplicate_key_handling`: an atom indicating how to handle duplicate keys in
  objects; valid values are:
  - `first`: keep the value associated with the first occurrence of the key.
  - `last`: keep the value associated with the last occurrence of the key.
  - `error`: return an error with the `{duplicate_key, Key}` reason.
  The default value is `last`.

### Errors
Parsing errors are returned as maps which may contain the following fields:
- `position`: a `{Line, Column}` tuple indicating the location of the error in
  the JSON source.
- `reason`: a value representing the cause of the error (mandatory).

## Serialization
The `json:serialize/1` and `json:serialize/2` functions are used to serialize
an Erlang term to a JSON string.

For example, `json:serialize([1, 2, 3], #{return_binary => true})` returns
`<<"[1, 2, 3]">>`.

Serialization functions will signal an error of the form `{invalid_value,
Term}` if an Erlang term cannot be represented in JSON.

## Serializers
While serialization functions usually handle terms which map directly to JSON
values, they also accept terms of the form `{Type, Value}`. To serialize
`Value`, the serializer looks for a serialization function associated with the
`Type` atom in a map containing all serializers. The map returned by
`json:default_serializers()` is used by default; a different map can be
provided with the `serializers` serialization option.

A serialization function takes a single argument, the value to serialize, and
must return one of the following two values:
- `{data, iodata()}`: data are inserted in the output document without any
  transformation;
- `{value, json:value()}`: the value is serialized as any other JSON value.

### Default serializers
The following serializers are available in the default serializer map:

| Type       | Value type            | Description                  |
| ----       | ----------            | -----------                  |
| `data`     | `iodata()`            | Raw data.                    |
| `date`     | `calendar:date()`     | RFC 3339 date string.        |
| `time`     | `calendar:time()`     | RFC 3339 simple time string. |
| `datetime` | `calendar:datetime()` | RFC 3339 datetime string.    |

### Options
Serialization options are represented as a map. The following options are available:
- `return_binary`: return the final document as a binary instead of an iodata
  value.
- `serializers`: a map associating types (as atoms) and serialization
  functions to be used as a replacement for the default serializer map.

## JSON Pointer
The `json_pointer:apply/2` function is used to parse a JSON Pointer string and
to apply it to a JSON value.

Example:
```erlang
json_pointer:eval(<<"/foo/2">>,
                  #{<<"foo">> => [1, 2, 3],
                    <<"bar">> => [4, 5]}).
```

# Types
JSON values and Erlang terms are mapped according to the following table:

| JSON value | Erlang term                   |
| ---------- | -----------                   |
| null       | `null`                        |
| boolean    | `true \| false`               |
| number     | `integer() \| float()`        |
| string     | `binary()`                    |
| array      | `[json:value()]`              |
| object     | `#{binary() := json:value()}` |
