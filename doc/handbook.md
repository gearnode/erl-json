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

The `json:format_error/1` function can be used to obtain a human-readable
error string from an error value.

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
The `json_pointer:find/2` function is used to obtain the value referenced by a
JSON pointer inside a JSON value.

Example:
```erlang
json_pointer:find(<<"/foo/2">>,
                  #{<<"foo">> => [1, 2, 3],
                    <<"bar">> => [4, 5]}).
```

## JSON Patch
The `json_patch:parse/1` function is used to to parse a patch made of a
sequence of JSON Patch operations from a list of JSON values.

The `json_patch:execute/2` function is then used to apply a patch to any JSON
value.

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

As an exception, the serializer accepts not only binaries but strings and
atoms as object keys, for convenience purposes. The parser will always return
object keys as binaries. Erlang terms of type `json:value()` used for anything
but direct serialization must use binaries for object keys.
