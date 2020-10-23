% erl-json changelog

# Next Version
## Features
- Add `json_pointer:parent/1` and `json_pointer:child/2`.
- Add custom serializers for values of the form `{Type, Value}`.
- Accept binaries, atoms and strings as object keys.
- `json_pointer:eval/2` now supports both serialized JSON pointers and parsed
  JSON pointers; `json_pointer:eval_pointer/2` is therefore redundant and has
  been removed.

# 1.1.0
## Features
- [JSON Pointer](https://tools.ietf.org/html/rfc6901) support.
## Bug fixes
- JSON strings are now always parsed as UTF-8 encoded binaries.

# 1.0.0
First public version.
