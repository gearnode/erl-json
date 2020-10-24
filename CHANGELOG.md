% erl-json changelog

# Next Version
## Features
- Add `json_pointer:parent/1` and `json_pointer:child/2`.
- Add custom serializers for values of the form `{Type, Value}`.
- Accept binaries, atoms and strings as object keys.
- Replace `json_pointer:eval/2` and `json_pointer:eval_pointer/2` by
  `json_pointer:find/2`, `json_pointer:insert/2` and
  `json_pointer:replace/2`. These three functions accept both a binary JSON
  pointer or a parsed JSON pointer.

# 1.1.0
## Features
- [JSON Pointer](https://tools.ietf.org/html/rfc6901) support.
## Bug fixes
- JSON strings are now always parsed as UTF-8 encoded binaries.

# 1.0.0
First public version.
