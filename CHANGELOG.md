% erl-json changelog

# Next Version
## Features
- Add optional indentation for serialization.
- Add optional formatting for serialization.

# 1.3.0
## Features
- Add the `json:array/0` and `json:object/0` types.
- Add error formatting functions.
## Bug fixes
- Reject non-escaped control characters as required by RFC 8259.
- Reject invalid UTF-8 sequences.
## Misc
- Remove the extra space before object values in the serializer.
- Make the parser about 7.5 times faster (tested on the Kubernetes OpenAPI
  schema: 5.3MB, 100+k lines).

# 1.2.0
## Features
- Add `json_pointer:parent/1` and `json_pointer:child/2`.
- Add custom serializers for values of the form `{Type, Value}`.
- Accept binaries, atoms and strings as object keys.
- Replace `json_pointer:eval/2` and `json_pointer:eval_pointer/2` by
  `json_pointer:find/2`, `json_pointer:insert/2` and
  `json_pointer:replace/2`. These three functions accept both a binary JSON
  pointer or a parsed JSON pointer.
- Introduce support for JSON Patch.

# 1.1.0
## Features
- [JSON Pointer](https://tools.ietf.org/html/rfc6901) support.
## Bug fixes
- JSON strings are now always parsed as UTF-8 encoded binaries.

# 1.0.0
First public version.
