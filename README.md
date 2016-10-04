# integrated-query-language

This repository contains an AST, Parser and Evaluator for a simple JSON query and transform language, implemented in scala using fastparse for parser combinators.

## Example

```
.foo.bar = "hello"
.foo.baz = .bar
```

This, when evaluated and run with some JSON input, would output JSON with the string "hello" inserted into the "bar" field inside "foo", and the contents of "bar" from the input inserted into "baz" inside "foo".
