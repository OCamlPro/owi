# Weasel: WebAssembly Specification Language

## Custom Annotations

Weasel specifications are written directly in WebAssembly's text format as [custom annotations](https://webassembly.github.io/annotations/core/text/lexical.html#annotations). Compared to comments, embedding specifications in custom annotations provides the benefit of syntax highlighting, while keeping the source file compatable with the WebAssembly specification.

Custom annotations take the form `(@annotid content)`, where there is no space between `(@` and `annotid`. `annotid` could either be a sequence of characters or a double-quoted string, and `content` should be well-parenthesized. It is possible to have nested custom annotations.

## Parser Behavior

Custom annotations may appear anywhere inside WebAssembly text format files where spaces are allowed. They do not interfere with the semantics of WebAssembly, and parsers are free to discard custom annotations.

For applications where certain custom annotations carry specific meanings, additional grammatical restrictions may be imposed on the content of custom annotations. Their behavior in case of an error are implementation-defined. For example, Owi's implementation of Weasel emits a syntax error when encountering an ill-formed function contract.

<!-- $MDX file=ill_formed.wat -->
```wat
(module
  (@contract $f
    owi
    (ensures true)
  )
  (func $f)
)
```

```sh
$ owi instrument ill_formed.wat
unknown annotation clause owi
```

## Types of Annotations

In Weasel, `annotid` specifies the type of annotation. Different types of annotations follow slightly different grammatical rules and must appear in specific locations of the source code. Weasel defines two types of annotations:

- **function contract** should appear anywhere a module field is allowed (i.e. as a custom section). Though it is recommended to place the contract immediately before the corresponding function definition.

- **assertion** should appear anywhere an instruction is allowed.

## Grammar

Weasel adopts an S-expression-like syntax for the time being, mainly because it is easy to prototype. The syntax may evolve in the future (to an infix one, possibly).

We present the grammar of Weasel annotations in BNF form. The following notations are adopted:

- Terminal symbol is written between double quotations. `"terminal"`
- Nonterminal symbol is written as normal words. `nonterminal`
- Bracket is used to group symbols `{sym1 sym2 ...}`
- Asterisk denotes zero, one or more occurrences. `{sym}*`
- Plus denotes one or more occurrences. `{sym}+`
- Question mark denotes zero or one occurrence. `{sym}?`

```ocaml
ind ::= "$" id
    | integer

unop ::= "clz"        (on wasm_int_type)
    | "ctz"           (on wasm_int_type)
    | "popcnt"        (on wasm_int_type)
    | "abs"
    | "neg"
    | "sqrt"
    | "ceil"
    | "floor"
    | "trunc"         (on wasm_float_type or real)
    | "nearst"        (on wasm_float_type or real)

binop ::= "add" | "+"
    | "sub" | "-"
    | "mul" | "*"
    | "div" | "/"
    | "rem" | "%"     (on wasm_int_type or integer)
    | "min"
    | "max"
    | "and" | "&"     (on wasm_int_type or integer)
    | "or" | "|"      (on wasm_int_type or integer)
    | "xor" | "^"     (on wasm_int_type or integer)
    | "shl" | "<<"    (on wasm_int_type or integer)
    | "shr_s" | ">>"  (on wasm_int_type or integer)
    | "shr_x" | ">>>" (on wasm_int_type or integer)
    | "rotl"          (on wasm_int_type)
    | "rotr"          (on wasm_int_type)
    | "copysign"

memarg ::= "(" "base" term ")" {"(" "offset" term ")"}? {"(" "align" term ")"}?

term ::= "result"
    | integer
    | real
    | i32_max
    | i32_min
    | i64_max
    | i64_min
    | ind
    | pterm

pterm ::= "result" {integer}?
    | "old" term
    | "param" ind
    | "global" ind
    | "binder" ind
    | unop term
    | binop term term
    | "null" ref_type
    | "ref_func" ind
    | "table_get" ind
    | "table_size" ind
    | "i32_load" memarg
    | "i64_load" memarg
    | "f32_load" memarg
    | "f64_load" memarg
    | "i32_load8_s" memarg
    | "i32_load8_x" memarg
    | "i32_load16_s" memarg
    | "i32_load16_x" memarg
    | "i64_load8_s" memarg
    | "i64_load8_x" memarg
    | "i64_load16_s" memarg
    | "i64_load16_x" memarg
    | "i64_load32_s" memarg
    | "i64_load32_x" memarg
    | "memory_size" ind           (now there is only one memory)
    | "?:" term term term
    | "let" {id}? term term
    | "cast" numerical_type term
    | (* function application *)

unpred ::= "eqz" | "=0?"
    | "is_null"       (on ref_type)

binpred ::= "eq" | "="
    | "ne" | "!="
    | "lt" | "<"
    | "gt" | ">"
    | "le" | "<="
    | "ge" | ">="

prop ::= "true"
    | "false"
    | "(" pprop ")"

pprop ::= "old" prop
    | "!" prop
    | "&&" prop prop
    | "||" prop prop
    | "^^" prop prop
    | "==>" prop prop
    | "<==>" prop prop
    | "?:" prop prop prop
    | "let" {id}? term prop
    | unpred prop
    | binpred prop prop
    | "forall" numerical_type {id}? prop
    | "exists" numerical_type {id}? prop
    | (* predicate application *)

wasm_int_type ::= "i32"
    | "i64"

wasm_float_type ::= "f32"
    | "f64"

wasm_type ::= wasm_int_type
    | wasm_float_type

numerical_type ::= wasm_type
    | "integer"
    | "real"

ref_type ::= "funcref"
    | "externref"

assumes_clause ::= "(" "assumes" prop ")"

requires_clause ::= "(" "requires" prop ")"

assigns_clause ::= "(" "assigns" {"nothing" | memarg} ")"

ensures_clause ::= "(" "ensures" prop ")"

simple_behavior ::= {requires_clause | assigns_clause | ensures_clause}*

named_behavior ::= "(" "behavior" {assumes_clause | requires_clause | assigns_clause | ensures_clause}* ")"

contract ::= "(@contract" ind simple_behavior {named_behavior}* ")"

assertion ::= "(@assert" prop ")"

annotation ::= contract
    | assertion

```

TODO:
- vector type
- application of (pure) functions through call, call_indirect, call_ref
- stack access in assertion
- make memory access syntax more lightweight
