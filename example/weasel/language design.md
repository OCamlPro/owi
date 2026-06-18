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

## Type of Annotation

In Weasel, `annotid` specifies the type of annotation. Different types of annotations follow slightly different grammatical rules and must appear in specific locations of the source code. Currently, Weasel define only one type of annotation:

- **function contract** should appear anywhere a module field is allowed (i.e. as a custom section). Though it is recommended to place the contract immediately before the corresponding function definition.

## Grammar

Weasel adopts an S-expression-like syntax for the time being, mainly because it is easy to prototype. The syntax may evolve in the future (to an infix one, possibly).

The grammar of Weasel annotations is specified [here](grammar.ebnf) in [EBNF-ISO form](https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_form#Table_of_symbols).

## Future works

- big number
- specification of loop invariant
