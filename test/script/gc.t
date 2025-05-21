  $ owi script --no-exhaustion reference/proposals/gc/array.wast
  owi: [ERROR] unexpected token "i8"
  [40]
  $ owi script --no-exhaustion reference/proposals/gc/br_on_cast_fail.wast
  owi: [ERROR] unexpected token "("
  [40]
  $ owi script --no-exhaustion reference/proposals/gc/br_on_cast.wast
  owi: [ERROR] unexpected token "("
  [40]
  $ owi script --no-exhaustion reference/proposals/gc/call_ref.wast
  owi: [ERROR] unexpected token ")"
  [40]
  $ owi script --no-exhaustion reference/proposals/gc/extern.wast
  owi: [ERROR] unexpected token ")"
  [40]
  $ owi script --no-exhaustion reference/proposals/gc/i31.wast
  owi: [ERROR] unexpected token ")"
  [40]
  $ owi script --no-exhaustion reference/proposals/gc/ref_cast.wast
  owi: [ERROR] unexpected token ")"
  [40]
  $ true # TODO: enable again: owi script --no-exhaustion reference/proposals/gc/ref_eq.wast
  $ owi script --no-exhaustion reference/proposals/gc/ref_test.wast
  owi: [ERROR] unexpected token ")"
  [40]
  $ owi script --no-exhaustion reference/proposals/gc/return_call_ref.wast
  owi: [ERROR] unexpected token ")"
  [40]
  $ owi script --no-exhaustion reference/proposals/gc/struct.wast
  owi: [ERROR] unexpected token ")"
  [40]
  $ owi script --no-exhaustion reference/proposals/gc/type-subtyping.wast
  owi: [ERROR] unexpected token "("
  [40]
