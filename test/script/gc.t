  $ owi script --no-exhaustion reference/array.wast
  owi: [ERROR] unexpected token "i8"
  [40]
  $ owi script --no-exhaustion reference/br_on_cast_fail.wast
  owi: [ERROR] unexpected token "("
  [40]
  $ owi script --no-exhaustion reference/br_on_cast.wast
  owi: [ERROR] unexpected token "("
  [40]
  $ owi script --no-exhaustion reference/call_ref.wast
  owi: [ERROR] unexpected token ")"
  [40]
  $ owi script --no-exhaustion reference/extern.wast
  owi: [ERROR] unexpected token ")"
  [40]
  $ owi script --no-exhaustion reference/i31.wast
  owi: [ERROR] unexpected token ")"
  [40]
  $ owi script --no-exhaustion reference/ref_cast.wast
  owi: [ERROR] unexpected token ")"
  [40]
  $ true # TODO: enable again: owi script --no-exhaustion reference/ref_eq.wast
  $ owi script --no-exhaustion reference/ref_test.wast
  owi: [ERROR] unexpected token ")"
  [40]
  $ owi script --no-exhaustion reference/return_call_ref.wast
  owi: [ERROR] unexpected token ")"
  [40]
  $ owi script --no-exhaustion reference/struct.wast
  owi: [ERROR] unexpected token ")"
  [40]
  $ owi script --no-exhaustion reference/type-subtyping.wast
  owi: [ERROR] unexpected token "("
  [40]
