  $ owi script --no-exhaustion reference/array.wast
  owi: [ERROR] unexpected token "i8" in line 4, character 15-17
  [40]
  $ owi script --no-exhaustion reference/br_on_cast_fail.wast
  owi: [ERROR] unexpected token "(" in line 5, character 20-21
  [40]
  $ owi script --no-exhaustion reference/br_on_cast.wast
  owi: [ERROR] unexpected token "(" in line 5, character 20-21
  [40]
  $ owi script --no-exhaustion reference/call_ref.wast
  owi: [ERROR] unexpected token ")" in line 4, character 33-34
  [40]
  $ owi script --no-exhaustion reference/extern.wast
  owi: [ERROR] unexpected token ")" in line 3, character 19-20
  [40]
  $ owi script --no-exhaustion reference/i31.wast
  owi: [ERROR] unexpected token ")" in line 2, character 54-55
  [40]
  $ owi script --no-exhaustion reference/ref_cast.wast
  owi: [ERROR] unexpected token ")" in line 5, character 19-20
  [40]
  $ true # TODO: enable again: owi script --no-exhaustion reference/ref_eq.wast
  $ owi script --no-exhaustion reference/ref_test.wast
  owi: [ERROR] unexpected token ")" in line 5, character 19-20
  [40]
  $ owi script --no-exhaustion reference/return_call_ref.wast
  owi: [ERROR] unexpected token ")" in line 38, character 31-32
  [40]
  $ owi script --no-exhaustion reference/struct.wast
  owi: [ERROR] unexpected token ")" in line 4, character 15-16
  [40]
  $ owi script --no-exhaustion reference/type-subtyping.wast
  owi: [ERROR] unexpected token "(" in line 4, character 17-18
  [40]
