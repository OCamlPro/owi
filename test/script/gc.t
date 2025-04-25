  $ owi script --no-exhaustion reference/proposals/gc/array.wast
  owi: [ERROR] unknown operator "array.get_s"
  [23]
  $ owi script --no-exhaustion reference/proposals/gc/br_on_cast_fail.wast
  owi: [ERROR] unknown operator "any.convert_extern"
  [23]
  $ owi script --no-exhaustion reference/proposals/gc/br_on_cast.wast
  owi: [ERROR] unknown operator "any.convert_extern"
  [23]
  $ owi script --no-exhaustion reference/proposals/gc/call_ref.wast
  owi: [ERROR] unknown type $ii
  [52]
  $ owi script --no-exhaustion reference/proposals/gc/extern.wast
  owi: [ERROR] unknown operator "any.convert_extern"
  [23]
  $ owi script --no-exhaustion reference/proposals/gc/i31.wast
  owi: [ERROR] unknown operator "i31ref"
  [23]
  $ owi script --no-exhaustion reference/proposals/gc/ref_cast.wast
  owi: [ERROR] unknown operator "any.convert_extern"
  [23]
  $ true # TODO: enable again: owi script --no-exhaustion reference/proposals/gc/ref_eq.wast
  $ owi script --no-exhaustion reference/proposals/gc/ref_test.wast
  owi: [ERROR] unknown operator "any.convert_extern"
  [23]
  $ owi script --no-exhaustion reference/proposals/gc/return_call_ref.wast
  owi: [ERROR] unknown type $i64-i64
  [52]
  $ owi script --no-exhaustion reference/proposals/gc/struct.wast
  owi: [ERROR] unknown operator "struct.get_u"
  [23]
  $ owi script --no-exhaustion reference/proposals/gc/type-subtyping.wast
  owi: [ERROR] unexpected token "ref"
  [40]
