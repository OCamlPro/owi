  $ owi script --no-exhaustion reference/proposals/gc/array.wast
  unknown operator "array.get_s"
  [23]
  $ owi script --no-exhaustion reference/proposals/gc/br_on_cast_fail.wast
  unknown operator "any.convert_extern"
  [23]
  $ owi script --no-exhaustion reference/proposals/gc/br_on_cast.wast
  unknown operator "any.convert_extern"
  [23]
  $ owi script --no-exhaustion reference/proposals/gc/call_ref.wast
  unknown type $ii
  [52]
  $ owi script --no-exhaustion reference/proposals/gc/extern.wast
  unknown operator "any.convert_extern"
  [23]
  $ owi script --no-exhaustion reference/proposals/gc/i31.wast
  unknown operator "i31ref"
  [23]
  $ owi script --no-exhaustion reference/proposals/gc/ref_cast.wast
  unknown operator "any.convert_extern"
  [23]
  $ true # TODO: enable again: owi script --no-exhaustion reference/proposals/gc/ref_eq.wast
  $ owi script --no-exhaustion reference/proposals/gc/ref_test.wast
  unknown operator "any.convert_extern"
  [23]
  $ owi script --no-exhaustion reference/proposals/gc/return_call_ref.wast
  unknown type $i64-i64
  [52]
  $ owi script --no-exhaustion reference/proposals/gc/struct.wast
  unknown operator "struct.get_u"
  [23]
  $ owi script --no-exhaustion reference/proposals/gc/type-subtyping.wast
  unexpected token "ref"
  [40]
