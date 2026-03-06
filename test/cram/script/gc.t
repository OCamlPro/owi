  $ owi script --no-exhaustion reference/array_copy.wast 2>&1 | grep -oE "Failure.*"
  Failure("Assigned: unimplemented for rec and sub types")
  $ owi script --no-exhaustion reference/array_fill.wast 2>&1 | grep -oE "Failure.*"
  Failure("Assigned: unimplemented for rec and sub types")
  $ owi script --no-exhaustion reference/array_init_data.wast 2>&1 | grep -oE "Failure.*"
  Failure("Assigned: unimplemented for rec and sub types")
  $ owi script --no-exhaustion reference/array_init_elem.wast 2>&1 | grep -oE "Failure.*"
  Failure("Assigned: unimplemented for rec and sub types")
  $ owi script --no-exhaustion reference/array_new_data.wast 2>&1 | grep -oE "Failure.*"
  Failure("Assigned: unimplemented for rec and sub types")
  $ owi script --no-exhaustion reference/array_new_elem.wast 2>&1 | grep -oE "Failure.*"
  Failure("Assigned: unimplemented for rec and sub types")
  $ owi script --no-exhaustion reference/array.wast 2>&1 | grep -oE "Failure.*"
  Failure("Assigned: unimplemented for rec and sub types")
  $ owi script --no-exhaustion reference/br_on_cast_fail.wast
  owi: [ERROR] unexpected token "$l" in line 30, character 23-25
  [40]
  $ owi script --no-exhaustion reference/br_on_cast.wast
  owi: [ERROR] unexpected token "$l" in line 30, character 18-20
  [40]
  $ owi script --no-exhaustion reference/call_ref.wast 2>&1 | grep -oE "Failure.*"
  Failure("TODO")
  $ owi script --no-exhaustion reference/extern.wast 2>&1 | grep -oE "Failure.*"
  Failure("Assigned: unimplemented for rec and sub types")
  $ owi script --no-exhaustion reference/i31.wast 2>&1 | grep -oE "Failure.*"
  [1]
  $ owi script --no-exhaustion reference/ref_cast.wast 2>&1 | grep -oE "Failure.*"
  Failure("Assigned: unimplemented for rec and sub types")
  $ true # TODO: enable again: owi script --no-exhaustion reference/ref_eq.wast
  $ owi script --no-exhaustion reference/ref_test.wast 2>&1 | grep -oE "Failure.*"
  Failure("Assigned: unimplemented for rec and sub types")
  $ owi script --no-exhaustion reference/return_call_ref.wast 2>&1 | grep -oE ".*Assertion failed"
  [1]
  $ owi script --no-exhaustion reference/struct.wast 2>&1 | grep -oE "Failure.*"
  Failure("Assigned: unimplemented for rec and sub types")
  $ owi script --no-exhaustion reference/type-subtyping.wast 2>&1 | grep -oE "Failure.*"
  Failure("Assigned: unimplemented for rec and sub types")
