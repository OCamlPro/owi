  $ owi script --no-exhaustion reference/array_copy.wast 2>&1 | grep -oE "Failure.*"
  Failure("TODO: unimplemented instruction interpretation: array.copy 1 0")
  $ owi script --no-exhaustion reference/array_fill.wast 2>&1 | grep -oE "Failure.*"
  Failure("TODO: unimplemented instruction interpretation: array.fill 1")
  $ owi script --no-exhaustion reference/array_init_data.wast 2>&1 | grep -oE "Failure.*"
  Failure("TODO: unimplemented instruction interpretation: array.init_data 1 0")
  $ owi script --no-exhaustion reference/array_init_elem.wast 2>&1 | grep -oE "Failure.*"
  Failure("TODO: unimplemented instruction interpretation: array.init_elem 0 0")
  $ owi script --no-exhaustion reference/array_new_data.wast 2>&1 | grep -oE "Failure.*"
  Failure("TODO: unimplemented instruction interpretation: array.new_data 0 0")
  $ owi script --no-exhaustion reference/array_new_elem.wast 2>&1 | grep -oE "Failure.*"
  Failure("TODO: unimplemented instruction interpretation: array.new_elem 0 0")
  $ owi script --no-exhaustion reference/array.wast 2>&1 | grep -oE "Failure.*"
  Failure("TODO: unimplemented instruction interpretation: array.new_default 0")
  $ owi script --no-exhaustion reference/br_on_cast_fail.wast 2>&1 | grep -oE "Failure.*"
  Failure("TODO: unimplemented instruction interpretation: ref.i31")
  $ owi script --no-exhaustion reference/br_on_cast.wast 2>&1 | grep -oE "Failure.*"
  Failure("TODO: unimplemented instruction interpretation: ref.i31")
  $ owi script --no-exhaustion reference/br_on_non_null.wast 2>&1 | grep -oE "Failure.*"
  Failure("TODO: unimplemented `call_ref`")
  $ owi script --no-exhaustion reference/br_on_null.wast 2>&1 | grep -oE "Failure.*"
  Failure("TODO: unimplemented `call_ref`")
  $ owi script --no-exhaustion reference/call_ref.wast 2>&1 | grep -oE "Failure.*"
  Failure("TODO: unimplemented `call_ref`")
  $ owi script --no-exhaustion reference/extern.wast 2>&1 | grep -oE "Failure.*"
  Failure("TODO: unimplemented instruction interpretation: ref.i31")
  $ owi script --no-exhaustion reference/i31.wast 2>&1 | grep -oE "Failure.*"
  Failure("TODO: unimplemented instruction interpretation: ref.i31")
  $ owi script --no-exhaustion reference/ref_cast.wast 2>&1 | grep -oE "Failure.*"
  Failure("TODO: unimplemented instruction interpretation: ref.i31")
  $ true # TODO: enable again: owi script --no-exhaustion reference/ref_eq.wast
  $ owi script --no-exhaustion reference/ref_test.wast 2>&1 | grep -oE "Failure.*"
  Failure("TODO: unimplemented instruction interpretation: ref.i31")
  $ owi script --no-exhaustion reference/return_call_ref.wast 2>&1 | grep -oE "Failure.*"
  Failure("TODO: unimplemented `call_ref`")
  $ owi script --no-exhaustion reference/struct.wast 2>&1 | grep -oE "Failure.*"
  Failure("TODO: unimplemented instruction interpretation: struct.new_default 0")
  $ owi script --no-exhaustion reference/type-subtyping.wast 2>&1
