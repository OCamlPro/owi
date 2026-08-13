  $ owi script concrete --no-exhaustion reference/array_copy.wast 2>&1 | grep -oE "Failure.*"
  Failure("TODO: unimplemented instruction interpretation: array.copy 1 0")
  $ owi script concrete --no-exhaustion reference/array_fill.wast 2>&1 | grep -oE "Failure.*"
  Failure("TODO: unimplemented instruction interpretation: array.fill 1")
  $ owi script concrete --no-exhaustion reference/array_init_data.wast 2>&1 | grep -oE "Failure.*"
  Failure("TODO: unimplemented instruction interpretation: array.init_data 1 0")
  $ owi script concrete --no-exhaustion reference/array_init_elem.wast 2>&1 | grep -oE "Failure.*"
  Failure("TODO: unimplemented instruction interpretation: array.init_elem 0 0")
  $ owi script concrete --no-exhaustion reference/array_new_data.wast 2>&1 | grep -oE "Failure.*"
  Failure("TODO: unimplemented instruction interpretation: array.new_data 0 0")
  $ owi script concrete --no-exhaustion reference/array_new_elem.wast 2>&1 | grep -oE "Failure.*"
  Failure("TODO: unimplemented instruction interpretation: array.new_elem 0 0")
  $ owi script concrete --no-exhaustion reference/array.wast 2>&1 | grep -oE "Failure.*"
  Failure("TODO: unimplemented instruction interpretation: array.new_data 0 0")
  $ owi script concrete --no-exhaustion reference/br_on_cast_fail.wast
  $ owi script concrete --no-exhaustion reference/br_on_cast.wast
  $ owi script concrete --no-exhaustion reference/br_on_non_null.wast
  $ owi script concrete --no-exhaustion reference/br_on_null.wast
  $ owi script concrete --no-exhaustion reference/call_ref.wast
  $ owi script concrete --no-exhaustion reference/extern.wast
  $ owi script concrete --no-exhaustion reference/i31.wast
  $ owi script concrete --no-exhaustion reference/ref_cast.wast
  $ owi script concrete --no-exhaustion reference/ref_eq.wast
  $ owi script concrete --no-exhaustion reference/ref_test.wast
  $ owi script concrete --no-exhaustion reference/return_call_ref.wast
  $ owi script concrete --no-exhaustion reference/struct.wast
  $ owi script concrete --no-exhaustion reference/type-subtyping.wast
