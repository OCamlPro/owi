(module
  (import "symbolic" "assert" (func $assert (param i32)))
  (memory $m 1)
  (func $start
    i32.const 0x00000000
    i32.load
    i32.const 0x00000000
    i32.eq
    call $assert)
  (start $start))
