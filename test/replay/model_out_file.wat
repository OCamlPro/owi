(module
  (import "symbolic" "range_symbol" (func $owi_range (param i32) (param i32) (result i32)))
  (import "symbolic" "assert" (func $owi_assert (param i32)))

  (start $start)
  (func $start
    (local $x i32)

    (local.set $x (call $owi_range (i32.const 10) (i32.const 20)))
    (call $owi_assert (i32.ge_u (local.get $x) (i32.const 20)))
  )
)
