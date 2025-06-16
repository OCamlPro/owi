(module

  (import "owi" "i32_symbol" (func $i32_symbol (result i32)))

  (func $start
    (i32.eq (i32.const 42) (call $i32_symbol))
    (if (then unreachable)))

  (start $start))
