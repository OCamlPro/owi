(module
  (import "owi" "i32_symbol" (func $i32_symbol (result i32)))
  (func $start
    call $i32_symbol
    i32.const 42
    i32.lt_s
    (if (then
      unreachable
    ))
  )
  (start $start)
)
