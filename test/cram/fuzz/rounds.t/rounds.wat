(module
  (import "owi" "i32_symbol" (func $i32_symbol (result i32)))
  (func $start
    call $i32_symbol
    drop
  )
  (start $start)
)
