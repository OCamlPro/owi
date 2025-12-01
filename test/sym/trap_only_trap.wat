;; check that trap fails in trap only mode
(module
  (import "owi" "i32_symbol" (func $i32_symbol (result i32)))
  (func $start
    call $i32_symbol
    i32.const 0
    i32.div_s
    drop
  )
  (start $start)
)
