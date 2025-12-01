;; check that assertion does not fail in trap only mode
(module
  (import "owi" "i32_symbol" (func $i32_symbol (result i32)))
  (import "owi" "assert" (func $assert_i32 (param i32)))
  (func $start
    call $i32_symbol
    call $assert_i32
  )
  (start $start)
)
