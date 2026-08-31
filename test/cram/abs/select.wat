(module
  (import "owi" "i32_symbol" (func $i32_symbol (result i32)))

  (func $start
    i32.const 42
    i32.const 28

    i32.const 1
    select

    drop

    i32.const 42
    i32.const 28

    (call $i32_symbol)
    select

    drop
  )

  (start $start)
)
