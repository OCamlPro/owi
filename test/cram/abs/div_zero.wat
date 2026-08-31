(module
  (import "owi" "i32_symbol" (func $i32_symbol (result i32)))

  (func $start
    (local $d i32)

    call $i32_symbol
    local.set $d

    i32.const 42
    local.get $d
    i32.div_s
    drop
  )

  (start $start)
)
