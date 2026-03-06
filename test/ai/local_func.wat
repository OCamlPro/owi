(module
  (import "owi" "i32_symbol" (func $i32_symbol (result i32)))

  (func $add (param $i i32) (result i32)
    local.get $i
    call $i32_symbol
    i32.add
    return
  )

  (func $start
    i32.const 42
    call $add
    return
  )

  (start $start)
)
