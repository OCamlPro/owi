(module
  (import "owi" "i32_symbol" (func $i32_symbol (result i32)))

  (func $start

    (local $x i32)
    (local $y i32)

    (local.set $x (call $i32_symbol))
    (local.set $y (call $i32_symbol))

    local.get $x
    local.get $y
    i32.div_s     ;; MIN_INT/(-1) != MAX_INT -- MAX_INT = (-MIN_INT)-1
    drop
  )

  (start $start)
)
