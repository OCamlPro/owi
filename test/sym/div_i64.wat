(module
  (import "owi" "i64_symbol" (func $i64_symbol (result i64)))

  (func $start

    (local $x i64)
    (local $y i64)

    (local.set $x (call $i64_symbol))
    (local.set $y (call $i64_symbol))

    local.get $x
    local.get $y
    i64.div_s     ;; MIN_INT/(-1) != MAX_INT -- MAX_INT = (-MIN_INT)-1
    drop
  )

  (start $start)
)
