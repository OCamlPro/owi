(module
  (import "owi" "i32_symbol" (func $i32_symbol (result i32)))

  (func $start
    (local $x i32)
    (local $y i32)

    call $i32_symbol
    local.set $x

    local.get $x
    i32.const 0
    i32.gt_s
    (if (result i32)
      (then
        ;; x > 0: y = x + 10
        local.get $x
        i32.const 10
        i32.add)
      (else
        ;; x <= 0: y = 0
        i32.const 0))
    local.set $y
  )

  (start $start)
)
