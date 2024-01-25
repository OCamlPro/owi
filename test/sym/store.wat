(module
  (import "symbolic" "i32_symbol" (func $i32_symbol (result i32)))

  (memory $m 1)

  (func $mem_set_i32 (param $idx i32) (param $val i32)
    local.get $idx
    local.get $val
    i32.store
  )

  (func $start
    (local $idx i32)
    (local.set $idx (call $i32_symbol))
    ;; check 'out of bounds memory access' with symbolic idx store
    (call $mem_set_i32 (local.get $idx) (call $i32_symbol))
  )

  (start $start)
)
