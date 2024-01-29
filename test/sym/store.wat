(module
  (import "symbolic" "i32_symbol" (func $i32_symbol (result i32)))

  (memory $m 1)

  (func $start
    ;; Objective: check 'out of bounds memory access' with symbolic idx store

    (local $idx i32)

    (local.tee $idx (call $i32_symbol))
    call $i32_symbol
    i32.store
  )

  (start $start)
)
