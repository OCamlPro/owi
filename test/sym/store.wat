(module
  (import "symbolic" "i32_symbol" (func $i32_symbol (result i32)))
  (import "symbolic" "assume" (func $owi_assume (param i32)))

  (memory $m 1)

  (func $start
    ;; Objective: check 'out of bounds memory access' with symbolic idx store

    (local $idx i32)

    (local.set  $idx (call $i32_symbol))

    ;; Hackish way to reduce the size of the search space. Otherwise, all possible
    ;; values of idx between 0 and <page_size> get checked.
    (call $owi_assume
      (i32.eqz (i32.rem_s (local.get $idx) (i32.const 4096)))
    )

    (i32.store (local.get $idx) (i32.const 42))
  )

  (start $start)
)
