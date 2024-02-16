(module
  (import "symbolic" "i32_symbol" (func $i32_symbol (result i32)))
  (import "symbolic" "assume" (func $assume (param i32)))
  (import "symbolic" "assert" (func $assert (param i32)))

  (memory $m 1)
  (data (offset (i32.const 0)) "\00\01\02\03")

  (func $start

    (local $n i32)

    (local.set $n (call $i32_symbol))

    (call $assume   ;; 0 <= i32 < 4
      (i32.and
        (i32.ge_u (local.get $n) (i32.const 0))
        (i32.lt_u (local.get $n) (i32.const 4))
        )
    )

    (call $assert
      (i32.eq
        (i32.load8_u (local.get $n))
        (local.get $n)
      )
    )
  )

  (start $start)
)
