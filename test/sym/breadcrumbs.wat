(module
  (import "symbolic" "i32_symbol" (func $i32_symbol (result i32)))
  (import "symbolic" "assume" (func $assume (param i32)))
  (import "symbolic" "assert" (func $assert (param i32)))

  (func $start
    (local $x i32)

    (local.set $x (call $i32_symbol))
    (call $assume (i32.ge_s (local.get $x) (i32.const 200)))


    (if (i32.gt_s (local.get $x) (i32.const 500))
      (then
        ;; empty block
      )
      (else
        (if (i32.gt_s (local.get $x) (i32.const 400))
          (then
            ;; empty block
          )
          (else
            (if (i32.gt_s (local.get $x) (i32.const 300))
              (then
                ;; empty block
              )
              (else
                (if (i32.gt_s (local.get $x) (i32.const 150))
                  (then
                    (if (i32.gt_s (local.get $x) (i32.const 100))
                      (then
                        (call $assert (i32.const 0))
                      )
                    )
                  )
                  (else
                    ;; empty block
                  )
                )
              )
            )
          )
        )
      )
    )
  )
  (start $start)
)
