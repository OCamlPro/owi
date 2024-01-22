(assert_invalid
  (module $m
    (start 0)
    (func $start
      i32.const 1
      i64.const 2

      i32.const 42
      (if  (param  i64) (param  i32)
        (then
          drop
          drop
        )
        (else
          drop
          drop
        )
      )
    )
  )
  "type mismatch"
)

(assert_invalid
  (module $m
    (start 0)
    (func $start
      i32.const 1
      i64.const 2

      (block  (param  i64) (param  i32)
          drop
          drop

      )
    )
  )
  "type mismatch"
)
