(module
  (func $start
    i32.const 42
    (loop $l
      nop
    )
    (block $l
      nop
    )
    drop
  )
  (start $start)
)
