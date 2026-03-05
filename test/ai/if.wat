(module
  (func $start
    i32.const 42
    i32.const 28

    i32.const 0
    (if (param i32) (param i32) (result i32) (then i32.add) (else i32.sub))

    drop
  )

  (start $start)
)
