(module
  (func $start
    i32.const 42
    i32.const 28

    i32.const 0
    (if (then i32.add) (else i32.sub))

    return
  )

  (start $start)
)
