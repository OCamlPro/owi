br instructions:
  $ owi opt br.wat > br.opt.wat
  $ cat br.opt.wat
  (module
    (type (func))
    (func $br
      i32.const 42
      br 0
    )
    (func $br_if
      br 0
    )
    (func $start
      call 0
      call 1
    )
    (start 2)
  )
  $ owi run br.opt.wat
