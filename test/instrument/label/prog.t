  $ owi instrument label --criteria=fc prog.wat
  generated 1 labels!
  $ cat prog.instrumented.wat
  (module
    (import "owi" "cov_label_set" (func  (param i32) (param i32)))
    (type (func))
    (type (func (result i32)))
    (func $f
      i32.const 0
      call 0
      i32.const 42
      (if (result i32)
        (then
          i32.const 12
        )
        (else
          i32.const 24
        )
      )
      i32.const 0
      i32.mul
      unreachable
    )
  )
  $ owi instrument label --criteria=sc prog.wat
  generated 7 labels!
  $ cat prog.instrumented.wat
  (module
    (import "owi" "cov_label_set" (func  (param i32) (param i32)))
    (type (func))
    (type (func (result i32)))
    (func $f
      i32.const 0
      call 0
      i32.const 42
      i32.const 1
      call 0
      (if (result i32)
        (then
          i32.const 2
          call 0
          i32.const 12
        )
        (else
          i32.const 3
          call 0
          i32.const 24
        )
      )
      i32.const 4
      call 0
      i32.const 0
      i32.const 5
      call 0
      i32.mul
      i32.const 6
      call 0
      unreachable
    )
  )
  $ owi instrument label --criteria=dc prog.wat
  generated 2 labels!
  $ cat prog.instrumented.wat
  (module
    (import "owi" "cov_label_set" (func  (param i32) (param i32)))
    (type (func))
    (type (func (result i32)))
    (func $f
      i32.const 42
      (if (result i32)
        (then
          i32.const 0
          call 0
          i32.const 12
        )
        (else
          i32.const 1
          call 0
          i32.const 24
        )
      )
      i32.const 0
      i32.mul
      unreachable
    )
  )
