(module
  (func (result i32)
    i32.const 42
    (block (result i32) (i32.const 1))
    i32.add
  )
)

(module
  (func
        i32.const 2
        (block (result i32) (i32.const 1))
        i32.const 3
        select
        drop
  )
)

(assert_invalid
  (module
    (func (result i32)
      (block
        (i32.const 0)
      ))
  )
  "type mismatch"
)

(module
  (func (param i32) (result i32)
        (result i64)
    i32.const 50
    i64.const 51
    local.get 0
    br_if 0
    drop
    drop
    i32.const 51
    i64.const 52
  )
)
