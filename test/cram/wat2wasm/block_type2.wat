(module
  (func (result i32)
    (local i32)
    i32.const 0
    local.tee 0
    (loop (param i32) (result i32)
      i32.const 1
      i32.add
      local.tee 0
      local.get 0
      i32.const 10
      i32.le_s
      br_if 0
    )
  )
  (func
    call 0
    drop
  )
  (start 1)
)
