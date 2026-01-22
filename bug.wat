(module
  (import "owi" "alloc" (func  (param i32) (param i32) (result i32)))
  (import "owi" "i32_symbol" (func  (result i32)))
  (import "owi" "assume" (func  (param i32)))
  (global (mut i32) i32.const 8389648)
  (memory 129)
  (func (param i32) (result i32) (local i32) (local i32)
    (block
      local.get 0
      br_if 0
      i32.const 0
      return)
    i32.const 0
    i32.const 1
    local.get 0
    i32.clz
    i32.const 31
    i32.xor
    local.tee 1
    i32.shl
    i32.const 16
    local.get 1
    i32.const 5
    i32.lt_u
    select
    local.tee 1
    i32.const 0
    i32.load offset=1024 align=4
    local.tee 2
    local.get 1
    i32.const -1
    i32.add
    i32.and
    local.tee 1
    i32.sub
    i32.const 0
    local.get 1
    select
    local.get 2
    i32.add
    local.tee 1
    local.get 0
    i32.add
    i32.store offset=1024 align=4
    local.get 1
    local.get 0
    call 0
  )
  (func (result i32)
    call 1
  )
  (func (local i32) (local i32)
    i32.const 2000
    call 3
    local.set 0
    call 4
    local.tee 1
    i32.const 8
    i32.eq
    call 2
    (block
      local.get 1
      i32.const 3
      i32.lt_s
      br_if 0
      local.get 0
      i32.const 8
      i32.add
      local.set 0
      local.get 1
      i32.const -2
      i32.add
      local.set 1
      (loop
        local.get 0
        i32.const 0
        i32.store align=4
        local.get 0
        i32.const 4
        i32.add
        local.set 0
        local.get 1
        i32.const -1
        i32.add
        local.tee 1
        br_if 0))
  )
  (data (memory 0) (offset i32.const 1024) "\016\004\128\000")
  (start 5)
)
