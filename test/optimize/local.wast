(module
  (func $f0 (local i32) (local i32) (local i32) (local i32)
    i32.const 0
    local.set 0
  )

  (func $f1 (local i32) (local i32) (local i32) (local i32)
    i32.const 0
    local.set 1
  )

  (func $f2 (local i32) (local i32) (local i32) (local i32)
    i32.const 0
    local.set 2
  )

  (func $f3 (local i32) (local i32) (local i32) (local i32)
    i32.const 0
    local.set 3
  )

  (func $f12 (local i32) (local i32) (local i32) (local i32)
    i32.const 0
    local.set 1
    i32.const 0
    local.set 2
  )

  (func $f23 (local i32) (local i32) (local i32) (local i32)
    i32.const 0
    local.set 2
    i32.const 0
    local.set 3
  )

  (func $f023 (local i32) (local i32) (local i32) (local i32)
    i32.const 0
    local.set 0
    i32.const 0
    local.set 2
    i32.const 0
    local.set 3
  )

  (func $start
    call $f0
    call $f1
    call $f2
    call $f3
    call $f12
    call $f23
    call $f023
  )
  
  (start $start)
)
