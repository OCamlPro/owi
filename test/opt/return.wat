(module

  (type $type (func (result i32)))

  (table $tab funcref
    (elem $return)
  )

  (func $return (type $type)
    i32.const 42
    return
    i32.const 42
    i32.const 42
    drop
    drop
  )

  (func $return_call (type $type)
    return_call $return
    i32.const 42
    i32.const 42
    drop
    drop
  )

  (func $return_call_indirect (result i32)
    (return_call_indirect $tab (type $type) (i32.const 0))
    i32.const 42
    i32.const 42
    drop
    drop
  )

  (func $start
    call $return
    drop
    call $return_call
    drop
    call $return_call_indirect
    drop
  )

  (start $start)
)
