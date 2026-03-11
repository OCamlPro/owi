(module
  (func $add (param $i i32) (result i32)
    local.get $i
    local.tee 0 (; should be top ;)
    i32.add
    return
  )

  (func $start
    i32.const 42
    call $add
    return
  )

  (start $start)
)
