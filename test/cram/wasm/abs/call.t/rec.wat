(module
  (func $incr (param $i i32) (result i32)
    i32.const 1
    local.get $i
    i32.add 
    call $start
  )

  (func $start
    i32.const 1
    call $incr
    call $incr
    drop
  )

	(start $start)
)
