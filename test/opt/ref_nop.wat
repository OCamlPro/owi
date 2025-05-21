(module
  (type $t (func))
  (func $start
    ref.null func
    ref.is_null
    nop
    i32.const 42
    drop
    drop
  )
  (start $start)
)
