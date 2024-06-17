(module
  (type $sig (func (param i64) (result i64)))

  (table $t0 1 10 funcref)
  (elem (i32.const 0) $fct)

  (func $fct (type $sig)
    local.get 0
    i64.const -1
    i64.add
  )

  (func $start
    i64.const 1
    i32.const 0
    call_indirect (type $sig)
    drop
  )

  (start $start)
)
