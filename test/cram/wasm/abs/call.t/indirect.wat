(module
  (table $t (ref func)
    (elem $f)
  )

  (type $type (func (param i32) (result i32)))

  (func $f (param $x i32) (result i32)
    (if (i32.eqz (local.get $x))
      (then unreachable))
    i32.const 2
  )

  (func $start
    i32.const 1;; arg
    (call_indirect
      $t
      (type $type)
      (i32.const 0);; tbl_i
    )
    drop
  )

  (start $start)
)
