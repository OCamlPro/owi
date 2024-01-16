(module
  (import "symbolic" "i32_symbol" (func $i32_symbol (result i32)))

  (table $t (ref func)
    (elem $f)
  )

  (type $type (func (param i32) (result i32)))

  (func $f (param $x i32) (result i32)
    (if (i32.eqz (local.get $x))
      (then unreachable))
    local.get $x
  )

  (func $start
    call $i32_symbol
    (call_indirect
      $t
      (type $type)
      (i32.const 0)
    )
    drop
  )

  (start $start)
)
