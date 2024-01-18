(module
  (import "symbolic" "i32_symbol" (func $i32_symbol (result i32)))

  (table $t (ref func)
    (elem $f)
  )

  (type $type (func (param i32)))

  (func $f (param $x i32)
    (if (i32.eqz (local.get $x))
      (then unreachable))
  )

  (func $start
    call $i32_symbol
    (call_indirect
      $t
      (type $type)
      (call $i32_symbol)
    )
  )

  (start $start)
)
