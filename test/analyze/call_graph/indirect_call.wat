(module
  (export "main" (func $main))
  (import "symbolic" "i32_symbol" (func $i32_symbol (result i32)))

  (type $type_f (func (param i32) (result i32)))
  (type $type_g (func (param i64) (result i64)))

  (import "env" "gl" (global $gl i32))

  (table $t 2 funcref)
  (elem (i32.const 0) $f)
  (elem  (global.get $gl) $g)

  (table $u 1 funcref)
  (table $v 1 funcref)
  (export "table" (table $v))

  (elem declare func $h)

  (func $f (param i32) (result i32)
    local.get 0
    local.get 0
    i32.add
  )


  (func $g (param i64) (result i64)
    local.get 0
    local.get 0
    i64.add
  )

  (func $h (param i64) (result i64)
    local.get 0
    local.get 0
    i64.add
  )

    (func $main

    i32.const 0
    ref.func $h
    table.set $u

    i32.const 42 
    i32.const 0  
    call_indirect $t (type $type_f)


    i64.const 42
    i32.const 1
    call_indirect $u (type $type_g)

    drop
    drop

  )


)
