;; Checks that all integer divisions by zero trap
;; but not floating point ones
;; Regression-test for https://github.com/OCamlPro/owi/issues/128
(module
  (type $void_t (func))
  (import "symbolic" "i32_symbol" (func $i32_symbol (result i32)))
  (import "symbolic" "i64_symbol" (func $i64_symbol (result i64)))
  (import "symbolic" "f32_symbol" (func $f32_symbol (result f32)))
  (import "symbolic" "f64_symbol" (func $f64_symbol (result f64)))
  (import "symbolic" "assume" (func $assume (param i32)))

  (table $func_table 6 6 funcref)

  (elem (i32.const 0) $i32_block)
  (elem (i32.const 1) $i32_block_u)
  (elem (i32.const 2) $i64_block)
  (elem (i32.const 3) $i64_block_u)
  (elem (i32.const 4) $f32_block)
  (elem (i32.const 5) $f64_block)

  (func $start

    (local $n i32)

    (local.set $n (call $i32_symbol))

    (call $assume 
      (i32.and
        (i32.ge_u (local.get $n) (i32.const 0))
        (i32.lt_u (local.get $n) (i32.const 6))
        )
    )

    (local.get $n)

    call_indirect  (type $void_t) 
  )

  (func $i32_block (type $void_t)
    i32.const 1
    (call $i32_symbol)
    i32.div_s
    drop
    return
  )

   (func $i32_block_u (type $void_t)
    i32.const 1
    (call $i32_symbol)
    i32.div_u
    drop
    return
  )

  (func $i64_block (type $void_t)
    i64.const 1
    (call $i64_symbol)
    i64.div_s
    drop
    return
  )

    (func $i64_block_u (type $void_t)
    i64.const 1
    (call $i64_symbol)
    i64.div_u
    drop
    return
  )


  (func $f32_block (type $void_t)
    f32.const 1
    (call $f32_symbol)
    f32.div
    drop
    return
  )

  (func $f64_block (type $void_t)
    f64.const 1
    (call $f64_symbol)
    f64.div
    drop
    return
  )

  (start $start)
)
