;; Checks that all integer divisions by zero trap
;; but not floating point ones
;; Regression-test for https://github.com/OCamlPro/owi/issues/128
(module
  (type $void_t (func))
  (import "owi" "i32_symbol" (func $i32_symbol (result i32)))
  (import "owi" "i64_symbol" (func $i64_symbol (result i64)))
  (import "owi" "f32_symbol" (func $f32_symbol (result f32)))
  (import "owi" "f64_symbol" (func $f64_symbol (result f64)))
  (import "owi" "assume" (func $assume (param i32)))

  (table $func_table  funcref
    (elem $i32_block $i32_block_u $i64_block $i64_block_u $f32_block $f64_block)
  )

  (func $start

    (local $n i32)

    (local.set $n (call $i32_symbol))

    (call $assume
      (i32.and
        (i32.ge_u (local.get $n) (i32.const 0))
        (i32.lt_u (local.get $n) (table.size $func_table))
        )
    )

   (call_indirect (type $void_t) (local.get $n))
  )

  (func $i32_block (type $void_t)
    i32.const 1
    call $i32_symbol
    i32.div_s
    drop
  )

   (func $i32_block_u (type $void_t)
    i32.const 1
    call $i32_symbol
    i32.div_u
    drop
  )

  (func $i64_block (type $void_t)
    i64.const 1
    call $i64_symbol
    i64.div_s
    drop
  )

    (func $i64_block_u (type $void_t)
    i64.const 1
    call $i64_symbol
    i64.div_u
    drop
  )


  (func $f32_block (type $void_t)
    f32.const 1
    (call $f32_symbol)
    f32.div
    drop
  )

  (func $f64_block (type $void_t)
    f64.const 1
    (call $f64_symbol)
    f64.div
    drop
  )

  (start $start)
)
