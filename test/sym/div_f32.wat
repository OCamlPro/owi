(module
  (import "symbolic" "f32_symbol" (func $f32_symbol (result f32)))
  (import "symbolic" "assume" (func $assume_i32 (param i32)))
  (import "symbolic" "assert" (func $assert_i32 (param i32)))

  (func $start
    (local $x f32)
    (local $y f32)

    (local.set $x (call $f32_symbol))
    (local.set $y (call $f32_symbol))

    ;; x = y
    (call $assume_i32 (f32.eq (local.get $x) (local.get $y)))
    ;; 1/x = 1/y
    (call $assert_i32 (f32.eq
      (f32.div (f32.const 1) (local.get $x))
      (f32.div (f32.const 1) (local.get $y))))
  )

  (start $start)
)
