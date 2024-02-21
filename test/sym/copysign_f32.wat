;; sign(y)*abs(x) == copy_sign(x,y)

(module
  (import "symbolic" "f32_symbol" (func $f32_symbol (result f32)))
  (import "symbolic" "assert" (func $assert_i32 (param i32)))
  (import "symbolic" "assume" (func $assume_i32 (param i32)))

  (func $start
    (local $x f32)
    (local $y f32)
    (local $sign_y f32)

    (local.set $x (call $f32_symbol))
    (local.set $y (call $f32_symbol))

    (call $assume_i32   ;;  x is not NaN
      (f32.eq (local.get $x) (local.get $x))
    )

    (call $assume_i32   ;; y is not NaN
        (f32.eq (local.get $y) (local.get $y))
    )

    (if  (i32.eqz (i32.shr_u (i32.reinterpret_f32 (local.get  $y)) (i32.const 31)))
      (then (local.set $sign_y (f32.const 1)) )
      (else (local.set $sign_y (f32.const -1)))
    )

    (call $assert_i32
      (f32.eq
        (f32.mul (local.get $sign_y) (f32.abs (local.get $x)))
        (f32.copysign (local.get $x)  (local.get $y))
      )
    )
  )

  (start $start)
)
