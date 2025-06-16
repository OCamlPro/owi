;; sign(y)*abs(x) == copy_sign(x,y)

(module
  (import "owi" "f64_symbol" (func $f64_symbol (result f64)))
  (import "owi" "assert" (func $assert_i32 (param i32)))
  (import "owi" "assume" (func $assume_i32 (param i32)))

  (func $start
    (local $x f64)
    (local $y f64)
    (local $sign_y f64)

    (local.set $x (call $f64_symbol))
    (local.set $y (call $f64_symbol))

    (call $assume_i32   ;;  x is not NaN
      (f64.eq (local.get $x) (local.get $x))
    )

    (call $assume_i32   ;; y is not NaN
        (f64.eq (local.get $y) (local.get $y))
    )

    (if  (i64.eqz (i64.shr_u (i64.reinterpret_f64 (local.get  $y)) (i64.const 63)))
      (then (local.set $sign_y (f64.const 1)) )
      (else (local.set $sign_y (f64.const -1)))
    )

    (call $assert_i32
      (f64.eq
        (f64.mul (local.get $sign_y) (f64.abs (local.get $x)))
        (f64.copysign (local.get $x)  (local.get $y))
      )
    )
  )

  (start $start)
)
