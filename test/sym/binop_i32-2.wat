(module
  (import "symbolic" "i32_symbol" (func $i32_symbol (result i32)))

  (func $start
    (local $w i32)
    (local $x i32)
    (local $y i32)
    (local $z i32)

    (local.set $w (call $i32_symbol))
    (local.set $x (call $i32_symbol))
    (local.set $y (call $i32_symbol))
    (local.set $z (call $i32_symbol))

    (i32.and (local.get $x) (local.get $y))
    (if (then unreachable))

    (i32.or (local.get $x) (local.get $y))
    (if (then unreachable))

    (i32.xor (local.get $x) (local.get $y))
    (if (then unreachable))

    (i32.eqz (i32.shl (local.get $z) (i32.const 0)))
    (if (then unreachable))

    (i32.eqz (i32.shr_s (local.get $z) (local.get $w)))
    (if (then unreachable))

    (i32.eqz (i32.shr_u (local.get $z) (local.get $w)))
    (if (then unreachable))

    ;; owi: internal error, uncaught exception:
    ;;  Failure("z3_mappings: rotl|rotr not implemented!")
    ;; (i32.eqz (i32.rotl (local.get $z) (local.get $w)))
    ;; (if (then unreachable))
    ;; (i32.eqz (i32.rotr (local.get $z) (local.get $w)))
    ;; (if (then unreachable))
  )

  (start $start)
)
