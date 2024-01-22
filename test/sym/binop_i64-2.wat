(module
  (import "symbolic" "i64_symbol" (func $i64_symbol (result i64)))

  (func $start
    (local $w i64)
    (local $x i64)
    (local $y i64)
    (local $z i64)

    (local.set $w (call $i64_symbol))
    (local.set $x (call $i64_symbol))
    (local.set $y (call $i64_symbol))
    (local.set $z (call $i64_symbol))

    (i64.eqz (i64.and (local.get $x) (local.get $y)))
    (if (then unreachable))

    (i64.eqz (i64.or (local.get $x) (local.get $y)))
    (if (then unreachable))

    (i64.eqz (i64.xor (local.get $x) (local.get $y)))
    (if (then unreachable))

    (i64.eqz (i64.shl (local.get $z) (i64.const 0)))
    (if (then unreachable))

    (i64.eqz (i64.shr_s (local.get $z) (local.get $w)))
    (if (then unreachable))

    (i64.eqz (i64.shr_u (local.get $z) (local.get $w)))
    (if (then unreachable))

    ;; owi: internal error, uncaught exception:
    ;;  Failure("z3_mappings: rotl|rotr not implemented!")
    ;; (i64.eqz (i64.rotl (local.get $z) (local.get $w)))
    ;; (if (then unreachable))
    ;; (i64.eqz (i64.rotr (local.get $z) (local.get $w)))
    ;; (if (then unreachable))
  )

  (start $start)
)
