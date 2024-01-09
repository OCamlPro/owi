(module
  (import "symbolic" "i32_symbol" (func $i32_symbol (result i32)))

  (func $start
    (local $x i32)

    (local.set $x (call $i32_symbol))

    (i32.eqz (local.get $x))
    (if (then unreachable))

    ;; Memo "lib/z3_mappings.ml" (to be implemented)
    ;; Clz not supported yet
    ;; (i32.ge_s (i32.const 2) (i32.clz (local.get $x)) )
    ;; (if (then unreachable))
    ;; Ctz: TODO
    ;; (i32.ge_s (i32.const 2) (i32.ctz (local.get $x)) )
    ;; (if (then unreachable))
    ;; Popcnt: TODO
    ;; (i32.ge_s (i32.const 2) (i32.popcnt (local.get $x)) )
    ;; (if (then unreachable))

  )

  (start $start)
)
