(module
  (import "owi" "i32_symbol" (func $i32_symbol (result i32)))

  ;; A minimal division-by-zero warning: $d comes from a symbolic
  ;; value, so the abstract interpreter can't prove it's nonzero, and
  ;; 42 / $d triggers a "Possible division by zero" warning — even
  ;; though no single concrete execution is required to actually hit
  ;; it, the abstract domain for $d includes zero as a possibility.
  (func $start
    (local $d i32)

    call $i32_symbol
    local.set $d

    i32.const 42
    local.get $d
    i32.div_s
    drop
  )

  (start $start)
)
