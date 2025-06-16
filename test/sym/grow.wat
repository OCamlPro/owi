(module
  (import "owi" "i32_symbol" (func $i32_symbol (result i32)))
  (import "owi" "assume" (func $assume (param i32)))
  ;; This is just to see whether we can detect the failure
  (func $test_out_of_bounds_with_symbolic_grow
      (local $x i32)
    ;; x = i32.symbol
    (local.set
      $x
      (call $i32_symbol))
    ;; assume: x >= 1
    (call $assume
      (i32.ge_s
        (local.get $x)
        (i32.const 1)))
    ;; assume: x < 3
    (call $assume
      (i32.lt_s
        (local.get $x)
        (i32.const 3)))
    (memory.grow
      (local.get $x))
    (drop)
    ;; Store something on the second page: pass
    (i32.store
      (i32.const 65536)
      (i32.const 42))
    ;; Store something on the third page (addr > 131_072) fails when x = 1
    (i32.store
      (i32.const 131072)
      (i32.const 1337)))
  (func $start
    call $test_out_of_bounds_with_symbolic_grow)
  (memory $m 1)
  (start $start))
