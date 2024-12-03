(module

  (import "symbolic" "i32_symbol" (func $i32_symbol (result i32)))
  (import "symbolic" "f32_symbol" (func $f32_symbol (result f32)))
  (import "symbolic" "i64_symbol" (func $i64_symbol (result i64)))
  (import "symbolic" "f64_symbol" (func $f64_symbol (result f64)))

  (func $start
    (i32.eq (i32.const 42) (call $i32_symbol))
    (if (then
      (i64.eq (i64.const 84) (call $i64_symbol))
      (if (then
        (f32.eq (f32.const 13.12) (call $f32_symbol))
        (if (then
          (f64.eq (f64.const 12.13 (call $f64_symbol)))
          (if (then unreachable)))))))))

  (start $start))
