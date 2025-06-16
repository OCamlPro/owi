(module
  (import "owi" "i32_symbol" (func $i32_symbol (result i32)))
  (import "owi" "i64_symbol" (func $i64_symbol (result i64)))
  (import "owi" "f32_symbol" (func $f32_symbol (result f32)))
  (import "owi" "f64_symbol" (func $f64_symbol (result f64)))
  (import "owi" "assume" (func $assume_i32 (param i32)))
  (import "owi" "assert" (func $assert_i32 (param i32)))

  (global $x (mut i32) (i32.const 0))
  (global $y (mut i64) (i64.const 0))
  (global $i (mut f32) (f32.const 0))
  (global $j (mut f64) (f64.const 0))

  (func $start
    ;; Objective: storing something in a global and then reading it should give the same value

    (local $x i32)
    (local $y i64)
    (local $i f32)
    (local $j f64)

    (local.set $x (call $i32_symbol))
    (local.set $y (call $i64_symbol))
    (local.set $i (call $f32_symbol))
    (local.set $j (call $f64_symbol))

    (global.set $x (local.get $x))
    (global.set $y (local.get $y))
    (call $assert_i32 (i32.eq (global.get $x) (local.get $x)))
    (call $assert_i32 (i64.eq (global.get $y) (local.get $y)))

    ;; In float numbers context, avoid Nan

    (call $assume_i32   ;; 0 < i < 42
      (i32.and
        (f32.gt (local.get $i) (f32.const 0))
        (f32.lt (local.get $i) (f32.const 42))))
    (global.set $i (local.get $i))
    (call $assert_i32 (f32.eq (global.get $i) (local.get $i)))

    (call $assume_i32   ;; 0 < j < 42
      (i32.and
        (f64.gt (local.get $j) (f64.const 0))
        (f64.lt (local.get $j) (f64.const 42))))
    (global.set $j (local.get $j))
    (call $assert_i32 (f64.eq (global.get $j) (local.get $j)))
  )

  (start $start)
)
