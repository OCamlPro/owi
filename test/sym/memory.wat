(module
  (import "symbolic" "i32_symbol" (func $i32_symbol (result i32)))
  (import "symbolic" "i64_symbol" (func $i64_symbol (result i64)))
  (import "symbolic" "f32_symbol" (func $f32_symbol (result f32)))
  (import "symbolic" "f64_symbol" (func $f64_symbol (result f64)))
  (import "symbolic" "assume" (func $assume_i32 (param i32)))
  (import "symbolic" "assert" (func $assert_i32 (param i32)))

  (memory $m 1)
  (data $d (memory $m) (offset i32.const 99) "str_data")

  ;; (func $mem_set_i32_8 (param $idx i32) (param $val i32)
  ;;   local.get $idx
  ;;   local.get $val
  ;;   i32.store8
  ;; )

  ;; (func $mem_set_i32_16 (param $idx i32) (param $val i32)
  ;;   local.get $idx
  ;;   local.get $val
  ;;   i32.store16
  ;; )

  (func $mem_set_i32 (param $idx i32) (param $val i32)
    local.get $idx
    local.get $val
    i32.store
  )

  ;; (func $mem_set_i64_8 (param $idx i32) (param $val i64)
  ;;   local.get $idx
  ;;   local.get $val
  ;;   i64.store8
  ;; )

  ;; (func $mem_set_i64_16 (param $idx i32) (param $val i64)
  ;;   local.get $idx
  ;;   local.get $val
  ;;   i64.store16
  ;; )

  ;; (func $mem_set_i64_32 (param $idx i32) (param $val i64)
  ;;   local.get $idx
  ;;   local.get $val
  ;;   i64.store32
  ;; )

  (func $mem_set_i64 (param $idx i32) (param $val i64)
    local.get $idx
    local.get $val
    i64.store
  )

  (func $mem_set_f32 (param $idx i32) (param $val f32)
    local.get $idx
    local.get $val
    f32.store
  )

  (func $mem_set_f64 (param $idx i32) (param $val f64)
    local.get $idx
    local.get $val
    f64.store
  )

  (func $start
    ;; Objective: storing something in memory and then loading it should give the same value

    (local $i32 i32)
    (local $i64 i64)
    (local $f32 f32)
    (local $f64 f64)

    (local.set $i32 (call $i32_symbol))
    (local.set $i64 (call $i64_symbol))
    (local.set $f32 (call $f32_symbol))
    (local.set $f64 (call $f64_symbol))

    (call $mem_set_i32 (i32.const 0) (local.get $i32))
    (call $mem_set_i64 (i32.const 4) (local.get $i64))

    (call $assert_i32 (i32.eq (i32.load (i32.const 0)) (local.get $i32)))
    (call $assert_i32 (i64.eq (i64.load (i32.const 4)) (local.get $i64)))

    ;; In float numbers context, avoid Nan

    (call $assume_i32   ;; 0 < f32 < 42
      (i32.and
        (f32.gt (local.get $f32) (f32.const 0))
        (f32.lt (local.get $f32) (f32.const 42))))
    (call $mem_set_f32 (i32.const 12) (local.get $f32))
    (call $assert_i32 (f32.eq (f32.load (i32.const 12)) (local.get $f32)))

    (call $assume_i32   ;; 0 < f64 < 42
      (i32.and
        (f64.gt (local.get $f64) (f64.const 0))
        (f64.lt (local.get $f64) (f64.const 42))))
    (call $mem_set_f64 (i32.const 16) (local.get $f64))
    (call $assert_i32 (f64.eq (f64.load (i32.const 16)) (local.get $f64)))
  )

  (start $start)
)
