(module
  (import "symbolic" "i32_symbol" (func $i32_symbol (result i32)))
  (import "symbolic" "i64_symbol" (func $i64_symbol (result i64)))
  (import "symbolic" "f32_symbol" (func $f32_symbol (result f32)))
  (import "symbolic" "f64_symbol" (func $f64_symbol (result f64)))

  (memory $m 1)
  (data $d (memory $m) (offset i32.const 99) "str_data")

  (func $mem_set_i32_8 (param $idx i32) (param $val i32)
    local.get $idx
    local.get $val
    i32.store8
  )

  (func $mem_set_i32_16 (param $idx i32) (param $val i32)
    local.get $idx
    local.get $val
    i32.store16
  )

  (func $mem_set_i32 (param $idx i32) (param $val i32)
    local.get $idx
    local.get $val
    i32.store
  )

  (func $mem_set_i64_8 (param $idx i32) (param $val i64)
    local.get $idx
    local.get $val
    i64.store8
  )

  (func $mem_set_i64_16 (param $idx i32) (param $val i64)
    local.get $idx
    local.get $val
    i64.store16
  )

  (func $mem_set_i64_32 (param $idx i32) (param $val i64)
    local.get $idx
    local.get $val
    i64.store32
  )

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
    (call $mem_set_i32_8 (i32.const 0) (call $i32_symbol))
    (call $mem_set_i32_16 (i32.const 4) (call $i32_symbol))
    (call $mem_set_i32 (i32.const 8) (call $i32_symbol))

    (call $mem_set_i64_8 (i32.const 12) (call $i64_symbol))
    (call $mem_set_i64_16 (i32.const 20) (call $i64_symbol))
    (call $mem_set_i64_32 (i32.const 28) (call $i64_symbol))
    (call $mem_set_i64 (i32.const 36) (call $i64_symbol))

    (call $mem_set_f32 (i32.const 44) (call $f32_symbol))

    (call $mem_set_f64 (i32.const 48) (call $f64_symbol))


    (if (i32.lt_s (i32.const 5) (i32.load8_s (i32.const 0)))
      (then unreachable))

    (if (i32.lt_u (i32.const 5) (i32.load8_u (i32.const 0)))
      (then unreachable))

    (if (i32.lt_s (i32.const 5) (i32.load16_s (i32.const 4)))
      (then unreachable))

    (if (i32.lt_u (i32.const 5) (i32.load16_u (i32.const 4)))
      (then unreachable))

    (if (i32.lt_s (i32.const 5) (i32.load (i32.const 8)))
      (then unreachable))


    (if (i64.gt_s (i64.const 5) (i64.load8_s (i32.const 12)))
      (then unreachable))

    (if (i64.gt_u (i64.const 5) (i64.load8_u (i32.const 12)))
      (then unreachable))

    (if (i64.gt_s (i64.const 5) (i64.load16_s (i32.const 20)))
      (then unreachable))

    (if (i64.gt_u (i64.const 5) (i64.load16_u (i32.const 20)))
      (then unreachable))

    (if (i64.gt_s (i64.const 5) (i64.load32_s (i32.const 28)))
      (then unreachable))

    (if (i64.gt_u (i64.const 5) (i64.load32_u (i32.const 28)))
      (then unreachable))

    (if (i64.gt_s (i64.const 5) (i64.load (i32.const 36)))
      (then unreachable))


    (if (f32.lt (f32.const 5) (f32.load (i32.const 44)))
      (then unreachable))


    (if (f64.gt (f64.const 5) (f64.load (i32.const 48)))
      (then unreachable)))

  (start $start)
)
