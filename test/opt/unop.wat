(module
  (func $iunop
    i32.const 2
    i32.clz
    i32.const 0
    drop
    drop

    i32.const 2
    i32.ctz
    i32.const 0
    drop
    drop

    i32.const 3
    i32.popcnt
    i32.const 0
    drop
    drop

    i64.const 2
    i64.clz
    i32.const 0
    drop
    drop

    i64.const 2
    i64.ctz
    i32.const 0
    drop
    drop

    i64.const 3
    i64.popcnt
    i32.const 0
    drop
    drop
  )

  (func $f32unop
    f32.const 42
    f32.abs
    i32.const 0
    drop
    drop

    f32.const -42
    f32.neg
    i32.const 0
    drop
    drop

    f32.const 42
    f32.ceil
    i32.const 0
    drop
    drop

    f32.const 42
    f32.floor
    i32.const 0
    drop
    drop

    f32.const 42
    f32.trunc
    i32.const 0
    drop
    drop

    f32.const 42
    f32.nearest
    i32.const 0
    drop
    drop

    f32.const 1764
    f32.sqrt
    i32.const 0
    drop
    drop
  )

  (func $f64unop
    f64.const 42
    f64.abs
    i32.const 0
    drop
    drop

    f64.const -42
    f64.neg
    i32.const 0
    drop
    drop

    f64.const 42
    f64.ceil
    i32.const 0
    drop
    drop

    f64.const 42
    f64.floor
    i32.const 0
    drop
    drop

    f64.const 42
    f64.trunc
    i32.const 0
    drop
    drop

    f64.const 42
    f64.nearest
    i32.const 0
    drop
    drop

    f64.const 1764
    f64.sqrt
    i32.const 0
    drop
    drop
  )

  (func $start
    call $iunop
    call $f32unop
    call $f64unop
  )

  (start $start)
)
