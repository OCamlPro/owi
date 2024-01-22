(module
  (func $start
    i32.const 42
    i32.const 24
    i32.const 1
    select
    i32.const 1
    drop
    drop
    i32.const 42
    i32.const 24
    i32.const 0
    select
    i32.const 1
    drop
    drop

    i64.const 42
    i64.const 24
    i32.const 1
    select
    i64.const 1
    drop
    drop
    i64.const 42
    i64.const 24
    i32.const 0
    select
    i64.const 1
    drop
    drop

    f32.const 42
    f32.const 24
    i32.const 1
    select
    f32.const 1
    drop
    drop
    f32.const 42
    f32.const 24
    i32.const 0
    select
    f32.const 1
    drop
    drop

    f64.const 42
    f64.const 24
    i32.const 1
    select
    f64.const 1
    drop
    drop
    f64.const 42
    f64.const 24
    i32.const 0
    select
    f64.const 1
    drop
    drop
  )
  (start $start)
)
