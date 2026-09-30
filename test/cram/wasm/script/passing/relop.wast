(module
  (func (export "i32") (result i32)
    (i32.le_u (i32.const 21) (i32.const 42))
    (i32.ge_u (i32.const 42) (i32.const 21))
    (i32.lt_u (i32.const 21) (i32.const 42))
    (i32.gt_u (i32.const 42) (i32.const 21))
    (i32.le_s (i32.const 21) (i32.const 42))
    (i32.ge_s (i32.const 42) (i32.const 21))
    (i32.lt_s (i32.const 21) (i32.const 42))
    (i32.gt_s (i32.const 42) (i32.const 21))
    (i32.eq (i32.const 42) (i32.const 42))
    (i32.ne (i32.const 42) (i32.const 21))
    i32.add
    i32.add
    i32.add
    i32.add
    i32.add
    i32.add
    i32.add
    i32.add
    i32.add
  )
)

(assert_return (invoke "i32") (i32.const 10))

(module
  (func (export "i64") (result i32)
    (i64.le_u (i64.const 21) (i64.const 42))
    (i64.ge_u (i64.const 42) (i64.const 21))
    (i64.lt_u (i64.const 21) (i64.const 42))
    (i64.gt_u (i64.const 42) (i64.const 21))
    (i64.le_s (i64.const 21) (i64.const 42))
    (i64.ge_s (i64.const 42) (i64.const 21))
    (i64.lt_s (i64.const 21) (i64.const 42))
    (i64.gt_s (i64.const 42) (i64.const 21))
    (i64.eq (i64.const 42) (i64.const 42))
    (i64.ne (i64.const 42) (i64.const 21))
    i32.add
    i32.add
    i32.add
    i32.add
    i32.add
    i32.add
    i32.add
    i32.add
    i32.add
  )
)

(assert_return (invoke "i64") (i32.const 10))

(module
  (func (export "f32") (result i32)
    (f32.le (f32.const 21) (f32.const 42))
    (f32.ge (f32.const 42) (f32.const 21))
    (f32.lt (f32.const 21) (f32.const 42))
    (f32.gt (f32.const 42) (f32.const 21))
    (f32.eq (f32.const 42) (f32.const 42))
    (f32.ne (f32.const 42) (f32.const 21))
    i32.add
    i32.add
    i32.add
    i32.add
    i32.add
  )
)

(assert_return (invoke "f32") (i32.const 6))

(module
  (func (export "f64") (result i32)
    (f64.le (f64.const 21) (f64.const 42))
    (f64.ge (f64.const 42) (f64.const 21))
    (f64.lt (f64.const 21) (f64.const 42))
    (f64.gt (f64.const 42) (f64.const 21))
    (f64.eq (f64.const 42) (f64.const 42))
    (f64.ne (f64.const 42) (f64.const 21))
    i32.add
    i32.add
    i32.add
    i32.add
    i32.add
  )
)

(assert_return (invoke "f64") (i32.const 6))
