;; Auxiliary module to import from

(module
  (func (export "func"))
  (func (export "func-i32") (param i32))
  (func (export "func-f32") (param f32))
  (func (export "func->i32") (result i32) (i32.const 22))
  (func (export "func->f32") (result f32) (f32.const 11))
  (func (export "func-i32->i32") (param i32) (result i32) (local.get 0))
  (func (export "func-i64->i64") (param i64) (result i64) (local.get 0))
)

(register "test")


;; Functions

(module
  (type $func_i32 (func (param i32)))
  (type $func_i64 (func (param i64)))
  (type $func_f32 (func (param f32)))
  (type $func_f64 (func (param f64)))

  (import "spectest" "print_i32" (func (param i32)))
  (import "spectest" "print_i32" (func $print_i32 (param i32)))
  (import "spectest" "print_f32" (func $print_f32 (param f32)))
  (import "spectest" "print_f64" (func $print_f64 (param f64)))
  (import "spectest" "print_i32_f32" (func $print_i32_f32 (param i32 f32)))
  (import "spectest" "print_f64_f64" (func $print_f64_f64 (param f64 f64)))
  (func $print_i32-2 (import "spectest" "print_i32") (param i32))
  (func $print_f64-2 (import "spectest" "print_f64") (param f64))
  (import "test" "func-i64->i64" (func $i64->i64 (param i64) (result i64)))

  (func (export "p1") (import "spectest" "print_i32") (param i32))
  (func $p (export "p2") (import "spectest" "print_i32") (param i32))
  (func (export "p3") (export "p4") (import "spectest" "print_i32") (param i32))
  (func (export "p5") (import "spectest" "print_i32") (type 0))
  (func (export "p6") (import "spectest" "print_i32") (type 0) (param i32) (result))

  (import "spectest" "print_i32" (func (type $forward)))
  (func (import "spectest" "print_i32") (type $forward))
  (type $forward (func (param i32)))

  (table funcref (elem $print_i32 $print_f64))

  (func (export "print32") (param $i i32)
    (local $x f32)
    (local.set $x (f32.convert_i32_s (local.get $i)))
    (call 0 (local.get $i))
    (call $print_i32_f32
      (i32.add (local.get $i) (i32.const 1))
      (f32.const 42)
    )
    (call $print_i32 (local.get $i))
    (call $print_i32-2 (local.get $i))
    (call $print_f32 (local.get $x))
    (call_indirect (type $func_i32) (local.get $i) (i32.const 0))
  )

  (func (export "print64") (param $i i64)
    (local $x f64)
    (local.set $x (f64.convert_i64_s (call $i64->i64 (local.get $i))))
    (call $print_f64_f64
      (f64.add (local.get $x) (f64.const 1))
      (f64.const 53)
    )
    (call $print_f64 (local.get $x))
    (call $print_f64-2 (local.get $x))
    (call_indirect (type $func_f64) (local.get $x) (i32.const 1))
  )
)

(assert_return (invoke "print32" (i32.const 13)))
(assert_return (invoke "print64" (i64.const 24)))
