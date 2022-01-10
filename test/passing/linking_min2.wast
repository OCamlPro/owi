;; Tables

(module $Mt
  (type (func (result i32)))

  (table (export "tab") 1 funcref)
  (func $g (result i32) (i32.const 4))

  (func (export "call") (param i32) (result i32)
    (call_indirect (type 0) (local.get 0))
  )
)

(register "Mt" $Mt)

(module $Ot

  (table (import "Mt" "tab") 1 funcref)
  (func $h (result i32) (i32.const -4))
  (elem (i32.const 0) $h)

)

(assert_return (invoke $Mt "call" (i32.const 0)) (i32.const -4)) ;; HERE
