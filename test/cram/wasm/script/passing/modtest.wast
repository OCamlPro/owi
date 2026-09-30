(module
  (func $printi32 (import "spectest" "print_i32") (param i32))
  (memory 4)
  (func $print98
    (call $printi32 (i32.const 98))
  )
  (start $print98)
)

(module $A
  (func $printi32 (import "spectest" "print_i32") (param i32))
  (memory 4)
  (func $print99
    (call $printi32 (i32.const 99))
  )
  (start $print99)
  (func $f (export "f") (result i32)
    i32.const 0
    (i32.add
      (i32.load (i32.const 0))
      (i32.const 42))
    i32.store
    (return (i32.load (i32.const 0)))
  )
  (func $g (export "g") (result i32)
    call $f
    drop
    call $f
    return
  )
)

(assert_return (invoke $A "f") (i32.const 42))

(register "A" $A)

(assert_return (invoke $A "g") (i32.const 126))


(module
  (func $f (import "A" "f") (result i32))
  (func $h (export "h") (result i32)
    call $f
    drop
    call $f
    return
  )
)

(assert_return (invoke "h") (i32.const 210))
