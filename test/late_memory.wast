(module
  (func $f (export "42") (result i32)
    (return (i32.const 42))
  )
  (memory 4)
  (func $printi32 (import "spectest" "print_i32") (param i32))
)
