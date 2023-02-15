(module
  (func $print_i32 (import "spectest" "print_i32") (param i32))
  (func $main (call $print_i32 (i32.const 42)))
  (start $main)
)
