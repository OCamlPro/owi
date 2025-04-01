(module

  (func $print_i32 (import "spectest" "print_i32") (param i32))

  (func $main
    i32.const 42
    call $print_i32
  )

  (start $main)
)
