(module $extern_mem

  (import "chorizo" "memset" (func $memset (param i32 i32 i32)))

  (import "chorizo" "print_x64" (func $print_x64 (param i64)))

  (memory 1)

  (func $start

    ;; memset 0 0xAA 8
    (call $memset (i32.const 0) (i32.const 0xAA) (i32.const 8))

    ;; print_x64 (load 0)
    (call $print_x64 (i64.load (i32.const 0)))
  )

  (start $start)
)
