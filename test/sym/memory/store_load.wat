(module
  (import "symbolic" "i32_symbol" (func $i32_symbol (result i32)))
  (import "symbolic" "assume" (func $assume (param i32)))
  (import "symbolic" "assert" (func $assert (param i32)))

  (import "debug" "dump_memory" (func $dump_memory))
  (import "debug" "print_i32" (func $print_i32 (param i32) (result i32)))

  (memory $m 1)

  (func $start
    (local $x i32)
    i32.const 42
    i32.const 0xdeadc0de
    i32.store

    call $dump_memory

    i32.const 42
    i32.load
    call $print_i32
    i32.const 0xdeadc0de
    i32.eq
    call $assert
  )
  (start $start))
