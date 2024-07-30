(module
  (import "symbolic" "i32_symbol" (func $i32_symbol (result i32)))
  (import "symbolic" "assume" (func $assume (param i32)))
  (import "symbolic" "assert" (func $assert (param i32)))
  (import "introspection" "dump_memory" (func $dump_memory))
  (import "introspection" "print_i32" (func $print_i32 (param i32) (result i32)))
  (memory $m 1)
  (func $start
    i32.const 0x00000000
    i32.const 0x50ffc001
    i32.store
    i32.const 0x00000000
    i32.load
    i32.const 0x50ffc001
    i32.eq
    call $assert)
  (start $start))
