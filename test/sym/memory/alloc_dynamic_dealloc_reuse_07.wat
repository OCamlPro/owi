;; Reuse memory address chunk with different size
(module
  (import "summaries" "alloc" (func $malloc (param i32 i32) (result i32)))
  (import "summaries" "dealloc" (func $free (param i32)))
  (import "symbolic" "assert" (func $assert (param i32)))
  (memory $m 1)
  (global $__heap_base (mut i32) (i32.const 0x00000000))
  (func $start (local i32)
    global.get $__heap_base
    i32.const 0x00000004
    call $malloc
    local.tee 0
    i32.const 0x0000002a
    i32.store
    local.get 0
    i32.load
    i32.const 0x0000002a
    i32.eq
    call $assert
    local.get 0
    call $free
    global.get $__heap_base
    i32.const 0x00000008
    call $malloc
    local.tee 0
    i32.const 0x00000054
    i32.store
    local.get 0
    i32.load
    i32.const 0x00000054
    i32.eq
    call $assert
    local.get 0
    call $free)
  (start $start))
