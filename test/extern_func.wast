
(module $extern_func
  (import "stuff" "fresh" (func $fresh (param i32) (result externref)))
  (import "stuff" "get_i32r" (func $get (param externref) (result i32)))
  (import "stuff" "set_i32r" (func $set (param externref) (param i32)))
  (import "stuff" "print_i32" (func $print_i32 (param i32)))
  (func $start (local $ref externref)
    (local.set $ref (call $fresh (i32.const 42)))
    (call $print_i32 (call $get (local.get $ref)))
    (call $set (i32.const 13) (local.get $ref))
    (call $print_i32 (call $get (local.get $ref)))
    return
  )
  (start $start)
)