(module $extern

  (import "sausage" "fresh"
    (func $fresh (param i32) (result externref)))

  (import "sausage" "get_i32r"
    (func $get (param externref) (result i32)))

  (import "sausage" "set_i32r"
    (func $set (param externref) (param i32)))

  (import "sausage" "print_i32"
    (func $print_i32 (param i32)))

  (func $start (local $ref externref)

    ;; let ref = fresh 42
    (local.set $ref (call $fresh (i32.const 42)))

    ;; print_i32 (get ref)
    (call $print_i32 (call $get (local.get $ref)))

    ;; set ref 13
    (call $set (local.get $ref) (i32.const 13)  )

    ;; print_i32 (get ref)
    (call $print_i32 (call $get (local.get $ref)))

  )

  (start $start)
)
