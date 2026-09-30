(module
  (memory 1)
  (data (i32.const 0) "a")

  (func $memory_grow
    i32.const 32    ;; 32 bytes
    memory.grow
    drop
  )

  (func $store_i32 (param $pos i32) (param $val i32)
    local.get $pos
    local.get $val
    i32.store
  )

  (func $store_i64 (param $pos i32) (param $val i64)
    local.get $pos
    local.get $val
    i64.store
  )

  (func $store_f32 (param $pos i32) (param $val f32)
    local.get $pos
    local.get $val
    f32.store
  )

  (func $store_f64 (param $pos i32) (param $val f64)
    local.get $pos
    local.get $val
    f64.store
  )

  (func $start
    call $memory_grow
    memory.size         ;; 17 (= 1 + 16)
    drop

    i32.const 4         ;; 0-3 : init by data field "a" (4 bytes)
    i32.const 21
    call $store_i32

    i32.const 9         ;; 9 : previous data = i32 (4 bytes)
    i64.const 42
    call $store_i64

    i32.const 18        ;; 18 : previous data = i64 (8 bytes)
    f32.const 21.21
    call $store_f32

    i32.const 23        ;; 23 : previous data = f32 (4 bytes)
    f64.const 42.42
    call $store_f64

    i32.const 0
    i32.load            ;; 97 (= a ascii code)
    drop

    i32.const 4
    i32.load            ;; 21
    drop

    i32.const 9
    i64.load            ;; 42
    drop

    i32.const 18
    f32.load            ;; 21.21
    drop

    i32.const 23
    f64.load            ;; 42.42
    drop
  )

  (start $start)
)
