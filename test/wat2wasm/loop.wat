(module
  (func $loop (param $nloop i32) (result i64)
    (local $cloop i32)
    i32.const 0
    local.set $cloop
    ;; f32.const 42
    (loop $l (result i64) ;; TODO: add (param f32)
      local.get $cloop
      i32.const 1
      i32.add
      local.tee $cloop
      local.get $nloop
      i32.ne
      (if 
        (then
          ;; f32.const 42
          br $l)
        (else nop))
      ;; br_if $l
      ;; drop
      i64.const 42
    )
  )

  (func $start
    i32.const 3
    call $loop
    drop
  )

  (start $start)
)
