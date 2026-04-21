(module
  (func $f (param $p f32) (result f32)
    (local $l f32)
    f32.const 2.02
    local.tee $l
    local.get $p
    f32.add
  )

  (func $start
    f32.const 40.40
    call $f
    drop
  )

  (start $start)
)
