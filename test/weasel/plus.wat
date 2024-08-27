(module
    (@contract $plus_three
        (ensures (= result (+ $x 3)))
    )
    (func $plus_three
        (param $x i32) (result i32)
        (i32.add (i32.const 3) (local.get $x)))
    (func $start
        (call $plus_three (i32.const 42))
        drop)
    (start $start)
)
