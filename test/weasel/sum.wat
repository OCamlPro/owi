(module
    (@contract 0
        (ensures (= result (+ $p1 (+ $p2 (+ $p3 $p4)))))
    )
    (func $sum
        (param $p1 i32) (param $p2 i32) (param $p3 i32) (param $p4 i32) (result i32)
        (local.get $p1)
        (local.get $p2)
        (local.get $p3)
        (local.get $p4)
        (i32.add)
        (i32.add)
        (i32.add))
    (func $start
        (call $sum (i32.const 42) (i32.const 42) (i32.const 42) (i32.const 42))
        drop)
    (start $start)
)
