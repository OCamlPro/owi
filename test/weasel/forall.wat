(module
    (@contract $start
        (requires (forall i32 $x (==> (&& (>= $x 1) (<= $x 10)) (<= $x 100))))
    )
    (func $start)
    (start $start)
)
