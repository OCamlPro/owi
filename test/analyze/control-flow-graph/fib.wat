 (module
    (memory 10)

    (func $fib2 (param $n i32) (result i32) (local $x1 i32) (local $x2 i32)

        (i32.lt_u (local.get $n) (i32.const 2))


        (if 
        (then ;; n<2
            local.get $n
            return)
        (else))

        (i32.sub (local.get $n) (i32.const 1))
        i32.const 4
        i32.mul
        i32.load
        local.tee $x1
        (if 
        (then)
        (else

            (i32.sub (local.get $n) (i32.const 1))
            i32.const 4
            i32.mul

            (i32.sub (local.get $n) (i32.const 1))
            call $fib2
            local.tee $x1

            i32.store
        ))
        local.get $x1

        (i32.sub (local.get $n) (i32.const 2))
        i32.const 4
        i32.mul
        i32.load
        local.tee $x2 ;; local.set $x2 local.get $x2
        (if 
        (then)
        (else

            (i32.sub (local.get $n) (i32.const 2))
            i32.const 4
            i32.mul

            (i32.sub (local.get $n) (i32.const 2))
            call $fib2
            local.tee $x2

            i32.store
        ))
        local.get $x2

        i32.add

        return
    )
 )