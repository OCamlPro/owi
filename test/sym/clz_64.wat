(module
  (import "symbolic" "i64_symbol" (func $i64_symbol (result i64)))
  (import "symbolic" "assume" (func $assume (param i32)))
  (import "symbolic" "assert" (func $assert (param i32)))

  (func $countLeadingZeros (param i64) (result i64)
    (local $x i64)
    (local $res i64)


    ;; Initialize local variables
    (local.set $res (i64.const 64)) ;; Initialize with the highest possible index of a bit
    (local.set $x (local.get 0))  ;; Store the input

    ;; Loop to find the leading zeros
    (block $outter
        (loop $loop

        ;; Check if all bits are shifted out
        (if (i64.eqz (local.get $x))
            (then (br $outter))
        )

        ;; Shift the input to the right by 1 bit
        (local.set $x (i64.shr_u (local.get $x) (i64.const 1)))

        ;; Decrement the count of zero bits
        (local.set $res (i64.sub (local.get $res) (i64.const 1)))

        (br $loop)
        )
    )

    ;; Return the number of leading zeros
    (return (local.get $res))
  )

  (func $start

    (local $n i64)
    (local.set $n (call $i64_symbol))

    (call $assert (i64.eq
        (call $countLeadingZeros (local.get $n))
        (i64.clz (local.get $n))
    ))

    (call $assert (i64.eq
        (i64.ctz (local.get $n))
        ;; Implem of ctz using clz 
        ;; from hacker's delight p107
        (i64.sub 
          (i64.const 64) 
          (i64.clz ( i64.and
            (i64.xor (local.get $n) (i64.const -1))
            (i64.sub (local.get $n) (i64.const 1))
        ))
        )
    ))
  )


  (start $start)
)
