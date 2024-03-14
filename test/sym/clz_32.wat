(module
  (import "symbolic" "i32_symbol" (func $i32_symbol (result i32)))
  (import "symbolic" "assume" (func $assume (param i32)))
  (import "symbolic" "assert" (func $assert (param i32)))

  (func $countLeadingZeros (param i32) (result i32)
    (local $x i32)
    (local $res i32)


    ;; Initialize local variables
    (local.set $res (i32.const 32)) ;; Initialize with the highest possible index of a bit
    (local.set $x (local.get 0))  ;; Store the input

    ;; Loop to find the leading zeros
    (block $outter
        (loop $loop

        ;; Check if all bits are shifted out
        (if (i32.eqz (local.get $x))
            (then (br $outter))
        )

        ;; Shift the input to the right by 1 bit
        (local.set $x (i32.shr_u (local.get $x) (i32.const 1)))

        ;; Decrement the count of zero bits
        (local.set $res (i32.sub (local.get $res) (i32.const 1)))

        (br $loop)
        )
    )

    ;; Return the number of leading zeros
    (return (local.get $res))
  )

  (func $start

    (local $n i32)
    (local.set $n (call $i32_symbol))

    (call $assert (i32.eq
        (call $countLeadingZeros (local.get $n))
        (i32.clz (local.get $n))
    ))

    (call $assert (i32.eq
        (i32.ctz (local.get $n))
        ;; Implem of ctz using clz 
        ;; from hacker's delight p107
        (i32.sub 
          (i32.const 32) 
          (i32.clz ( i32.and
            (i32.xor (local.get $n) (i32.const -1))
            (i32.sub (local.get $n) (i32.const 1))
        ))
        )
    ))
  )


  (start $start)
)
