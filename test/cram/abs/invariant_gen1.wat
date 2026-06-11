(module
  (import "owi" "i32_symbol" (func $i32_symbol (result i32)))

  (func $start
    (local $a i32)
    (local $b i32)
    (local $c i32)
    (local $d i32)
    (local $e i32)
    (local $i i32)
    (local $n i32)

    ;; a <- unknown
    call $i32_symbol
    local.set $a

    ;; b <- a + 10 = unknown + 10
    local.get $a
    i32.const 10
    i32.add
    local.set $b

    ;; c <- b + 20 = unknown + 10 + 20 = unknown + 30
    local.get $b
    i32.const 20
    i32.add
    local.set $c

    ;; d <- c - a = (unknown + 30) - (unknown) = 30
    local.get $c
    local.get $a
    i32.sub
    local.set $d

    ;; e <- d - 29 = 30 - 29 = 1
    local.get $d
    i32.const 29
    i32.sub
    local.set $e

    ;; n <- 10_000
    ;; i <- 0
    i32.const 10000
    local.set $n
    i32.const 0
    local.set $i

    block $break
      loop $loop

        ;; safe division (100 / e = 100 / 1)
        i32.const 100
        local.get $e
        i32.div_s
        drop

        ;; i++
        local.get $i
        i32.const 1
        i32.add
        local.tee $i

        ;; if n < i then loop
        local.get $n
        i32.lt_s
        br_if $loop
      end
    end

    ;; if a == 4242 then e <- 0
    local.get $a
    i32.const 4242
    i32.eq
    (if
      (then
        i32.const 0
        local.set $e
      )
    )

    ;; 42 / e ---> bug if a == 4242
    i32.const 42
    local.get $e
    i32.div_s
    drop
  )

  (start $start)
)
