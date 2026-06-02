(module
  (func $start (local $x i32)

    ;; pour i de 0 à a, vérifier que f(i) est != 0

    (local $i i32)
    (local $n i32)
    (local $sum i32)


    i32.const 3
    local.set $n

    (loop $loop
      (i32.div_s
        (i32.const 100)
        (i32.add (local.get $i) (i32.const 1)))
      local.get $sum
      i32.add
      local.set $sum


      (i32.lt_s (local.get $i) (local.get $n))
      (if (then
        local.get $i
        i32.const 1
        i32.add
        local.set $i
         br $loop
      ))
    )


    ;;i32.const 42
    ;;local.get $x
    ;;i32.div_s
    ;;drop
  )
  (start $start))
