;; Fibonacci memoïzed version

(module
  (memory 1)

  (func $fibo (param $n i32) (result i32)
    (if
      (i32.lt_s
        (local.get $n)
        (i32.const 0))
    (then
      (unreachable)))
    (if
      (i32.lt_s
        (local.get $n)
        (i32.const 2))
    (then
      (return (local.get $n))))
    (if
      (i32.eqz
        (i32.load (i32.mul (i32.const 4) (local.get $n))))
    (then
      local.get $n
      i32.const 4
      i32.mul
      (call $fibo (i32.sub (local.get $n) (i32.const 1)))
      (call $fibo (i32.sub (local.get $n) (i32.const 2)))
      i32.add
      i32.store ))
    local.get $n
    i32.const 4
    i32.mul
    i32.load
    return
  )

  (func $start
    i32.const 4
    call $fibo
    drop
  )

  (start $start)
)
