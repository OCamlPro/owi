;; Factorial function : some versions

(module
  ;; recursive version (pre condition: n > 0)
  (func $factR (export "fact") (param $n i32) (result i32)
    local.get $n
    local.get $n
    i32.const 1
    i32.eq
    br_if 0
    i32.const 1
    i32.sub
    call $factR
    local.get $n
    i32.mul
  )
)

(assert_return (invoke "fact" (i32.const 1)) (i32.const 1))
(assert_return (invoke "fact" (i32.const 2)) (i32.const 2))
(assert_return (invoke "fact" (i32.const 6)) (i32.const 720))
(assert_return (invoke "fact" (i32.const 7)) (i32.const 5040))

(module
  ;; non recursive version 1 (pre condition: n > 0)
  (func $factNR (export "fact") (param $n i32) (result i32)
    (local $cond i32)
    (local $acc i32)
    local.get $n
    local.get $n
    i32.const 1
    i32.eq
    br_if 0
    local.tee $cond
    local.set $acc
    (loop $l
      local.get $cond
      i32.const 1
      i32.sub
      local.tee $cond
      local.get $acc
      i32.mul
      local.set $acc
      local.get $cond
      i32.const 1
      i32.ne
      br_if $l
    )
    local.get $acc
  )
)

(assert_return (invoke "fact" (i32.const 1)) (i32.const 1))
(assert_return (invoke "fact" (i32.const 2)) (i32.const 2))
(assert_return (invoke "fact" (i32.const 6)) (i32.const 720))
(assert_return (invoke "fact" (i32.const 7)) (i32.const 5040))

(module
  ;; non recursive version 2: without $cond local var (pre condition: n > 0)
  (func $factNR (export "fact") (param $n i32) (result i32)
    (local $acc i32)
    local.get $n
    local.get $n
    i32.const 1
    i32.eq
    br_if 0
    local.set $acc
    (loop $l
      local.get $n
      i32.const 1
      i32.sub
      local.tee $n
      local.get $acc
      i32.mul
      local.set $acc
      local.get $n
      i32.const 1
      i32.ne
      br_if $l
    )
    local.get $acc
  )
)

(assert_return (invoke "fact" (i32.const 1)) (i32.const 1))
(assert_return (invoke "fact" (i32.const 2)) (i32.const 2))
(assert_return (invoke "fact" (i32.const 6)) (i32.const 720))
(assert_return (invoke "fact" (i32.const 7)) (i32.const 5040))

(module
  ;; non recursive version 3: without $acc local var (pre condition: n > 0)
  (func $factNR (export "fact") (param $n i32) (result i32)
    local.get $n
    local.get $n
    i32.const 1
    i32.eq
    br_if 0
    (loop $l (param i32) (result i32)
      local.get $n
      i32.const 1
      i32.sub
      local.tee $n
      i32.mul
      local.get $n
      i32.const 1
      i32.ne
      br_if $l
    )
  )
)

(assert_return (invoke "fact" (i32.const 1)) (i32.const 1))
(assert_return (invoke "fact" (i32.const 2)) (i32.const 2))
(assert_return (invoke "fact" (i32.const 6)) (i32.const 720))
(assert_return (invoke "fact" (i32.const 7)) (i32.const 5040))
