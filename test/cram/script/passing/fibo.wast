(module
  (func $f (export "fibo") (param $n i32) (result i32)

    (if
      (i32.lt_s
        (local.get $n)
        (i32.const 2))
      (then (return (local.get $n))))
    (return
      (i32.add
        (call $f
          (i32.sub
            (local.get $n)
            (i32.const 2)))
        (call $f
          (i32.sub
            (local.get $n)
            (i32.const 1)))
      )
    )
  )
)

(assert_return (invoke "fibo" (i32.const 0)) (i32.const 0))
(assert_return (invoke "fibo" (i32.const 1)) (i32.const 1))
(assert_return (invoke "fibo" (i32.const 2)) (i32.const 1))
(assert_return (invoke "fibo" (i32.const 3)) (i32.const 2))
(assert_return (invoke "fibo" (i32.const 4)) (i32.const 3))
(assert_return (invoke "fibo" (i32.const 5)) (i32.const 5))
(assert_return (invoke "fibo" (i32.const 6)) (i32.const 8))
(assert_return (invoke "fibo" (i32.const 7)) (i32.const 13))
(assert_return (invoke "fibo" (i32.const 8)) (i32.const 21))
(assert_return (invoke "fibo" (i32.const 9)) (i32.const 34))
(assert_return (invoke "fibo" (i32.const 10)) (i32.const 55))

;; memoïzed version :

(module

  (memory 10) ;; the memory is 0-initialized !

  ;; please note that Wasm's text format (wast) allows both s-expression and instruction sequence notation
  ;; we sometimes mix them to make the code more readable

  (func $f (export "fibo")
    (param $n i32) ;; the function has one parameter $n of type `i32`
    (result i32) ;; the functions returns one value of type `i32`

    ;; if n < 0 then assert false
    (if
      (i32.lt_s
        (local.get $n)
        (i32.const 0))
    (then
      (unreachable)))

    ;; if n < 2 then n
    (if
      (i32.lt_s
        (local.get $n)
        (i32.const 2))
    (then
      (return (local.get $n))))

    ;; if memory.(4n) = 0, then we need to compute and store it
    (if
      (i32.eqz
        (i32.load (i32.mul (i32.const 4) (local.get $n))))
    (then
      local.get $n                                     ;; stack: [n]
      i32.const 4                                      ;; stack: [4 ; n]
      i32.mul                                          ;; stack: [4n]
      (call $f (i32.sub (local.get $n) (i32.const 1))) ;; stack: [f(n - 1) ; 4n]
      (call $f (i32.sub (local.get $n) (i32.const 2))) ;; stack: [f(n - 2) ; f(n - 1) ; 4n]
      i32.add                                          ;; stack: [f(n) ; 4n]
      i32.store ))

    ;; we can return the value at memory.(4n)
    local.get $n
    i32.const 4
    i32.mul
    i32.load
    return
  )
)


(assert_return (invoke "fibo" (i32.const 0)) (i32.const 0))
(assert_return (invoke "fibo" (i32.const 1)) (i32.const 1))
(assert_return (invoke "fibo" (i32.const 2)) (i32.const 1))
(assert_return (invoke "fibo" (i32.const 3)) (i32.const 2))
(assert_return (invoke "fibo" (i32.const 4)) (i32.const 3))
(assert_return (invoke "fibo" (i32.const 5)) (i32.const 5))
(assert_return (invoke "fibo" (i32.const 6)) (i32.const 8))
(assert_return (invoke "fibo" (i32.const 7)) (i32.const 13))
(assert_return (invoke "fibo" (i32.const 8)) (i32.const 21))
(assert_return (invoke "fibo" (i32.const 9)) (i32.const 34))
(assert_return (invoke "fibo" (i32.const 10)) (i32.const 55))
