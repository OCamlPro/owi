
(module

  (global $import (import "a" "b") i32)
  (global $g1 i32 (i32.add (i32.const 0) (i32.const 23)))


  (func $start
    global.get $g1
    (if
    (then call $a)
    (else call $b)
    )
  )

  (func $a 
  (block
  call $b)
  (loop
  return_call $c))

  (func $b)

  (func $c)

  (func $unreachable)

  (func $e)

  (export "e" (func $e))

  (start $start))
