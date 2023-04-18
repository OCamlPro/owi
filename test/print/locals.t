print optimized locals:
  $ dune exec -- ./print_optimized.exe locals.wast
  (module
    (func $f1 (param $a1 i32) (param $a2 i32) (param $a3 i32)  (local $l1 i32) (local $l2 i32)
      local.get 3
      drop
      local.get 4
      drop
    )
    (func $f2 (param $a1 i32) (param $a2 i32) (param $a3 i32)  (local $l1 i32)
      local.get 3
      drop
    )
    (func $f3 (param $a1 i32) (param $a2 i32) (param $a3 i32)  (local $l2 i32)
      local.get 3
      drop
    )
    (func $f4 (param $a1 i32) (param $a2 i32) (param $a3 i32)  
      local.get 0
      drop
    )
    (func $f5 (param $a1 i32) (param $a2 i32) (param $a3 i32)  
      local.get 1
      drop
    )
    (func $f6 (param $a1 i32) (param $a2 i32) (param $a3 i32)  
      local.get 2
      drop
    )
    (func $start   
      i32.const 0
      i32.const 1
      i32.const 2
      call 0
      i32.const 0
      i32.const 1
      i32.const 2
      call 1
      i32.const 0
      i32.const 1
      i32.const 2
      call 2
      i32.const 0
      i32.const 1
      i32.const 2
      call 3
      i32.const 0
      i32.const 1
      i32.const 2
      call 4
      i32.const 0
      i32.const 1
      i32.const 2
      call 5
      drop
    )
    (start 6)
  )
