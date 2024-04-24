print optimized locals:
  $ owi fmt locals.wat
  (module
    (func $f1 (param $a1 i32) (param $a2 i32) (param $a3 i32) (local $l1 i32) (local $l2 i32)
      local.get $l1
      local.get $l2
      i32.add
      drop
    )
    (func $f2 (param $a1 i32) (param $a2 i32) (param $a3 i32) (local $l1 i32) (local $l2 i32)
      local.get $l1
      i32.const 0
      i32.add
      drop
    )
    (func $f3 (param $a1 i32) (param $a2 i32) (param $a3 i32) (local $l1 i32) (local $l2 i32)
      local.get $l2
      i32.const 0
      i32.add
      drop
    )
    (func $f4 (param $a1 i32) (param $a2 i32) (param $a3 i32) (local $l1 i32) (local $l2 i32)
      local.get $a1
      i32.const 0
      i32.add
      drop
    )
    (func $f5 (param $a1 i32) (param $a2 i32) (param $a3 i32) (local $l1 i32) (local $l2 i32)
      local.get $a2
      i32.const 0
      i32.add
      drop
    )
    (func $f6 (param $a1 i32) (param $a2 i32) (param $a3 i32) (local $l1 i32) (local $l2 i32)
      local.get $a3
      i32.const 0
      i32.add
      drop
    )
    (func $f7 (param $a1 i32) (param $a2 i32) (param $a3 i32) (local $l1 i32) (local $l2 i32)
      local.get $l2
      local.get $a1
      i32.add
      drop
    )
    (func $f8 (param $a1 i32) (param $a2 i32) (param $a3 i32) (local $l1 i32) (local $l2 i32)
      local.get $a1
      local.get $a2
      i32.add
      drop
    )
    (func $start
      i32.const 0
      i32.const 1
      i32.const 2
      call $f1
      i32.const 0
      i32.const 1
      i32.const 2
      call $f2
      i32.const 0
      i32.const 1
      i32.const 2
      call $f3
      i32.const 0
      i32.const 1
      i32.const 2
      call $f4
      i32.const 0
      i32.const 1
      i32.const 2
      call $f5
      i32.const 0
      i32.const 1
      i32.const 2
      call $f6
      i32.const 0
      i32.const 1
      i32.const 2
      call $f7
      i32.const 0
      i32.const 1
      i32.const 2
      call $f8
    )
    (start $start)
  )
