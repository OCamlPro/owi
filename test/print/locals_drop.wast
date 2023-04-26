(module

  ;; the 2 local variables are used
  (func $f1 (param $a1 i32) (param $a2 i32) (param $a3 i32)
    (local $l1 i32)
    (local $l2 i32)
    local.get $l1
    drop
    local.get $l2
    drop
  )

  ;; only the 1st local variable is used
  (func $f2 (param $a1 i32) (param $a2 i32) (param $a3 i32)
    (local $l1 i32)
    (local $l2 i32)
    local.get $l1
    drop
  )

  ;; only the 2nd local variable is used
  (func $f3 (param $a1 i32) (param $a2 i32) (param $a3 i32)
    (local $l1 i32)
    (local $l2 i32)
    local.get $l2
    drop
  )

  ;; no local variable are used (1st argument is used)
  (func $f4 (param $a1 i32) (param $a2 i32) (param $a3 i32)
    (local $l1 i32)
    (local $l2 i32)
    local.get $a1
    drop
  )

  ;; no local variable are used (2nd argument is used)
  (func $f5 (param $a1 i32) (param $a2 i32) (param $a3 i32)
    (local $l1 i32)
    (local $l2 i32)
    local.get $a2
    drop
  )

  ;; no local variable are used (3rd argument is used)
  (func $f6 (param $a1 i32) (param $a2 i32) (param $a3 i32)
    (local $l1 i32)
    (local $l2 i32)
    local.get $a3
    drop
  )

  ;; only the 2nd local variable is used, and the 1st argument
  (func $f7 (param $a1 i32) (param $a2 i32) (param $a3 i32)
    (local $l1 i32)
    (local $l2 i32)
    local.get $l2
    drop
    local.get $a1
    drop
  )

  ;; no local variable are used, 1st and 2nd argument are used
  (func $f8 (param $a1 i32) (param $a2 i32) (param $a3 i32)
    (local $l1 i32)
    (local $l2 i32)
    local.get $a1
    drop
    local.get $a2
    drop
  )

  (func $start
    (call $f1 (i32.const 0) (i32.const 1) (i32.const 2))
    (call $f2 (i32.const 0) (i32.const 1) (i32.const 2))
    (call $f3 (i32.const 0) (i32.const 1) (i32.const 2))
    (call $f4 (i32.const 0) (i32.const 1) (i32.const 2))
    (call $f5 (i32.const 0) (i32.const 1) (i32.const 2))
    (call $f6 (i32.const 0) (i32.const 1) (i32.const 2))
    (call $f7 (i32.const 0) (i32.const 1) (i32.const 2))
    (call $f8 (i32.const 0) (i32.const 1) (i32.const 2))
    drop
  )

  (start $start)
)
