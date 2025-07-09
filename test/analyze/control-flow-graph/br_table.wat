(module
(global $A (export "A") (mut i32) (i32.const 0))
(global $B (export "B") (mut i32) (i32.const 0))
(func (export "set") (param $select i32) (param $value i32)
  (block
    (block
      (local.get $select)
      (br_table 1 0 2))
    (local.get $value)
    (global.set $A)
    (local.get $value)
    (i32.const 42)
    (i32.eq)
    (br_if 0)
    (return)) 
  (local.get $value)
  (global.set $B))
)
