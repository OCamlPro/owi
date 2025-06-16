(module
  (import "owi" "assert" (func $assert (param i32)))
  (import "owi" "assume" (func $assume (param i32)))
  (import "owi" "i32_symbol" (func $i32_symbol (result i32)))

  (start $f)
  (func $f
    (local $x i32) (local $y i32)
    (local.set $x (call $i32_symbol))
    (local.set $y (call $i32_symbol))
    (call $assume (i32.lt_u (local.get $x) (local.get $y)))
    (call $assert (i32.gt_u (local.get $x) (local.get $y))))
)
