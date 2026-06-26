(module
  (import "owi" "i32_symbol" (func $i32_symbol (result i32)))
  (import "owi" "assume" (func $assume (param i32)))

  (func $start (local $n i32)
    (call $i32_symbol)
    (local.tee $n)
    i32.const 0
    i32.gt_s
    (call $assume)
    i32.const 100
    (local.get $n)
    i32.div_s
    i32.const 0
    i32.gt_s
    (call $assume)
  )
  (start $start)
)
