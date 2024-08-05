(module
  (memory $m 1)
  (func $start
    i32.const 0x00010000
    i32.const 0xbaaaaaad
    i32.store)
  (start $start))
