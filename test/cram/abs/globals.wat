(module
  (import "owi" "i32_symbol" (func $i32_symbol (result i32)))
  (global $x (mut i32) (i32.const 42))
  
  (func $start
    (call $i32_symbol)
    (if (then
      i32.const 42
      global.set $x
    ) (else
      i32.const 54
      global.set $x
    ))
    global.get $x
    drop
  )
  
  (start $start)
)
