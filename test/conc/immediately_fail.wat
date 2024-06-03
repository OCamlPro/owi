(module
  (import "symbolic" "assert" (func $assert (param i32)))
  (func $f
    (i32.const 0)
    (call $assert))
  (start $f))
