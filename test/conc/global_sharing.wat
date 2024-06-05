;; Loads symbolic memory in various iterations
(module
  (import "symbolic" "i32_symbol" (func $i32 (result i32)))
  (import "symbolic" "assert" (func $assert (param i32)))

  (func $test_globals (param i32)
        (global.get 0)
        (i32.const 2)
        (i32.sub)
        (global.set 0)
        (global.get 0)
        (local.get 0)
        (i32.eq)
        (call $assert))

  (func $main
        (i32.const 40)
        (call $test_globals)
        (if (i32.eq (call $i32) (i32.const 0))
          (then
            (i32.const 38)
            (call $test_globals))))
  (memory $0 1)
  (global $0 (mut i32) (i32.const 42))
  (export "main" (func $main))
  (start $main))
