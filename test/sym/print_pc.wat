(module
  (import "symbolic" "i32_symbol" (func $i32_symbol (result i32)))

  (func $f (export "f") (param $x i32)

    (if (i32.lt_u (local.get $x) (i32.const 1))
      (then unreachable ))
  ))
