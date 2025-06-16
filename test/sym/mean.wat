(module
  (import "owi" "i32_symbol" (func $i32_symbol (result i32)))

  (func $mean (export "mean") (param $x i32) (param $y i32) (result i32)
    (local $res i32)

    (i32.div_u (i32.add (local.get $x) (local.get $y)) (i32.const 2))
    local.set $res

    (if (i32.lt_u (local.get $res) (local.get $x))
      (then
	(if (i32.lt_u (local.get $res) (local.get $y))
	  (then
	    unreachable
	  )
	)
      )
    )
    local.get $res
  )

  (func $start
    call $i32_symbol
    call $i32_symbol
    call $mean
    drop
  )
  (start $start))
