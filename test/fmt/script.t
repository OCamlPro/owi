print script:
  $ owi fmt script.wast
  (module
    (func (param $x i32) (param $y i32) (result i32)
      local.get $x
      local.get $y
      i32.add
    )
    (export "add" (func ))
    (func (param $x i32) (param $y i32) (result i32)
      local.get $x
      local.get $y
      i32.sub
    )
    (export "sub" (func ))
    (func (param $x i32) (param $y i32) (result i32)
      local.get $x
      local.get $y
      i32.mul
    )
    (export "mul" (func ))
    (func (param $x i32) (param $y i32) (result i32)
      local.get $x
      local.get $y
      i32.div_s
    )
    (export "div_s" (func ))
    (func (param $x i32) (param $y i32) (result i32)
      local.get $x
      local.get $y
      i32.div_u
    )
    (export "div_u" (func ))
    (func (param $x i32) (param $y i32) (result i32)
      local.get $x
      local.get $y
      i32.rem_s
    )
    (export "rem_s" (func ))
    (func (param $x i32) (param $y i32) (result i32)
      local.get $x
      local.get $y
      i32.rem_u
    )
    (export "rem_u" (func ))
    (func (param $x i32) (param $y i32) (result i32)
      local.get $x
      local.get $y
      i32.and
    )
    (export "and" (func ))
    (func (param $x i32) (param $y i32) (result i32)
      local.get $x
      local.get $y
      i32.or
    )
    (export "or" (func ))
    (func (param $x i32) (param $y i32) (result i32)
      local.get $x
      local.get $y
      i32.xor
    )
    (export "xor" (func ))
    (func (param $x i32) (param $y i32) (result i32)
      local.get $x
      local.get $y
      i32.shl
    )
    (export "shl" (func ))
    (func (param $x i32) (param $y i32) (result i32)
      local.get $x
      local.get $y
      i32.shr_s
    )
    (export "shr_s" (func ))
    (func (param $x i32) (param $y i32) (result i32)
      local.get $x
      local.get $y
      i32.shr_u
    )
    (export "shr_u" (func ))
    (func (param $x i32) (param $y i32) (result i32)
      local.get $x
      local.get $y
      i32.rotl
    )
    (export "rotl" (func ))
    (func (param $x i32) (param $y i32) (result i32)
      local.get $x
      local.get $y
      i32.rotr
    )
    (export "rotr" (func ))
    (func (param $x i32) (result i32)
      local.get $x
      i32.clz
    )
    (export "clz" (func ))
    (func (param $x i32) (result i32)
      local.get $x
      i32.ctz
    )
    (export "ctz" (func ))
    (func (param $x i32) (result i32)
      local.get $x
      i32.popcnt
    )
    (export "popcnt" (func ))
    (func (param $x i32) (result i32)
      local.get $x
      i32.extend8_s
    )
    (export "extend8_s" (func ))
    (func (param $x i32) (result i32)
      local.get $x
      i32.extend16_s
    )
    (export "extend16_s" (func ))
    (func (param $x i32) (result i32)
      local.get $x
      i32.eqz
    )
    (export "eqz" (func ))
    (func (param $x i32) (param $y i32) (result i32)
      local.get $x
      local.get $y
      i32.eq
    )
    (export "eq" (func ))
    (func (param $x i32) (param $y i32) (result i32)
      local.get $x
      local.get $y
      i32.ne
    )
    (export "ne" (func ))
    (func (param $x i32) (param $y i32) (result i32)
      local.get $x
      local.get $y
      i32.lt_s
    )
    (export "lt_s" (func ))
    (func (param $x i32) (param $y i32) (result i32)
      local.get $x
      local.get $y
      i32.lt_u
    )
    (export "lt_u" (func ))
    (func (param $x i32) (param $y i32) (result i32)
      local.get $x
      local.get $y
      i32.le_s
    )
    (export "le_s" (func ))
    (func (param $x i32) (param $y i32) (result i32)
      local.get $x
      local.get $y
      i32.le_u
    )
    (export "le_u" (func ))
    (func (param $x i32) (param $y i32) (result i32)
      local.get $x
      local.get $y
      i32.gt_s
    )
    (export "gt_s" (func ))
    (func (param $x i32) (param $y i32) (result i32)
      local.get $x
      local.get $y
      i32.gt_u
    )
    (export "gt_u" (func ))
    (func (param $x i32) (param $y i32) (result i32)
      local.get $x
      local.get $y
      i32.ge_s
    )
    (export "ge_s" (func ))
    (func (param $x i32) (param $y i32) (result i32)
      local.get $x
      local.get $y
      i32.ge_u
    )
    (export "ge_u" (func ))
  )
  (assert_return (invoke "add" (i32.const 1) (i32.const 1)) (i32.const 2))
  (assert_return (invoke "add" (i32.const 1) (i32.const 0)) (i32.const 1))
  (assert_return (invoke "add" (i32.const -1) (i32.const -1)) (i32.const -2))
  (assert_return (invoke "add" (i32.const -1) (i32.const 1)) (i32.const 0))
  (assert_return (invoke "add" (i32.const 2147483647) (i32.const 1)) (i32.const -2147483648))
  (assert_return (invoke "add" (i32.const -2147483648) (i32.const -1)) (i32.const 2147483647))
  (assert_return (invoke "add" (i32.const -2147483648) (i32.const -2147483648)) (i32.const 0))
  (assert_return (invoke "add" (i32.const 1073741823) (i32.const 1)) (i32.const 1073741824))
  (assert_return (invoke "sub" (i32.const 1) (i32.const 1)) (i32.const 0))
  (assert_return (invoke "sub" (i32.const 1) (i32.const 0)) (i32.const 1))
  (assert_return (invoke "sub" (i32.const -1) (i32.const -1)) (i32.const 0))
  (assert_return (invoke "sub" (i32.const 2147483647) (i32.const -1)) (i32.const -2147483648))
  (assert_return (invoke "sub" (i32.const -2147483648) (i32.const 1)) (i32.const 2147483647))
  (assert_return (invoke "sub" (i32.const -2147483648) (i32.const -2147483648)) (i32.const 0))
  (assert_return (invoke "sub" (i32.const 1073741823) (i32.const -1)) (i32.const 1073741824))
  (assert_return (invoke "mul" (i32.const 1) (i32.const 1)) (i32.const 1))
  (assert_return (invoke "mul" (i32.const 1) (i32.const 0)) (i32.const 0))
  (assert_return (invoke "mul" (i32.const -1) (i32.const -1)) (i32.const 1))
  (assert_return (invoke "mul" (i32.const 268435456) (i32.const 4096)) (i32.const 0))
  (assert_return (invoke "mul" (i32.const -2147483648) (i32.const 0)) (i32.const 0))
  (assert_return (invoke "mul" (i32.const -2147483648) (i32.const -1)) (i32.const -2147483648))
  (assert_return (invoke "mul" (i32.const 2147483647) (i32.const -1)) (i32.const -2147483647))
  (assert_return (invoke "mul" (i32.const 19088743) (i32.const 1985229328)) (i32.const 898528368))
  (assert_return (invoke "mul" (i32.const 2147483647) (i32.const 2147483647)) (i32.const 1))
  (assert_trap (invoke "div_s" (i32.const 1) (i32.const 0)) "integer divide by zero")
  (assert_trap (invoke "div_s" (i32.const 0) (i32.const 0)) "integer divide by zero")
  (assert_trap (invoke "div_s" (i32.const -2147483648) (i32.const -1)) "integer overflow")
  (assert_trap (invoke "div_s" (i32.const -2147483648) (i32.const 0)) "integer divide by zero")
  (assert_return (invoke "div_s" (i32.const 1) (i32.const 1)) (i32.const 1))
  (assert_return (invoke "div_s" (i32.const 0) (i32.const 1)) (i32.const 0))
  (assert_return (invoke "div_s" (i32.const 0) (i32.const -1)) (i32.const 0))
  (assert_return (invoke "div_s" (i32.const -1) (i32.const -1)) (i32.const 1))
  (assert_return (invoke "div_s" (i32.const -2147483648) (i32.const 2)) (i32.const -1073741824))
  (assert_return (invoke "div_s" (i32.const -2147483647) (i32.const 1000)) (i32.const -2147483))
  (assert_return (invoke "div_s" (i32.const 5) (i32.const 2)) (i32.const 2))
  (assert_return (invoke "div_s" (i32.const -5) (i32.const 2)) (i32.const -2))
  (assert_return (invoke "div_s" (i32.const 5) (i32.const -2)) (i32.const -2))
  (assert_return (invoke "div_s" (i32.const -5) (i32.const -2)) (i32.const 2))
  (assert_return (invoke "div_s" (i32.const 7) (i32.const 3)) (i32.const 2))
  (assert_return (invoke "div_s" (i32.const -7) (i32.const 3)) (i32.const -2))
  (assert_return (invoke "div_s" (i32.const 7) (i32.const -3)) (i32.const -2))
  (assert_return (invoke "div_s" (i32.const -7) (i32.const -3)) (i32.const 2))
  (assert_return (invoke "div_s" (i32.const 11) (i32.const 5)) (i32.const 2))
  (assert_return (invoke "div_s" (i32.const 17) (i32.const 7)) (i32.const 2))
  (assert_trap (invoke "div_u" (i32.const 1) (i32.const 0)) "integer divide by zero")
  (assert_trap (invoke "div_u" (i32.const 0) (i32.const 0)) "integer divide by zero")
  (assert_return (invoke "div_u" (i32.const 1) (i32.const 1)) (i32.const 1))
  (assert_return (invoke "div_u" (i32.const 0) (i32.const 1)) (i32.const 0))
  (assert_return (invoke "div_u" (i32.const -1) (i32.const -1)) (i32.const 1))
  (assert_return (invoke "div_u" (i32.const -2147483648) (i32.const -1)) (i32.const 0))
  (assert_return (invoke "div_u" (i32.const -2147483648) (i32.const 2)) (i32.const 1073741824))
  (assert_return (invoke "div_u" (i32.const -1880092688) (i32.const 65537)) (i32.const 36847))
  (assert_return (invoke "div_u" (i32.const -2147483647) (i32.const 1000)) (i32.const 2147483))
  (assert_return (invoke "div_u" (i32.const 5) (i32.const 2)) (i32.const 2))
  (assert_return (invoke "div_u" (i32.const -5) (i32.const 2)) (i32.const 2147483645))
  (assert_return (invoke "div_u" (i32.const 5) (i32.const -2)) (i32.const 0))
  (assert_return (invoke "div_u" (i32.const -5) (i32.const -2)) (i32.const 0))
  (assert_return (invoke "div_u" (i32.const 7) (i32.const 3)) (i32.const 2))
  (assert_return (invoke "div_u" (i32.const 11) (i32.const 5)) (i32.const 2))
  (assert_return (invoke "div_u" (i32.const 17) (i32.const 7)) (i32.const 2))
  (assert_trap (invoke "rem_s" (i32.const 1) (i32.const 0)) "integer divide by zero")
  (assert_trap (invoke "rem_s" (i32.const 0) (i32.const 0)) "integer divide by zero")
  (assert_return (invoke "rem_s" (i32.const 2147483647) (i32.const -1)) (i32.const 0))
  (assert_return (invoke "rem_s" (i32.const 1) (i32.const 1)) (i32.const 0))
  (assert_return (invoke "rem_s" (i32.const 0) (i32.const 1)) (i32.const 0))
  (assert_return (invoke "rem_s" (i32.const 0) (i32.const -1)) (i32.const 0))
  (assert_return (invoke "rem_s" (i32.const -1) (i32.const -1)) (i32.const 0))
  (assert_return (invoke "rem_s" (i32.const -2147483648) (i32.const -1)) (i32.const 0))
  (assert_return (invoke "rem_s" (i32.const -2147483648) (i32.const 2)) (i32.const 0))
  (assert_return (invoke "rem_s" (i32.const -2147483647) (i32.const 1000)) (i32.const -647))
  (assert_return (invoke "rem_s" (i32.const 5) (i32.const 2)) (i32.const 1))
  (assert_return (invoke "rem_s" (i32.const -5) (i32.const 2)) (i32.const -1))
  (assert_return (invoke "rem_s" (i32.const 5) (i32.const -2)) (i32.const 1))
  (assert_return (invoke "rem_s" (i32.const -5) (i32.const -2)) (i32.const -1))
  (assert_return (invoke "rem_s" (i32.const 7) (i32.const 3)) (i32.const 1))
  (assert_return (invoke "rem_s" (i32.const -7) (i32.const 3)) (i32.const -1))
  (assert_return (invoke "rem_s" (i32.const 7) (i32.const -3)) (i32.const 1))
  (assert_return (invoke "rem_s" (i32.const -7) (i32.const -3)) (i32.const -1))
  (assert_return (invoke "rem_s" (i32.const 11) (i32.const 5)) (i32.const 1))
  (assert_return (invoke "rem_s" (i32.const 17) (i32.const 7)) (i32.const 3))
  (assert_trap (invoke "rem_u" (i32.const 1) (i32.const 0)) "integer divide by zero")
  (assert_trap (invoke "rem_u" (i32.const 0) (i32.const 0)) "integer divide by zero")
  (assert_return (invoke "rem_u" (i32.const 1) (i32.const 1)) (i32.const 0))
  (assert_return (invoke "rem_u" (i32.const 0) (i32.const 1)) (i32.const 0))
  (assert_return (invoke "rem_u" (i32.const -1) (i32.const -1)) (i32.const 0))
  (assert_return (invoke "rem_u" (i32.const -2147483648) (i32.const -1)) (i32.const -2147483648))
  (assert_return (invoke "rem_u" (i32.const -2147483648) (i32.const 2)) (i32.const 0))
  (assert_return (invoke "rem_u" (i32.const -1880092688) (i32.const 65537)) (i32.const 32769))
  (assert_return (invoke "rem_u" (i32.const -2147483647) (i32.const 1000)) (i32.const 649))
  (assert_return (invoke "rem_u" (i32.const 5) (i32.const 2)) (i32.const 1))
  (assert_return (invoke "rem_u" (i32.const -5) (i32.const 2)) (i32.const 1))
  (assert_return (invoke "rem_u" (i32.const 5) (i32.const -2)) (i32.const 5))
  (assert_return (invoke "rem_u" (i32.const -5) (i32.const -2)) (i32.const -5))
  (assert_return (invoke "rem_u" (i32.const 7) (i32.const 3)) (i32.const 1))
  (assert_return (invoke "rem_u" (i32.const 11) (i32.const 5)) (i32.const 1))
  (assert_return (invoke "rem_u" (i32.const 17) (i32.const 7)) (i32.const 3))
  (assert_return (invoke "and" (i32.const 1) (i32.const 0)) (i32.const 0))
  (assert_return (invoke "and" (i32.const 0) (i32.const 1)) (i32.const 0))
  (assert_return (invoke "and" (i32.const 1) (i32.const 1)) (i32.const 1))
  (assert_return (invoke "and" (i32.const 0) (i32.const 0)) (i32.const 0))
  (assert_return (invoke "and" (i32.const 2147483647) (i32.const -2147483648)) (i32.const 0))
  (assert_return (invoke "and" (i32.const 2147483647) (i32.const -1)) (i32.const 2147483647))
  (assert_return (invoke "and" (i32.const -252641281) (i32.const -3856)) (i32.const -252645136))
  (assert_return (invoke "and" (i32.const -1) (i32.const -1)) (i32.const -1))
  (assert_return (invoke "or" (i32.const 1) (i32.const 0)) (i32.const 1))
  (assert_return (invoke "or" (i32.const 0) (i32.const 1)) (i32.const 1))
  (assert_return (invoke "or" (i32.const 1) (i32.const 1)) (i32.const 1))
  (assert_return (invoke "or" (i32.const 0) (i32.const 0)) (i32.const 0))
  (assert_return (invoke "or" (i32.const 2147483647) (i32.const -2147483648)) (i32.const -1))
  (assert_return (invoke "or" (i32.const -2147483648) (i32.const 0)) (i32.const -2147483648))
  (assert_return (invoke "or" (i32.const -252641281) (i32.const -3856)) (i32.const -1))
  (assert_return (invoke "or" (i32.const -1) (i32.const -1)) (i32.const -1))
  (assert_return (invoke "xor" (i32.const 1) (i32.const 0)) (i32.const 1))
  (assert_return (invoke "xor" (i32.const 0) (i32.const 1)) (i32.const 1))
  (assert_return (invoke "xor" (i32.const 1) (i32.const 1)) (i32.const 0))
  (assert_return (invoke "xor" (i32.const 0) (i32.const 0)) (i32.const 0))
  (assert_return (invoke "xor" (i32.const 2147483647) (i32.const -2147483648)) (i32.const -1))
  (assert_return (invoke "xor" (i32.const -2147483648) (i32.const 0)) (i32.const -2147483648))
  (assert_return (invoke "xor" (i32.const -1) (i32.const -2147483648)) (i32.const 2147483647))
  (assert_return (invoke "xor" (i32.const -1) (i32.const 2147483647)) (i32.const -2147483648))
  (assert_return (invoke "xor" (i32.const -252641281) (i32.const -3856)) (i32.const 252645135))
  (assert_return (invoke "xor" (i32.const -1) (i32.const -1)) (i32.const 0))
  (assert_return (invoke "shl" (i32.const 1) (i32.const 1)) (i32.const 2))
  (assert_return (invoke "shl" (i32.const 1) (i32.const 0)) (i32.const 1))
  (assert_return (invoke "shl" (i32.const 2147483647) (i32.const 1)) (i32.const -2))
  (assert_return (invoke "shl" (i32.const -1) (i32.const 1)) (i32.const -2))
  (assert_return (invoke "shl" (i32.const -2147483648) (i32.const 1)) (i32.const 0))
  (assert_return (invoke "shl" (i32.const 1073741824) (i32.const 1)) (i32.const -2147483648))
  (assert_return (invoke "shl" (i32.const 1) (i32.const 31)) (i32.const -2147483648))
  (assert_return (invoke "shl" (i32.const 1) (i32.const 32)) (i32.const 1))
  (assert_return (invoke "shl" (i32.const 1) (i32.const 33)) (i32.const 2))
  (assert_return (invoke "shl" (i32.const 1) (i32.const -1)) (i32.const -2147483648))
  (assert_return (invoke "shl" (i32.const 1) (i32.const 2147483647)) (i32.const -2147483648))
  (assert_return (invoke "shr_s" (i32.const 1) (i32.const 1)) (i32.const 0))
  (assert_return (invoke "shr_s" (i32.const 1) (i32.const 0)) (i32.const 1))
  (assert_return (invoke "shr_s" (i32.const -1) (i32.const 1)) (i32.const -1))
  (assert_return (invoke "shr_s" (i32.const 2147483647) (i32.const 1)) (i32.const 1073741823))
  (assert_return (invoke "shr_s" (i32.const -2147483648) (i32.const 1)) (i32.const -1073741824))
  (assert_return (invoke "shr_s" (i32.const 1073741824) (i32.const 1)) (i32.const 536870912))
  (assert_return (invoke "shr_s" (i32.const 1) (i32.const 32)) (i32.const 1))
  (assert_return (invoke "shr_s" (i32.const 1) (i32.const 33)) (i32.const 0))
  (assert_return (invoke "shr_s" (i32.const 1) (i32.const -1)) (i32.const 0))
  (assert_return (invoke "shr_s" (i32.const 1) (i32.const 2147483647)) (i32.const 0))
  (assert_return (invoke "shr_s" (i32.const 1) (i32.const -2147483648)) (i32.const 1))
  (assert_return (invoke "shr_s" (i32.const -2147483648) (i32.const 31)) (i32.const -1))
  (assert_return (invoke "shr_s" (i32.const -1) (i32.const 32)) (i32.const -1))
  (assert_return (invoke "shr_s" (i32.const -1) (i32.const 33)) (i32.const -1))
  (assert_return (invoke "shr_s" (i32.const -1) (i32.const -1)) (i32.const -1))
  (assert_return (invoke "shr_s" (i32.const -1) (i32.const 2147483647)) (i32.const -1))
  (assert_return (invoke "shr_s" (i32.const -1) (i32.const -2147483648)) (i32.const -1))
  (assert_return (invoke "shr_u" (i32.const 1) (i32.const 1)) (i32.const 0))
  (assert_return (invoke "shr_u" (i32.const 1) (i32.const 0)) (i32.const 1))
  (assert_return (invoke "shr_u" (i32.const -1) (i32.const 1)) (i32.const 2147483647))
  (assert_return (invoke "shr_u" (i32.const 2147483647) (i32.const 1)) (i32.const 1073741823))
  (assert_return (invoke "shr_u" (i32.const -2147483648) (i32.const 1)) (i32.const 1073741824))
  (assert_return (invoke "shr_u" (i32.const 1073741824) (i32.const 1)) (i32.const 536870912))
  (assert_return (invoke "shr_u" (i32.const 1) (i32.const 32)) (i32.const 1))
  (assert_return (invoke "shr_u" (i32.const 1) (i32.const 33)) (i32.const 0))
  (assert_return (invoke "shr_u" (i32.const 1) (i32.const -1)) (i32.const 0))
  (assert_return (invoke "shr_u" (i32.const 1) (i32.const 2147483647)) (i32.const 0))
  (assert_return (invoke "shr_u" (i32.const 1) (i32.const -2147483648)) (i32.const 1))
  (assert_return (invoke "shr_u" (i32.const -2147483648) (i32.const 31)) (i32.const 1))
  (assert_return (invoke "shr_u" (i32.const -1) (i32.const 32)) (i32.const -1))
  (assert_return (invoke "shr_u" (i32.const -1) (i32.const 33)) (i32.const 2147483647))
  (assert_return (invoke "shr_u" (i32.const -1) (i32.const -1)) (i32.const 1))
  (assert_return (invoke "shr_u" (i32.const -1) (i32.const 2147483647)) (i32.const 1))
  (assert_return (invoke "shr_u" (i32.const -1) (i32.const -2147483648)) (i32.const -1))
  (assert_return (invoke "rotl" (i32.const 1) (i32.const 1)) (i32.const 2))
  (assert_return (invoke "rotl" (i32.const 1) (i32.const 0)) (i32.const 1))
  (assert_return (invoke "rotl" (i32.const -1) (i32.const 1)) (i32.const -1))
  (assert_return (invoke "rotl" (i32.const 1) (i32.const 32)) (i32.const 1))
  (assert_return (invoke "rotl" (i32.const -1412589450) (i32.const 1)) (i32.const 1469788397))
  (assert_return (invoke "rotl" (i32.const -33498112) (i32.const 4)) (i32.const -535969777))
  (assert_return (invoke "rotl" (i32.const -1329474845) (i32.const 5)) (i32.const 406477942))
  (assert_return (invoke "rotl" (i32.const 32768) (i32.const 37)) (i32.const 1048576))
  (assert_return (invoke "rotl" (i32.const -1329474845) (i32.const 65285)) (i32.const 406477942))
  (assert_return (invoke "rotl" (i32.const 1989852383) (i32.const -19)) (i32.const 1469837011))
  (assert_return (invoke "rotl" (i32.const 1989852383) (i32.const -2147483635)) (i32.const 1469837011))
  (assert_return (invoke "rotl" (i32.const 1) (i32.const 31)) (i32.const -2147483648))
  (assert_return (invoke "rotl" (i32.const -2147483648) (i32.const 1)) (i32.const 1))
  (assert_return (invoke "rotr" (i32.const 1) (i32.const 1)) (i32.const -2147483648))
  (assert_return (invoke "rotr" (i32.const 1) (i32.const 0)) (i32.const 1))
  (assert_return (invoke "rotr" (i32.const -1) (i32.const 1)) (i32.const -1))
  (assert_return (invoke "rotr" (i32.const 1) (i32.const 32)) (i32.const 1))
  (assert_return (invoke "rotr" (i32.const -16724992) (i32.const 1)) (i32.const 2139121152))
  (assert_return (invoke "rotr" (i32.const 524288) (i32.const 4)) (i32.const 32768))
  (assert_return (invoke "rotr" (i32.const -1329474845) (i32.const 5)) (i32.const 495324823))
  (assert_return (invoke "rotr" (i32.const 32768) (i32.const 37)) (i32.const 1024))
  (assert_return (invoke "rotr" (i32.const -1329474845) (i32.const 65285)) (i32.const 495324823))
  (assert_return (invoke "rotr" (i32.const 1989852383) (i32.const -19)) (i32.const -419711787))
  (assert_return (invoke "rotr" (i32.const 1989852383) (i32.const -2147483635)) (i32.const -419711787))
  (assert_return (invoke "rotr" (i32.const 1) (i32.const 31)) (i32.const 2))
  (assert_return (invoke "rotr" (i32.const -2147483648) (i32.const 31)) (i32.const 1))
  (assert_return (invoke "clz" (i32.const -1)) (i32.const 0))
  (assert_return (invoke "clz" (i32.const 0)) (i32.const 32))
  (assert_return (invoke "clz" (i32.const 32768)) (i32.const 16))
  (assert_return (invoke "clz" (i32.const 255)) (i32.const 24))
  (assert_return (invoke "clz" (i32.const -2147483648)) (i32.const 0))
  (assert_return (invoke "clz" (i32.const 1)) (i32.const 31))
  (assert_return (invoke "clz" (i32.const 2)) (i32.const 30))
  (assert_return (invoke "clz" (i32.const 2147483647)) (i32.const 1))
  (assert_return (invoke "ctz" (i32.const -1)) (i32.const 0))
  (assert_return (invoke "ctz" (i32.const 0)) (i32.const 32))
  (assert_return (invoke "ctz" (i32.const 32768)) (i32.const 15))
  (assert_return (invoke "ctz" (i32.const 65536)) (i32.const 16))
  (assert_return (invoke "ctz" (i32.const -2147483648)) (i32.const 31))
  (assert_return (invoke "ctz" (i32.const 2147483647)) (i32.const 0))
  (assert_return (invoke "popcnt" (i32.const -1)) (i32.const 32))
  (assert_return (invoke "popcnt" (i32.const 0)) (i32.const 0))
  (assert_return (invoke "popcnt" (i32.const 32768)) (i32.const 1))
  (assert_return (invoke "popcnt" (i32.const -2147450880)) (i32.const 2))
  (assert_return (invoke "popcnt" (i32.const 2147483647)) (i32.const 31))
  (assert_return (invoke "popcnt" (i32.const -1431655766)) (i32.const 16))
  (assert_return (invoke "popcnt" (i32.const 1431655765)) (i32.const 16))
  (assert_return (invoke "popcnt" (i32.const -559038737)) (i32.const 24))
  (assert_return (invoke "extend8_s" (i32.const 0)) (i32.const 0))
  (assert_return (invoke "extend8_s" (i32.const 127)) (i32.const 127))
  (assert_return (invoke "extend8_s" (i32.const 128)) (i32.const -128))
  (assert_return (invoke "extend8_s" (i32.const 255)) (i32.const -1))
  (assert_return (invoke "extend8_s" (i32.const 19088640)) (i32.const 0))
  (assert_return (invoke "extend8_s" (i32.const -19088768)) (i32.const -128))
  (assert_return (invoke "extend8_s" (i32.const -1)) (i32.const -1))
  (assert_return (invoke "extend16_s" (i32.const 0)) (i32.const 0))
  (assert_return (invoke "extend16_s" (i32.const 32767)) (i32.const 32767))
  (assert_return (invoke "extend16_s" (i32.const 32768)) (i32.const -32768))
  (assert_return (invoke "extend16_s" (i32.const 65535)) (i32.const -1))
  (assert_return (invoke "extend16_s" (i32.const 19070976)) (i32.const 0))
  (assert_return (invoke "extend16_s" (i32.const -19103744)) (i32.const -32768))
  (assert_return (invoke "extend16_s" (i32.const -1)) (i32.const -1))
  (assert_return (invoke "eqz" (i32.const 0)) (i32.const 1))
  (assert_return (invoke "eqz" (i32.const 1)) (i32.const 0))
  (assert_return (invoke "eqz" (i32.const -2147483648)) (i32.const 0))
  (assert_return (invoke "eqz" (i32.const 2147483647)) (i32.const 0))
  (assert_return (invoke "eqz" (i32.const -1)) (i32.const 0))
  (assert_return (invoke "eq" (i32.const 0) (i32.const 0)) (i32.const 1))
  (assert_return (invoke "eq" (i32.const 1) (i32.const 1)) (i32.const 1))
  (assert_return (invoke "eq" (i32.const -1) (i32.const 1)) (i32.const 0))
  (assert_return (invoke "eq" (i32.const -2147483648) (i32.const -2147483648)) (i32.const 1))
  (assert_return (invoke "eq" (i32.const 2147483647) (i32.const 2147483647)) (i32.const 1))
  (assert_return (invoke "eq" (i32.const -1) (i32.const -1)) (i32.const 1))
  (assert_return (invoke "eq" (i32.const 1) (i32.const 0)) (i32.const 0))
  (assert_return (invoke "eq" (i32.const 0) (i32.const 1)) (i32.const 0))
  (assert_return (invoke "eq" (i32.const -2147483648) (i32.const 0)) (i32.const 0))
  (assert_return (invoke "eq" (i32.const 0) (i32.const -2147483648)) (i32.const 0))
  (assert_return (invoke "eq" (i32.const -2147483648) (i32.const -1)) (i32.const 0))
  (assert_return (invoke "eq" (i32.const -1) (i32.const -2147483648)) (i32.const 0))
  (assert_return (invoke "eq" (i32.const -2147483648) (i32.const 2147483647)) (i32.const 0))
  (assert_return (invoke "eq" (i32.const 2147483647) (i32.const -2147483648)) (i32.const 0))
  (assert_return (invoke "ne" (i32.const 0) (i32.const 0)) (i32.const 0))
  (assert_return (invoke "ne" (i32.const 1) (i32.const 1)) (i32.const 0))
  (assert_return (invoke "ne" (i32.const -1) (i32.const 1)) (i32.const 1))
  (assert_return (invoke "ne" (i32.const -2147483648) (i32.const -2147483648)) (i32.const 0))
  (assert_return (invoke "ne" (i32.const 2147483647) (i32.const 2147483647)) (i32.const 0))
  (assert_return (invoke "ne" (i32.const -1) (i32.const -1)) (i32.const 0))
  (assert_return (invoke "ne" (i32.const 1) (i32.const 0)) (i32.const 1))
  (assert_return (invoke "ne" (i32.const 0) (i32.const 1)) (i32.const 1))
  (assert_return (invoke "ne" (i32.const -2147483648) (i32.const 0)) (i32.const 1))
  (assert_return (invoke "ne" (i32.const 0) (i32.const -2147483648)) (i32.const 1))
  (assert_return (invoke "ne" (i32.const -2147483648) (i32.const -1)) (i32.const 1))
  (assert_return (invoke "ne" (i32.const -1) (i32.const -2147483648)) (i32.const 1))
  (assert_return (invoke "ne" (i32.const -2147483648) (i32.const 2147483647)) (i32.const 1))
  (assert_return (invoke "ne" (i32.const 2147483647) (i32.const -2147483648)) (i32.const 1))
  (assert_return (invoke "lt_s" (i32.const 0) (i32.const 0)) (i32.const 0))
  (assert_return (invoke "lt_s" (i32.const 1) (i32.const 1)) (i32.const 0))
  (assert_return (invoke "lt_s" (i32.const -1) (i32.const 1)) (i32.const 1))
  (assert_return (invoke "lt_s" (i32.const -2147483648) (i32.const -2147483648)) (i32.const 0))
  (assert_return (invoke "lt_s" (i32.const 2147483647) (i32.const 2147483647)) (i32.const 0))
  (assert_return (invoke "lt_s" (i32.const -1) (i32.const -1)) (i32.const 0))
  (assert_return (invoke "lt_s" (i32.const 1) (i32.const 0)) (i32.const 0))
  (assert_return (invoke "lt_s" (i32.const 0) (i32.const 1)) (i32.const 1))
  (assert_return (invoke "lt_s" (i32.const -2147483648) (i32.const 0)) (i32.const 1))
  (assert_return (invoke "lt_s" (i32.const 0) (i32.const -2147483648)) (i32.const 0))
  (assert_return (invoke "lt_s" (i32.const -2147483648) (i32.const -1)) (i32.const 1))
  (assert_return (invoke "lt_s" (i32.const -1) (i32.const -2147483648)) (i32.const 0))
  (assert_return (invoke "lt_s" (i32.const -2147483648) (i32.const 2147483647)) (i32.const 1))
  (assert_return (invoke "lt_s" (i32.const 2147483647) (i32.const -2147483648)) (i32.const 0))
  (assert_return (invoke "lt_u" (i32.const 0) (i32.const 0)) (i32.const 0))
  (assert_return (invoke "lt_u" (i32.const 1) (i32.const 1)) (i32.const 0))
  (assert_return (invoke "lt_u" (i32.const -1) (i32.const 1)) (i32.const 0))
  (assert_return (invoke "lt_u" (i32.const -2147483648) (i32.const -2147483648)) (i32.const 0))
  (assert_return (invoke "lt_u" (i32.const 2147483647) (i32.const 2147483647)) (i32.const 0))
  (assert_return (invoke "lt_u" (i32.const -1) (i32.const -1)) (i32.const 0))
  (assert_return (invoke "lt_u" (i32.const 1) (i32.const 0)) (i32.const 0))
  (assert_return (invoke "lt_u" (i32.const 0) (i32.const 1)) (i32.const 1))
  (assert_return (invoke "lt_u" (i32.const -2147483648) (i32.const 0)) (i32.const 0))
  (assert_return (invoke "lt_u" (i32.const 0) (i32.const -2147483648)) (i32.const 1))
  (assert_return (invoke "lt_u" (i32.const -2147483648) (i32.const -1)) (i32.const 1))
  (assert_return (invoke "lt_u" (i32.const -1) (i32.const -2147483648)) (i32.const 0))
  (assert_return (invoke "lt_u" (i32.const -2147483648) (i32.const 2147483647)) (i32.const 0))
  (assert_return (invoke "lt_u" (i32.const 2147483647) (i32.const -2147483648)) (i32.const 1))
  (assert_return (invoke "le_s" (i32.const 0) (i32.const 0)) (i32.const 1))
  (assert_return (invoke "le_s" (i32.const 1) (i32.const 1)) (i32.const 1))
  (assert_return (invoke "le_s" (i32.const -1) (i32.const 1)) (i32.const 1))
  (assert_return (invoke "le_s" (i32.const -2147483648) (i32.const -2147483648)) (i32.const 1))
  (assert_return (invoke "le_s" (i32.const 2147483647) (i32.const 2147483647)) (i32.const 1))
  (assert_return (invoke "le_s" (i32.const -1) (i32.const -1)) (i32.const 1))
  (assert_return (invoke "le_s" (i32.const 1) (i32.const 0)) (i32.const 0))
  (assert_return (invoke "le_s" (i32.const 0) (i32.const 1)) (i32.const 1))
  (assert_return (invoke "le_s" (i32.const -2147483648) (i32.const 0)) (i32.const 1))
  (assert_return (invoke "le_s" (i32.const 0) (i32.const -2147483648)) (i32.const 0))
  (assert_return (invoke "le_s" (i32.const -2147483648) (i32.const -1)) (i32.const 1))
  (assert_return (invoke "le_s" (i32.const -1) (i32.const -2147483648)) (i32.const 0))
  (assert_return (invoke "le_s" (i32.const -2147483648) (i32.const 2147483647)) (i32.const 1))
  (assert_return (invoke "le_s" (i32.const 2147483647) (i32.const -2147483648)) (i32.const 0))
  (assert_return (invoke "le_u" (i32.const 0) (i32.const 0)) (i32.const 1))
  (assert_return (invoke "le_u" (i32.const 1) (i32.const 1)) (i32.const 1))
  (assert_return (invoke "le_u" (i32.const -1) (i32.const 1)) (i32.const 0))
  (assert_return (invoke "le_u" (i32.const -2147483648) (i32.const -2147483648)) (i32.const 1))
  (assert_return (invoke "le_u" (i32.const 2147483647) (i32.const 2147483647)) (i32.const 1))
  (assert_return (invoke "le_u" (i32.const -1) (i32.const -1)) (i32.const 1))
  (assert_return (invoke "le_u" (i32.const 1) (i32.const 0)) (i32.const 0))
  (assert_return (invoke "le_u" (i32.const 0) (i32.const 1)) (i32.const 1))
  (assert_return (invoke "le_u" (i32.const -2147483648) (i32.const 0)) (i32.const 0))
  (assert_return (invoke "le_u" (i32.const 0) (i32.const -2147483648)) (i32.const 1))
  (assert_return (invoke "le_u" (i32.const -2147483648) (i32.const -1)) (i32.const 1))
  (assert_return (invoke "le_u" (i32.const -1) (i32.const -2147483648)) (i32.const 0))
  (assert_return (invoke "le_u" (i32.const -2147483648) (i32.const 2147483647)) (i32.const 0))
  (assert_return (invoke "le_u" (i32.const 2147483647) (i32.const -2147483648)) (i32.const 1))
  (assert_return (invoke "gt_s" (i32.const 0) (i32.const 0)) (i32.const 0))
  (assert_return (invoke "gt_s" (i32.const 1) (i32.const 1)) (i32.const 0))
  (assert_return (invoke "gt_s" (i32.const -1) (i32.const 1)) (i32.const 0))
  (assert_return (invoke "gt_s" (i32.const -2147483648) (i32.const -2147483648)) (i32.const 0))
  (assert_return (invoke "gt_s" (i32.const 2147483647) (i32.const 2147483647)) (i32.const 0))
  (assert_return (invoke "gt_s" (i32.const -1) (i32.const -1)) (i32.const 0))
  (assert_return (invoke "gt_s" (i32.const 1) (i32.const 0)) (i32.const 1))
  (assert_return (invoke "gt_s" (i32.const 0) (i32.const 1)) (i32.const 0))
  (assert_return (invoke "gt_s" (i32.const -2147483648) (i32.const 0)) (i32.const 0))
  (assert_return (invoke "gt_s" (i32.const 0) (i32.const -2147483648)) (i32.const 1))
  (assert_return (invoke "gt_s" (i32.const -2147483648) (i32.const -1)) (i32.const 0))
  (assert_return (invoke "gt_s" (i32.const -1) (i32.const -2147483648)) (i32.const 1))
  (assert_return (invoke "gt_s" (i32.const -2147483648) (i32.const 2147483647)) (i32.const 0))
  (assert_return (invoke "gt_s" (i32.const 2147483647) (i32.const -2147483648)) (i32.const 1))
  (assert_return (invoke "gt_u" (i32.const 0) (i32.const 0)) (i32.const 0))
  (assert_return (invoke "gt_u" (i32.const 1) (i32.const 1)) (i32.const 0))
  (assert_return (invoke "gt_u" (i32.const -1) (i32.const 1)) (i32.const 1))
  (assert_return (invoke "gt_u" (i32.const -2147483648) (i32.const -2147483648)) (i32.const 0))
  (assert_return (invoke "gt_u" (i32.const 2147483647) (i32.const 2147483647)) (i32.const 0))
  (assert_return (invoke "gt_u" (i32.const -1) (i32.const -1)) (i32.const 0))
  (assert_return (invoke "gt_u" (i32.const 1) (i32.const 0)) (i32.const 1))
  (assert_return (invoke "gt_u" (i32.const 0) (i32.const 1)) (i32.const 0))
  (assert_return (invoke "gt_u" (i32.const -2147483648) (i32.const 0)) (i32.const 1))
  (assert_return (invoke "gt_u" (i32.const 0) (i32.const -2147483648)) (i32.const 0))
  (assert_return (invoke "gt_u" (i32.const -2147483648) (i32.const -1)) (i32.const 0))
  (assert_return (invoke "gt_u" (i32.const -1) (i32.const -2147483648)) (i32.const 1))
  (assert_return (invoke "gt_u" (i32.const -2147483648) (i32.const 2147483647)) (i32.const 1))
  (assert_return (invoke "gt_u" (i32.const 2147483647) (i32.const -2147483648)) (i32.const 0))
  (assert_return (invoke "ge_s" (i32.const 0) (i32.const 0)) (i32.const 1))
  (assert_return (invoke "ge_s" (i32.const 1) (i32.const 1)) (i32.const 1))
  (assert_return (invoke "ge_s" (i32.const -1) (i32.const 1)) (i32.const 0))
  (assert_return (invoke "ge_s" (i32.const -2147483648) (i32.const -2147483648)) (i32.const 1))
  (assert_return (invoke "ge_s" (i32.const 2147483647) (i32.const 2147483647)) (i32.const 1))
  (assert_return (invoke "ge_s" (i32.const -1) (i32.const -1)) (i32.const 1))
  (assert_return (invoke "ge_s" (i32.const 1) (i32.const 0)) (i32.const 1))
  (assert_return (invoke "ge_s" (i32.const 0) (i32.const 1)) (i32.const 0))
  (assert_return (invoke "ge_s" (i32.const -2147483648) (i32.const 0)) (i32.const 0))
  (assert_return (invoke "ge_s" (i32.const 0) (i32.const -2147483648)) (i32.const 1))
  (assert_return (invoke "ge_s" (i32.const -2147483648) (i32.const -1)) (i32.const 0))
  (assert_return (invoke "ge_s" (i32.const -1) (i32.const -2147483648)) (i32.const 1))
  (assert_return (invoke "ge_s" (i32.const -2147483648) (i32.const 2147483647)) (i32.const 0))
  (assert_return (invoke "ge_s" (i32.const 2147483647) (i32.const -2147483648)) (i32.const 1))
  (assert_return (invoke "ge_u" (i32.const 0) (i32.const 0)) (i32.const 1))
  (assert_return (invoke "ge_u" (i32.const 1) (i32.const 1)) (i32.const 1))
  (assert_return (invoke "ge_u" (i32.const -1) (i32.const 1)) (i32.const 1))
  (assert_return (invoke "ge_u" (i32.const -2147483648) (i32.const -2147483648)) (i32.const 1))
  (assert_return (invoke "ge_u" (i32.const 2147483647) (i32.const 2147483647)) (i32.const 1))
  (assert_return (invoke "ge_u" (i32.const -1) (i32.const -1)) (i32.const 1))
  (assert_return (invoke "ge_u" (i32.const 1) (i32.const 0)) (i32.const 1))
  (assert_return (invoke "ge_u" (i32.const 0) (i32.const 1)) (i32.const 0))
  (assert_return (invoke "ge_u" (i32.const -2147483648) (i32.const 0)) (i32.const 1))
  (assert_return (invoke "ge_u" (i32.const 0) (i32.const -2147483648)) (i32.const 0))
  (assert_return (invoke "ge_u" (i32.const -2147483648) (i32.const -1)) (i32.const 0))
  (assert_return (invoke "ge_u" (i32.const -1) (i32.const -2147483648)) (i32.const 1))
  (assert_return (invoke "ge_u" (i32.const -2147483648) (i32.const 2147483647)) (i32.const 1))
  (assert_return (invoke "ge_u" (i32.const 2147483647) (i32.const -2147483648)) (i32.const 0))
  (assert_invalid
    (module
      (func $type-unary-operand-empty
        i32.eqz
        drop
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func $type-unary-operand-empty-in-block
        i32.const 0
        (block
          i32.eqz
          drop)
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func $type-unary-operand-empty-in-loop
        i32.const 0
        (loop
          i32.eqz
          drop)
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func $type-unary-operand-empty-in-if
        i32.const 0
        i32.const 0
        (if
          (then
            i32.eqz
            drop
          )
        )
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func $type-unary-operand-empty-in-else
        i32.const 0
        i32.const 0
        (if (result i32)
          (then
            i32.const 0
          )
          (else
            i32.eqz
          )
        )
        drop
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func $type-unary-operand-empty-in-br
        i32.const 0
        (block
          i32.eqz
          br 0
          drop)
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func $type-unary-operand-empty-in-br_if
        i32.const 0
        (block
          i32.eqz
          i32.const 1
          br_if 0
          drop)
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func $type-unary-operand-empty-in-br_table
        i32.const 0
        (block
          i32.eqz
          br_table  0
          drop)
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func $type-unary-operand-empty-in-return
        i32.eqz
        return
        drop
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func $type-unary-operand-empty-in-select
        i32.eqz
        i32.const 1
        i32.const 2
        select
        drop
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func $type-unary-operand-empty-in-call
        i32.eqz
        call 1
        drop
      )
      (func (param i32) (result i32)
        local.get 0
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func $f (param i32) (result i32)
        local.get 0
      )
      (type $sig (func (param i32) (result i32)))
      (table 1 1 (ref null func))
      (elem (offset i32.const 0) (ref null func) (item ref.func $f))
      (func $type-unary-operand-empty-in-call_indirect
        (block (result i32)
          i32.eqz
          i32.const 0
          call_indirect 0 (type $sig)
          drop)
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func $type-unary-operand-empty-in-local.set (local i32)
        i32.eqz
        local.set 0
        local.get 0
        drop
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func $type-unary-operand-empty-in-local.tee (local i32)
        i32.eqz
        local.tee 0
        drop
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (global $x (mut i32) i32.const 0)
      (func $type-unary-operand-empty-in-global.set
        i32.eqz
        global.set $x
        global.get $x
        drop
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (memory 0)
      (func $type-unary-operand-empty-in-memory.grow
        i32.eqz
        memory.grow 0
        drop
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (memory 0)
      (func $type-unary-operand-empty-in-load
        i32.eqz
        i32.load align=1
        drop
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (memory 1)
      (func $type-unary-operand-empty-in-store
        i32.eqz
        i32.const 1
        i32.store align=1
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func $type-binary-1st-operand-empty
        i32.add
        drop
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func $type-binary-2nd-operand-empty
        i32.const 0
        i32.add
        drop
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func $type-binary-1st-operand-empty-in-block
        i32.const 0
        i32.const 0
        (block
          i32.add
          drop)
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func $type-binary-2nd-operand-empty-in-block
        i32.const 0
        (block
          i32.const 0
          i32.add
          drop)
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func $type-binary-1st-operand-empty-in-loop
        i32.const 0
        i32.const 0
        (loop
          i32.add
          drop)
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func $type-binary-2nd-operand-empty-in-loop
        i32.const 0
        (loop
          i32.const 0
          i32.add
          drop)
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func $type-binary-1st-operand-empty-in-if
        i32.const 0
        i32.const 0
        i32.const 0
        i32.add
        (if
          (then
            drop
          )
        )
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func $type-binary-2nd-operand-empty-in-if
        i32.const 0
        i32.const 0
        i32.const 0
        (if
          (then
            i32.add
          )
          (else
            drop
          )
        )
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func $type-binary-1st-operand-empty-in-else
        i32.const 0
        i32.const 0
        i32.const 0
        (if (result i32)
          (then
            i32.const 0
          )
          (else
            i32.add
            i32.const 0
          )
        )
        drop
        drop
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func $type-binary-2nd-operand-empty-in-else
        i32.const 0
        i32.const 0
        (if (result i32)
          (then
            i32.const 0
          )
          (else
            i32.add
          )
        )
        drop
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func $type-binary-1st-operand-empty-in-br
        i32.const 0
        i32.const 0
        (block
          i32.add
          br 0
          drop)
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func $type-binary-2nd-operand-empty-in-br
        i32.const 0
        (block
          i32.const 0
          i32.add
          br 0
          drop)
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func $type-binary-1st-operand-empty-in-br_if
        i32.const 0
        i32.const 0
        (block
          i32.add
          i32.const 1
          br_if 0
          drop)
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func $type-binary-2nd-operand-empty-in-br_if
        i32.const 0
        (block
          i32.const 0
          i32.add
          i32.const 1
          br_if 0
          drop)
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func $type-binary-1st-operand-empty-in-br_table
        i32.const 0
        i32.const 0
        (block
          i32.add
          br_table  0
          drop)
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func $type-binary-2nd-operand-empty-in-br_table
        i32.const 0
        (block
          i32.const 0
          i32.add
          br_table  0
          drop)
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func $type-binary-1st-operand-empty-in-return
        i32.add
        return
        drop
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func $type-binary-2nd-operand-empty-in-return
        i32.const 0
        i32.add
        return
        drop
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func $type-binary-1st-operand-empty-in-select
        i32.add
        i32.const 1
        i32.const 2
        select
        drop
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func $type-binary-2nd-operand-empty-in-select
        i32.const 0
        i32.add
        i32.const 1
        i32.const 2
        select
        drop
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func $type-binary-1st-operand-empty-in-call
        i32.add
        call 1
        drop
      )
      (func (param i32) (param i32) (result i32)
        local.get 0
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func $type-binary-2nd-operand-empty-in-call
        i32.const 0
        i32.add
        call 1
        drop
      )
      (func (param i32) (param i32) (result i32)
        local.get 0
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func $f (param i32) (result i32)
        local.get 0
      )
      (type $sig (func (param i32) (result i32)))
      (table 1 1 (ref null func))
      (elem (offset i32.const 0) (ref null func) (item ref.func $f))
      (func $type-binary-1st-operand-empty-in-call_indirect
        (block (result i32)
          i32.add
          i32.const 0
          call_indirect 0 (type $sig)
          drop)
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func $f (param i32) (result i32)
        local.get 0
      )
      (type $sig (func (param i32) (result i32)))
      (table 1 1 (ref null func))
      (elem (offset i32.const 0) (ref null func) (item ref.func $f))
      (func $type-binary-2nd-operand-empty-in-call_indirect
        (block (result i32)
          i32.const 0
          i32.add
          i32.const 0
          call_indirect 0 (type $sig)
          drop)
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func $type-binary-1st-operand-empty-in-local.set (local i32)
        i32.add
        local.set 0
        local.get 0
        drop
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func $type-binary-2nd-operand-empty-in-local.set (local i32)
        i32.const 0
        i32.add
        local.set 0
        local.get 0
        drop
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func $type-binary-1st-operand-empty-in-local.tee (local i32)
        i32.add
        local.tee 0
        drop
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func $type-binary-2nd-operand-empty-in-local.tee (local i32)
        i32.const 0
        i32.add
        local.tee 0
        drop
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (global $x (mut i32) i32.const 0)
      (func $type-binary-1st-operand-empty-in-global.set
        i32.add
        global.set $x
        global.get $x
        drop
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (global $x (mut i32) i32.const 0)
      (func $type-binary-2nd-operand-empty-in-global.set
        i32.const 0
        i32.add
        global.set $x
        global.get $x
        drop
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (memory 0)
      (func $type-binary-1st-operand-empty-in-memory.grow
        i32.add
        memory.grow 0
        drop
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (memory 0)
      (func $type-binary-2nd-operand-empty-in-memory.grow
        i32.const 0
        i32.add
        memory.grow 0
        drop
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (memory 0)
      (func $type-binary-1st-operand-empty-in-load
        i32.add
        i32.load align=1
        drop
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (memory 0)
      (func $type-binary-2nd-operand-empty-in-load
        i32.const 0
        i32.add
        i32.load align=1
        drop
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (memory 1)
      (func $type-binary-1st-operand-empty-in-store
        i32.add
        i32.const 1
        i32.store align=1
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (memory 1)
      (func $type-binary-2nd-operand-empty-in-store
        i32.const 1
        i32.add
        i32.const 0
        i32.store align=1
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func (result i32)
        i64.const 0
        f32.const 0
        i32.add
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func (result i32)
        i64.const 0
        f32.const 0
        i32.and
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func (result i32)
        i64.const 0
        f32.const 0
        i32.div_s
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func (result i32)
        i64.const 0
        f32.const 0
        i32.div_u
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func (result i32)
        i64.const 0
        f32.const 0
        i32.mul
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func (result i32)
        i64.const 0
        f32.const 0
        i32.or
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func (result i32)
        i64.const 0
        f32.const 0
        i32.rem_s
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func (result i32)
        i64.const 0
        f32.const 0
        i32.rem_u
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func (result i32)
        i64.const 0
        f32.const 0
        i32.rotl
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func (result i32)
        i64.const 0
        f32.const 0
        i32.rotr
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func (result i32)
        i64.const 0
        f32.const 0
        i32.shl
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func (result i32)
        i64.const 0
        f32.const 0
        i32.shr_s
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func (result i32)
        i64.const 0
        f32.const 0
        i32.shr_u
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func (result i32)
        i64.const 0
        f32.const 0
        i32.sub
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func (result i32)
        i64.const 0
        f32.const 0
        i32.xor
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func (result i32)
        i64.const 0
        i32.eqz
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func (result i32)
        i64.const 0
        i32.clz
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func (result i32)
        i64.const 0
        i32.ctz
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func (result i32)
        i64.const 0
        i32.popcnt
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func (result i32)
        i64.const 0
        f32.const 0
        i32.eq
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func (result i32)
        i64.const 0
        f32.const 0
        i32.ge_s
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func (result i32)
        i64.const 0
        f32.const 0
        i32.ge_u
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func (result i32)
        i64.const 0
        f32.const 0
        i32.gt_s
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func (result i32)
        i64.const 0
        f32.const 0
        i32.gt_u
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func (result i32)
        i64.const 0
        f32.const 0
        i32.le_s
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func (result i32)
        i64.const 0
        f32.const 0
        i32.le_u
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func (result i32)
        i64.const 0
        f32.const 0
        i32.lt_s
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func (result i32)
        i64.const 0
        f32.const 0
        i32.lt_u
      )
    )
    "type mismatch"
  )
  (assert_invalid
    (module
      (func (result i32)
        i64.const 0
        f32.const 0
        i32.ne
      )
    )
    "type mismatch"
  )
  (assert_malformed_quote
    "(func (result i32) (i32.const nan:arithmetic))"
    "unexpected token"
  )
  (assert_malformed_quote
    "(func (result i32) (i32.const nan:canonical))"
    "unexpected token"
  )
