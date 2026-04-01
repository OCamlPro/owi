print symbolic:
  $ owi fmt m.wat
  (module
    (func $f (param $x i32) (param $y i32) (result i32)
      local.get $x
      local.get $y
      i32.add
    )
    (func $start
      i32.const 22
      i32.const 20
      call $f
      drop
    )
    (start $start)
  )
print simplified:
  $ dune exec -- ./print_simplified.exe m.wat
  (module
    (type (func (param $x i32) (param $y i32) (result i32)))
    (type (func))
    (func $f (param $x i32) (param $y i32) (result i32)
      local.get 0
      local.get 1
      i32.add
    )
    (func $start
      i32.const 22
      i32.const 20
      call 0
      drop
    )
    (start 1)
  )
print data with special chars:
  $ owi fmt data_special_chars.wat
  (module
    (memory 1)
    (data (memory 0) (offset i32.const 0) "hello\n\t\u{0d}\"\'\\world")
  )
print data with raw bytes:
  $ owi fmt data_bytes.wat
  (module
    (memory 1)
    (data (memory 0) (offset i32.const 0) "Hello\u{00}World\u{01}\u{02}\u{03}\u{ff}")
  )
