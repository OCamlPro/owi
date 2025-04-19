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
    (type (sub final  (func (param $x i32) (param $y i32) (result i32))))
    (type (sub final  (func)))
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
print data:
  $ owi fmt data.wat
  (module
    (memory $m 1)
    (data $d (memory 0) (offset i32.const 0) "hello\n\\n\\\\\'\'\\r\u{0d}\\t\t\\\"\"world!")
  )
