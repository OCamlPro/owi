test data special chars round-trip:
  $ owi fmt data_special_chars.wat > ./owi_test_output.wat
  $ owi fmt ./owi_test_output.wat
  (module
    (memory 1)
    (data (memory 0) (offset i32.const 0) "hello\n\t\u{0d}\"\'\\world")
  )
  $ rm ./owi_test_output.wat
