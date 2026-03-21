test data special chars round-trip:
  $ owi fmt data_special_chars.wat > /tmp/owi_test_output.wat
  $ owi fmt /tmp/owi_test_output.wat
  (module
    (memory 1)
    (data (memory 0) (offset i32.const 0) "hello\n\t\u{0d}\"\'\\world")
  )
  $ rm /tmp/owi_test_output.wat
