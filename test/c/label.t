  $ owi c label.c -O0 -w1 --no-stop-at-failure --deterministic
  Assert failure: false
  model {
    symbol symbol_0 i32 50
    label 1 label_1
  }
  Assert failure: false
  model {
    symbol symbol_0 i32 49
    label 2 label_2
  }
  Reached 2 problems!
  [13]
