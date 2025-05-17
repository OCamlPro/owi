  $ owi run --timeout 0.042 loop.wat
  owi: [ERROR] timeout
  [26]

  $ owi run --timeout-instr 2048 loop.wat
  owi: [ERROR] timeout
  [26]

  $ owi run --timeout-instr 10 fibo.wat
  owi: [ERROR] timeout
  [26]

  $ owi run --timeout-instr 100 fibo.wat
