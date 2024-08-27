  $ owi rac plus.wat
  $ owi sym plus.instrumented.wat
  All OK
  $ owi sym plus.instrumented.wasm
  All OK
  $ owi wasm2wat plus.instrumented.wasm > plus.instrumented2.wat
  $ owi sym plus.instrumented2.wat
  All OK
