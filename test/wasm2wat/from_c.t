  $ owi c from_c.c -o a.out.wasm
  All OK!
  $ owi wasm2wat a.out.wasm > a.out.wat
  $ owi sym a.out.wat --entry-point main
  All OK!
