  $ owi c from_c.c -o a.out.wasm
  All OK!
  $ owi wasm to_wat a.out.wasm > a.out.wat
  $ owi wasm sym a.out.wat --entry-point main
  All OK!
