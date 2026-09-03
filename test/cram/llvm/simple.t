simple.bc and simple.ll cramtests :
  $ owi llvm sym simple.ll --entry-point=main --no-value
  All OK!

  $ owi llvm sym simple.bc --entry-point=main --no-value
  All OK!
