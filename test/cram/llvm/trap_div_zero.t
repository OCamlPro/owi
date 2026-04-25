trap_div_zero:
  $ owi llvm trap_div_zero.ll --entry-point=main --no-value
  owi: [ERROR] Trap: integer divide by zero
  model
  owi: [ERROR] Reached problem!
  [13]

  $ owi llvm trap_div_zero.bc --entry-point=main --no-value
  owi: [ERROR] Trap: integer divide by zero
  model
  owi: [ERROR] Reached problem!
  [13]
