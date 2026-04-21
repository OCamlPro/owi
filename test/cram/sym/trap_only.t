  $ owi sym trap_only_trap.wat --fail-on-trap-only --deterministic-result-order -w1
  owi: [ERROR] Trap: integer divide by zero
  model {
    symbol symbol_0 i32 0
  }
  
  owi: [ERROR] Reached problem!
  [13]
  $ owi sym trap_only_no_failure.wat --fail-on-trap-only --deterministic-result-order -w1
  All OK!
