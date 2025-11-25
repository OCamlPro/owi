  $ owi sym div_i32.wat --no-value --deterministic-result-order -w1 --bench 2>&1 | sed 's/\(solver\|interpreter\|parsing\|execution\|typechecking\|validation\) \(time\).*/\1 \2/' | sed 's/\(stat \("[^"]*"\|\w*\)\).*/\1/' | grep -v "(memory" | grep -v "(max memory" | grep -v "(num allocs"
  owi: [INFO] parsing time
  owi: [INFO] typechecking time
  owi: [INFO] validation time
  owi: [ERROR] Trap: integer overflow
  model {
    symbol symbol_0 i32
    symbol symbol_1 i32
  }

  owi: [ERROR] Trap: integer divide by zero
  model {
    symbol symbol_0 i32
    symbol symbol_1 i32
  }

  owi: [INFO] Benchmarks:
              execution time
              solver time
              interpreter time
              Solver stats:
              ((added eqs 13)
               (bv bit2core 192)
               (bv dynamic diseqs 4)
               (bv->core eq 1)
               (decisions 96)
               (del clause 10)
               (final checks 6)
               (mk bool var 309)
               (mk clause 15)
               (num checks 6)
               (propagations 12)
               (rlimit count 2087))
  owi: [ERROR] Reached 2 problems!
