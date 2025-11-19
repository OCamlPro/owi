  $ owi sym div_i32.wat --no-value --deterministic-result-order -w1 --bench 2>&1 | sed 's/\(solver\|interpreter\|parsing\|execution\|typechecking\|validation\) \(time\).*/\1 \2/' | grep -v "(memory" | grep -v "(max memory" | grep -v "(num allocs"
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
              path count: 3
              solver stats : 
              ((added eqs 11)
               (bv bit2core 160)
               (bv dynamic diseqs 4)
               (bv->core eq 1)
               (cache hits 0)
               (cache misses 4)
               (decisions 96)
               (del clause 9)
               (final checks 6)
               (mk bool var 277)
               (mk clause 14)
               (num checks 6)
               (propagations 11)
               (rlimit count 2003))
  owi: [ERROR] Reached 2 problems!
