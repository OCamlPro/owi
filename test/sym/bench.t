  $ owi sym div_i32.wat --no-value --deterministic-result-order -w1 --bench 2>&1 | sed 's/\(solver\|interpreter\|parsing\|execution\|typechecking\|validation\) \(time\).*/\1 \2/' | sed 's/\(stat \("[^"]*"\|\w*\)\).*/\1/'
  owi: [INFO] parsing time
  owi: [INFO] typechecking time
  owi: [INFO] validation time
  owi: [INFO] execution time
  owi: [ERROR] Trap: integer overflow
  model {
    symbol symbol_0 i32
    symbol symbol_1 i32
  }
  solver_stats {
    stat "rlimit count"
    stat propagations
    stat "num checks"
    stat "num allocs"
    stat "mk clause"
    stat "mk bool var"
    stat memory
    stat "max memory"
    stat "final checks"
    stat "del clause"
    stat decisions
    stat "bv dynamic diseqs"
    stat "bv bit2core"
    stat "added eqs"
  }
  
  owi: [ERROR] Trap: integer divide by zero
  model {
    symbol symbol_0 i32
    symbol symbol_1 i32
  }
  solver_stats {
    stat "rlimit count"
    stat propagations
    stat "num checks"
    stat "num allocs"
    stat "mk clause"
    stat "mk bool var"
    stat memory
    stat "max memory"
    stat "final checks"
    stat "del clause"
    stat "bv bit2core"
    stat "added eqs"
  }
  
  owi: [INFO] Benchmarks:
              solver time
              interpreter time
              
  owi: [ERROR] Reached 2 problems!
