  $ owi sym div_i32.wat --no-value --deterministic-result-order -w1 --bench 2>&1 | sed 's/\(solver\|interpreter\|parsing\|execution\|typechecking\) \(time\).*/\1 \2/'
  owi: [INFO] parsing time
  owi: [INFO] typechecking time
  owi: [INFO] execution time
  owi: [ERROR] Trap: integer overflow
  model {
    symbol symbol_0 i32
    symbol symbol_1 i32
  }
  solver_stats {
    stat "rlimit count" 1261
    stat propagations 10
    stat "num checks" 5
    stat "num allocs" 8627
    stat "mk clause" 10
    stat "mk bool var" 240
    stat memory 33.53
    stat "max memory" 33.53
    stat "final checks" 5
    stat "del clause" 7
    stat decisions 31
    stat "bv dynamic diseqs" 1
    stat "bv bit2core" 192
    stat "added eqs" 12
  }
  
  owi: [ERROR] Trap: integer divide by zero
  model {
    symbol symbol_0 i32
    symbol symbol_1 i32
  }
  solver_stats {
    stat "rlimit count" 275
    stat propagations 2
    stat "num checks" 2
    stat "num allocs" 8627
    stat "mk clause" 2
    stat "mk bool var" 68
    stat memory 33.53
    stat "max memory" 33.53
    stat "final checks" 2
    stat "del clause" 1
    stat "bv bit2core" 64
    stat "added eqs" 4
  }
  
  owi: [INFO] Benchmarks:
              solver time
              interpreter time
              
  owi: [ERROR] Reached 2 problems!
