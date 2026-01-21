  $ owi sym div_i32.wat --no-value --deterministic-result-order -w1 --bench 2>&1 | sed 's/: [0-9].*/: X/' | sed 's/[0-9].*)/ X)/'
  owi: [INFO] parsing time : X
  owi: [INFO] typechecking time : X
  owi: [INFO] validation time : X
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
  
  owi: [INFO] whole execution time          : X
  owi: [INFO] solver time                   : X
  owi: [INFO] solver SAT time               : X
  owi: [INFO] solver model time             : X
  owi: [INFO] solver final model time       : X
  owi: [INFO] solver intermediate model time: X
  owi: [INFO] interpreter loop time         : X
  owi: [INFO] path count: X
  owi: [INFO] solver stats: ((added eqs  X)
                             (bv bit X)
                             (bv dynamic diseqs  X)
                             (bv->core eq  X)
                             (cache hits  X)
                             (cache hits ratio  X)
                             (cache misses  X)
                             (decisions  X)
                             (del clause  X)
                             (final checks  X)
                             (max memory  X)
                             (memory  X)
                             (mk bool var  X)
                             (mk clause  X)
                             (num allocs  X)
                             (num checks  X)
                             (propagations  X)
                             (rlimit count  X)
  owi: [ERROR] Reached 2 problems!
Testing with the -q flag:
  $ owi sym div_i32.wat --unsafe --fail-on-assertion-only --no-value -w1 --bench --workspace . -q 2>&1 | sed 's/: [0-9].*/: X/' | sed 's/[0-9].*)/ X)/'
  owi: [INFO] parsing time : X
  owi: [INFO] validation time : X
  owi: [INFO] whole execution time          : X
  owi: [INFO] solver time                   : X
  owi: [INFO] solver SAT time               : X
  owi: [INFO] solver model time             : X
  owi: [INFO] solver final model time       : X
  owi: [INFO] solver intermediate model time: X
  owi: [INFO] interpreter loop time         : X
  owi: [INFO] path count: X
  owi: [INFO] solver stats: ((added eqs  X)
                             (bv dynamic diseqs  X)
                             (bv->core eq  X)
                             (cache hits  X)
                             (cache hits ratio  X)
                             (cache misses  X)
                             (decisions  X)
                             (del clause  X)
                             (final checks  X)
                             (max memory  X)
                             (memory  X)
                             (mk bool var  X)
                             (mk clause  X)
                             (num allocs  X)
                             (num checks  X)
                             (propagations  X)
                             (rlimit count  X)

