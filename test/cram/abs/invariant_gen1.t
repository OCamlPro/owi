  $ owi sym --generate-abstract-invariant ./invariant_gen1.wat
  owi: [ERROR] a worker ended with exception File "src/path_condition.ml", line 89, characters 6-12: Assertion failed, backtrace is: 
               Raised at Symex__Path_condition.add_one_constraint in file "src/path_condition.ml", line 89, characters 6-18
  Called from Stdlib__Set.Make.fold in file "set.ml", line 384, characters 34-55
  Called from Symex__Path_condition.add_checked_sat_condition in file "src/path_condition.ml" (inlined), line 116, characters 2-62
  Called from Owi__Thread.add_already_checked_condition_to_pc in file "src/symbolic/thread.ml", line 77, characters 11-64
  Called from Symex__Monad.map_state.(fun) in file "src/monad.ml", line 47, characters 48-55
  Called from Symex__Monad.bind.(fun) in file "src/monad.ml", line 33, characters 56-65
  Called from Symex__Monad.bind.(fun) in file "src/monad.ml", line 33, characters 56-65
  Called from Symex__Monad.bind.(fun) in file "src/monad.ml", line 33, characters 56-65
  Called from Symex__Monad.map_schedulable.(fun) in file "src/monad.ml", line 24, characters 67-76
  Called from Owi__Symbolic_driver.run.run_worker.(fun) in file "src/symbolic/symbolic_driver.ml", line 121, characters 50-56
  Called from Stdlib__Fun.protect in file "fun.ml", line 34, characters 8-15
  Re-raised at Stdlib__Fun.protect in file "fun.ml", line 39, characters 6-52
  Called from Synchronizer.work_while in file "src/synchronizer.ml", lines 72-77, characters 4-31
  Called from Owi__Symbolic_driver.run.run_worker.(fun) in file "src/symbolic/symbolic_driver.ml", lines 120-122, characters 12-19
  
  All OK!
