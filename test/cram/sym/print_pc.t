  $ owi sym --invoke-with-symbols --entry-point=f -w1 ./print_pc.wat -vv 2>&1 | grep -v "Completed"
  owi: [INFO] parsing      ...
  owi: [INFO] checking     ...
  owi: [INFO] checking     ...
  owi: [DEBUG] grouping     ...
  owi: [DEBUG] {id: 
                 typ: 
                 decl_types: (func (result i32))
                 (func (param $x i32))
                 type_checks: 
                 global: 
                 table: 
                 mem: 
                 func: Imported ({
                   modul: "owi"
                   name: "i32_symbol"
                   assigned_name:  $i32_symbol
                   typ:  (result i32)})
                 Local ((func $f (param $x i32)
                   local.get $x
                   i32.const 1
                   i32.lt_u
                   (if
                     (then
                       unreachable
                     )
                   )
                 ))
                 elem: 
                 data: 
                 start: 
                 }
  owi: [DEBUG] assigning    ...
  owi: [DEBUG] Types: (func (result i32))
               (func (param $x i32))
               Types names: 
               Global names: 
               Table names: 
               Mem names: 
               Func names: ("f", 1) ; ("i32_symbol", 0)
               Elem names: 
               Data names: 
               Tag names: 
               
  owi: [DEBUG] rewriting    ...
  owi: [INFO] typechecking ...
  owi: [DEBUG] stack             : []
  owi: [DEBUG] typechecking instr: local.get 0
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: i32.const 1
  owi: [DEBUG] stack             : [i32 i32]
  owi: [DEBUG] typechecking instr: i32.lt_u
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: if
  owi: [DEBUG] stack             : []
  owi: [DEBUG] typechecking instr: unreachable
  owi: [DEBUG] linking extern module: wasi_snapshot_preview1
  owi: [DEBUG] linking extern module: owi
  owi: [INFO] linking      ...
  owi: [DEBUG] env is: functions: {0 -> Extern <code>; 1 -> Extern <code>;
                                   2 -> Extern <code>; 3 -> Extern <code>;
                                   4 -> Extern <code>; 5 -> Extern <code>;
                                   6 -> Extern <code>; 7 -> Extern <code>;
                                   8 -> Extern <code>; 9 -> Extern <code>;
                                   10 -> Extern <code>; 11 -> Extern <code>;
                                   12 -> Extern <code>; 13 -> Extern <code>;
                                   14 -> Extern <code>; 15 -> Extern <code>;
                                   16 -> Extern <code>; 17 -> Extern <code>;
                                   18 -> Extern <code>; 19 -> Extern <code>;
                                   20 -> Extern <code>; 21 -> Extern <code>;
                                   22 -> Extern <code>; 23 -> Extern <code>;
                                   24 -> Extern <code>; 25 -> Extern <code>;
                                   26 -> Extern <code>; 27 -> Extern <code>;
                                   28 -> Extern <code>; 29 -> Extern <code>;
                                   30 -> Extern <code>; 31 -> Extern <code>;
                                   32 -> Extern <code>; 33 -> Extern <code>;
                                   34 -> Extern <code>; 35 -> Extern <code>;
                                   36 -> Extern <code>; 37 -> Extern <code>;
                                   38 -> Extern <code>; 44 -> Wasm <code>;
                                   45 -> Wasm <code>}
                       globals: {}
                       memories: {}
                       tables: {}
                       datas: {}
                       elems: {}
                       initialization_codes: {2 -> call 45}
                       exported_functions: {0 -> {"args_get" -> 0;
                                                  "args_sizes_get" -> 1;
                                                  "clock_time_get" -> 4;
                                                  "environ_get" -> 2;
                                                  "environ_sizes_get" -> 3;
                                                  "fd_close" -> 5;
                                                  "fd_fdstat_get" -> 6;
                                                  "fd_fdstat_set_flags" -> 7;
                                                  "fd_filestat_get" -> 8;
                                                  "fd_filestat_set_size" -> 9;
                                                  "fd_prestat_dir_name" -> 11;
                                                  "fd_prestat_get" -> 10;
                                                  "fd_read" -> 12;
                                                  "fd_seek" -> 13;
                                                  "fd_write" -> 14;
                                                  "path_create_directory" -> 15;
                                                  "path_filestat_get" -> 16;
                                                  "path_open" -> 17;
                                                  "poll_oneoff" -> 18;
                                                  "proc_exit" -> 19;
                                                  "random_get" -> 20};
                                            1 -> {"abort" -> 37;
                                                  "assert" -> 29;
                                                  "assume" -> 28;
                                                  "close_scope" -> 36;
                                                  "cov_label_is_covered" -> 33;
                                                  "cov_label_set" -> 32;
                                                  "exit" -> 38;
                                                  "f32_symbol" -> 23;
                                                  "f64_symbol" -> 24;
                                                  "i32_symbol" -> 21;
                                                  "i64_symbol" -> 22;
                                                  "in_replay_mode" -> 30;
                                                  "invisible_bool_symbol" -> 26;
                                                  "open_scope_null_terminated" -> 34;
                                                  "open_scope_of_length" -> 35;
                                                  "print_char" -> 31;
                                                  "range_symbol" -> 27;
                                                  "v128_symbol" -> 25};
                                            2 -> {"f" -> 44}}
                       exported_globals: {}
                       exported_memories: {}
                       exported_tables: {}
                       last_module: 2
                       registered_modules: {"owi" -> 1;
                                            "wasi_snapshot_preview1" -> 0}
                       types: <TODO>
                       type_groups: <TODO>
  owi: [INFO] interpreting ...
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : call 45 (executed 0 times)
  owi: [INFO] calling func  : func anonymous
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : call 21 (executed 0 times)
  owi: [INFO] stack         : [ symbol_0 ]
  owi: [INFO] running instr : call 44 (executed 0 times)
  owi: [INFO] calling func  : func f
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : local.get 0 (executed 0 times)
  owi: [INFO] stack         : [ symbol_0 ]
  owi: [INFO] running instr : i32.const 1 (executed 0 times)
  owi: [DEBUG] UUID IS: 1
  owi: [INFO] stack         : [ 1 ; symbol_0 ]
  owi: [INFO] running instr : i32.lt_u (executed 0 times)
  owi: [DEBUG] UUID IS: 2
  owi: [INFO] stack         : [ (i32.of_bool (i32.lt_u symbol_0 1)) ]
  owi: [INFO] running instr : if (executed 0 times)
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : unreachable (executed 0 times)
  owi: [DEBUG] path condition smt query:
                (let-const symbol_0 i32)
                (assert (i32.lt_u symbol_0 (i32 0x1)))
                (check-sat)
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 i32 0
  }
  owi: [ERROR] Reached problem!
