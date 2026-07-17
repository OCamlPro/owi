  $ owi sym --generate-abstract-invariant ./invariant_gen1.wat
  owi: [ERROR] Trap: integer divide by zero
  model {
    symbol symbol_0 i32 4242
  }
  owi: [ERROR] Reached problem!
  [13]
  $ owi abs ./invariant_gen1.wat -vv
  owi: [INFO] parsing      ...
  owi: [INFO] checking     ...
  owi: [DEBUG] grouping     ...
  owi: [DEBUG] {id: 
                 typ: 
                 decl_types: (func (result i32))
                 (func)
                 type_checks: 
                 global: 
                 table: 
                 mem: 
                 func: Imported ({
                   modul: "owi"
                   name: "i32_symbol"
                   assigned_name:  $i32_symbol
                   typ:  (result i32)})
                 Local ((func $start (local $a i32) (local $b i32) (local $c i32) (local $d i32) (local $e i32) (local $i i32) (local $n i32)
                   call $i32_symbol
                   local.set $a
                   local.get $a
                   i32.const 10
                   i32.add
                   local.set $b
                   local.get $b
                   i32.const 20
                   i32.add
                   local.set $c
                   local.get $c
                   local.get $a
                   i32.sub
                   local.set $d
                   local.get $d
                   i32.const 29
                   i32.sub
                   local.set $e
                   i32.const 10000
                   local.set $n
                   i32.const 0
                   local.set $i
                   (block $break
                     (loop $loop
                       i32.const 100
                       local.get $e
                       i32.div_s
                       drop
                       local.get $i
                       i32.const 1
                       i32.add
                       local.tee $i
                       local.get $n
                       i32.lt_s
                       br_if $loop))
                   local.get $a
                   i32.const 4242
                   i32.eq
                   (if
                     (then
                       i32.const 0
                       local.set $e
                     )
                   )
                   i32.const 42
                   local.get $e
                   i32.div_s
                   drop
                 ))
                 elem: 
                 data: 
                 start: $start
                 }
  owi: [DEBUG] assigning    ...
  owi: [DEBUG] Types: (func (result i32))
               (func)
               Types names: 
               Global names: 
               Table names: 
               Mem names: 
               Func names: ("start", 1) ; ("i32_symbol", 0)
               Elem names: 
               Data names: 
               Tag names: 
               
  owi: [DEBUG] rewriting    ...
  owi: [INFO] typechecking ...
  owi: [DEBUG] stack             : []
  owi: [DEBUG] typechecking instr: call 0
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: local.set 0
  owi: [DEBUG] stack             : []
  owi: [DEBUG] typechecking instr: local.get 0
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: i32.const 10
  owi: [DEBUG] stack             : [i32 i32]
  owi: [DEBUG] typechecking instr: i32.add
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: local.set 1
  owi: [DEBUG] stack             : []
  owi: [DEBUG] typechecking instr: local.get 1
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: i32.const 20
  owi: [DEBUG] stack             : [i32 i32]
  owi: [DEBUG] typechecking instr: i32.add
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: local.set 2
  owi: [DEBUG] stack             : []
  owi: [DEBUG] typechecking instr: local.get 2
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: local.get 0
  owi: [DEBUG] stack             : [i32 i32]
  owi: [DEBUG] typechecking instr: i32.sub
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: local.set 3
  owi: [DEBUG] stack             : []
  owi: [DEBUG] typechecking instr: local.get 3
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: i32.const 29
  owi: [DEBUG] stack             : [i32 i32]
  owi: [DEBUG] typechecking instr: i32.sub
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: local.set 4
  owi: [DEBUG] stack             : []
  owi: [DEBUG] typechecking instr: i32.const 10000
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: local.set 6
  owi: [DEBUG] stack             : []
  owi: [DEBUG] typechecking instr: i32.const 0
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: local.set 5
  owi: [DEBUG] stack             : []
  owi: [DEBUG] typechecking instr: block $break
  owi: [DEBUG] stack             : []
  owi: [DEBUG] typechecking instr: loop $loop
  owi: [DEBUG] stack             : []
  owi: [DEBUG] typechecking instr: i32.const 100
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: local.get 4
  owi: [DEBUG] stack             : [i32 i32]
  owi: [DEBUG] typechecking instr: i32.div_s
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: drop
  owi: [DEBUG] stack             : []
  owi: [DEBUG] typechecking instr: local.get 5
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: i32.const 1
  owi: [DEBUG] stack             : [i32 i32]
  owi: [DEBUG] typechecking instr: i32.add
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: local.tee 5
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: local.get 6
  owi: [DEBUG] stack             : [i32 i32]
  owi: [DEBUG] typechecking instr: i32.lt_s
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: br_if 0
  owi: [DEBUG] stack             : []
  owi: [DEBUG] typechecking instr: local.get 0
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: i32.const 4242
  owi: [DEBUG] stack             : [i32 i32]
  owi: [DEBUG] typechecking instr: i32.eq
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: if
  owi: [DEBUG] stack             : []
  owi: [DEBUG] typechecking instr: i32.const 0
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: local.set 4
  owi: [DEBUG] stack             : []
  owi: [DEBUG] typechecking instr: i32.const 42
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: local.get 4
  owi: [DEBUG] stack             : [i32 i32]
  owi: [DEBUG] typechecking instr: i32.div_s
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: drop
  owi: [INFO] linking      ...
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : 
                 locals : 
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : call 1
  owi: [INFO] calling func  : func start
  owi: [DEBUG] call (start): abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : 
                 locals : i32 {0};
                 i32 {0};
                 i32 {0};
                 i32 {0};
                 i32 {0};
                 i32 {0};
                 i32 {0}
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : 
                 locals : i32 {0};
                 i32 {0};
                 i32 {0};
                 i32 {0};
                 i32 {0};
                 i32 {0};
                 i32 {0}
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {0});  (1 -> i32 {0}); 
              (2 -> i32 {0});  (3 -> i32 {0});  (4 -> i32 {0}); 
              (5 -> i32 {0});  (6 -> i32 {0}) ]
  owi: [INFO] running instr : call 0
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <(B:9)> -> [--..--]
                                        }
                 stack  : i32 [--..--]
                 locals : i32 {0};
                 i32 {0};
                 i32 {0};
                 i32 {0};
                 i32 {0};
                 i32 {0};
                 i32 {0}
  owi: [INFO] stack         : [ i32 [--..--] ]
  owi: [INFO] locals        : [ (0 -> i32 {0});  (1 -> i32 {0}); 
              (2 -> i32 {0});  (3 -> i32 {0});  (4 -> i32 {0}); 
              (5 -> i32 {0});  (6 -> i32 {0}) ]
  owi: [INFO] running instr : local.set 0
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <(B:9)> -> [--..--]
                                        }
                 stack  : 
                 locals : i32 [--..--];
                 i32 {0};
                 i32 {0};
                 i32 {0};
                 i32 {0};
                 i32 {0};
                 i32 {0}
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 {0}); 
              (2 -> i32 {0});  (3 -> i32 {0});  (4 -> i32 {0}); 
              (5 -> i32 {0});  (6 -> i32 {0}) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <(B:9)> -> [--..--]
                                        }
                 stack  : i32 [--..--]
                 locals : i32 [--..--];
                 i32 {0};
                 i32 {0};
                 i32 {0};
                 i32 {0};
                 i32 {0};
                 i32 {0}
  owi: [INFO] stack         : [ i32 [--..--] ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 {0}); 
              (2 -> i32 {0});  (3 -> i32 {0});  (4 -> i32 {0}); 
              (5 -> i32 {0});  (6 -> i32 {0}) ]
  owi: [INFO] running instr : i32.const 10
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <(B:9)> -> [--..--]
                                        }
                 stack  : i32 {10} ; i32 [--..--]
                 locals : i32 [--..--];
                 i32 {0};
                 i32 {0};
                 i32 {0};
                 i32 {0};
                 i32 {0};
                 i32 {0}
  owi: [INFO] stack         : [ i32 {10} ; i32 [--..--] ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 {0}); 
              (2 -> i32 {0});  (3 -> i32 {0});  (4 -> i32 {0}); 
              (5 -> i32 {0});  (6 -> i32 {0}) ]
  owi: [INFO] running instr : i32.add
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <(B:9)> -> [--..--]
                                        <(B:11)> -> [--..--]
                                        }
                 stack  : i32 [--..--]
                 locals : i32 [--..--];
                 i32 {0};
                 i32 {0};
                 i32 {0};
                 i32 {0};
                 i32 {0};
                 i32 {0}
  owi: [INFO] stack         : [ i32 [--..--] ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 {0}); 
              (2 -> i32 {0});  (3 -> i32 {0});  (4 -> i32 {0}); 
              (5 -> i32 {0});  (6 -> i32 {0}) ]
  owi: [INFO] running instr : local.set 1
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <(B:9)> -> [--..--]
                                        <(B:11)> -> [--..--]
                                        }
                 stack  : 
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 {0};
                 i32 {0};
                 i32 {0};
                 i32 {0};
                 i32 {0}
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 {0});  (3 -> i32 {0});  (4 -> i32 {0}); 
              (5 -> i32 {0});  (6 -> i32 {0}) ]
  owi: [INFO] running instr : local.get 1
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <(B:9)> -> [--..--]
                                        <(B:11)> -> [--..--]
                                        }
                 stack  : i32 [--..--]
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 {0};
                 i32 {0};
                 i32 {0};
                 i32 {0};
                 i32 {0}
  owi: [INFO] stack         : [ i32 [--..--] ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 {0});  (3 -> i32 {0});  (4 -> i32 {0}); 
              (5 -> i32 {0});  (6 -> i32 {0}) ]
  owi: [INFO] running instr : i32.const 20
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <(B:9)> -> [--..--]
                                        <(B:11)> -> [--..--]
                                        }
                 stack  : i32 {20} ; i32 [--..--]
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 {0};
                 i32 {0};
                 i32 {0};
                 i32 {0};
                 i32 {0}
  owi: [INFO] stack         : [ i32 {20} ; i32 [--..--] ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 {0});  (3 -> i32 {0});  (4 -> i32 {0}); 
              (5 -> i32 {0});  (6 -> i32 {0}) ]
  owi: [INFO] running instr : i32.add
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  }
                 stack  : i32 [--..--]
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 {0};
                 i32 {0};
                 i32 {0};
                 i32 {0};
                 i32 {0}
  owi: [INFO] stack         : [ i32 [--..--] ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 {0});  (3 -> i32 {0});  (4 -> i32 {0}); 
              (5 -> i32 {0});  (6 -> i32 {0}) ]
  owi: [INFO] running instr : local.set 2
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  }
                 stack  : 
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {0};
                 i32 {0};
                 i32 {0};
                 i32 {0}
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {0});  (4 -> i32 {0}); 
              (5 -> i32 {0});  (6 -> i32 {0}) ]
  owi: [INFO] running instr : local.get 2
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  }
                 stack  : i32 [--..--]
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {0};
                 i32 {0};
                 i32 {0};
                 i32 {0}
  owi: [INFO] stack         : [ i32 [--..--] ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {0});  (4 -> i32 {0}); 
              (5 -> i32 {0});  (6 -> i32 {0}) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  }
                 stack  : i32 [--..--] ; i32 [--..--]
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {0};
                 i32 {0};
                 i32 {0};
                 i32 {0}
  owi: [INFO] stack         : [ i32 [--..--] ; i32 [--..--] ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {0});  (4 -> i32 {0}); 
              (5 -> i32 {0});  (6 -> i32 {0}) ]
  owi: [INFO] running instr : i32.sub
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  }
                 stack  : i32 {30}
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {0};
                 i32 {0};
                 i32 {0};
                 i32 {0}
  owi: [INFO] stack         : [ i32 {30} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {0});  (4 -> i32 {0}); 
              (5 -> i32 {0});  (6 -> i32 {0}) ]
  owi: [INFO] running instr : local.set 3
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  }
                 stack  : 
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {0};
                 i32 {0};
                 i32 {0}
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {0}); 
              (5 -> i32 {0});  (6 -> i32 {0}) ]
  owi: [INFO] running instr : local.get 3
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  }
                 stack  : i32 {30}
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {0};
                 i32 {0};
                 i32 {0}
  owi: [INFO] stack         : [ i32 {30} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {0}); 
              (5 -> i32 {0});  (6 -> i32 {0}) ]
  owi: [INFO] running instr : i32.const 29
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  }
                 stack  : i32 {29} ; i32 {30}
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {0};
                 i32 {0};
                 i32 {0}
  owi: [INFO] stack         : [ i32 {29} ; i32 {30} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {0}); 
              (5 -> i32 {0});  (6 -> i32 {0}) ]
  owi: [INFO] running instr : i32.sub
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  }
                 stack  : i32 {1}
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {0};
                 i32 {0};
                 i32 {0}
  owi: [INFO] stack         : [ i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {0}); 
              (5 -> i32 {0});  (6 -> i32 {0}) ]
  owi: [INFO] running instr : local.set 4
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  }
                 stack  : 
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 {0};
                 i32 {0}
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {0});  (6 -> i32 {0}) ]
  owi: [INFO] running instr : i32.const 10000
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  }
                 stack  : i32 {0x2710}
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 {0};
                 i32 {0}
  owi: [INFO] stack         : [ i32 {0x2710} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {0});  (6 -> i32 {0}) ]
  owi: [INFO] running instr : local.set 6
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  }
                 stack  : 
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 {0};
                 i32 {0x2710}
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {0});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : i32.const 0
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  }
                 stack  : i32 {0}
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 {0};
                 i32 {0x2710}
  owi: [INFO] stack         : [ i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {0});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : local.set 5
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  }
                 stack  : 
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 {0};
                 i32 {0x2710}
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {0});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : block $break
  owi: [DEBUG] abstract state : 
                 context: Context{id=1,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  }
                 stack  : 
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 {0};
                 i32 {0x2710}
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {0});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : loop $loop
  owi: [DEBUG] abstract state : 
                 context: Context{id=1,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  }
                 stack  : 
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 {0};
                 i32 {0x2710}
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {0});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : i32.const 100
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  }
                 stack  : i32 {100}
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 {0};
                 i32 {0x2710}
  owi: [INFO] stack         : [ i32 {100} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {0});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : local.get 4
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  }
                 stack  : i32 {1} ; i32 {100}
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 {0};
                 i32 {0x2710}
  owi: [INFO] stack         : [ i32 {1} ; i32 {100} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {0});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : i32.div_s
  owi: [DEBUG] marked 24 as IMPOSSIBLE divide by zero
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  }
                 stack  : i32 {100}
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 {0};
                 i32 {0x2710}
  owi: [INFO] stack         : [ i32 {100} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {0});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : drop
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  }
                 stack  : 
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 {0};
                 i32 {0x2710}
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {0});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : local.get 5
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  }
                 stack  : i32 {0}
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 {0};
                 i32 {0x2710}
  owi: [INFO] stack         : [ i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {0});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : i32.const 1
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  }
                 stack  : i32 {1} ; i32 {0}
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 {0};
                 i32 {0x2710}
  owi: [INFO] stack         : [ i32 {1} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {0});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : i32.add
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  }
                 stack  : i32 {1}
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 {0};
                 i32 {0x2710}
  owi: [INFO] stack         : [ i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {0});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : local.tee 5
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  }
                 stack  : i32 {1}
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 {1};
                 i32 {0x2710}
  owi: [INFO] stack         : [ i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {1});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : local.get 6
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  }
                 stack  : i32 {0x2710} ; i32 {1}
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 {1};
                 i32 {0x2710}
  owi: [INFO] stack         : [ i32 {0x2710} ; i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {1});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : i32.lt_s
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  }
                 stack  : i32 {1}
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 {1};
                 i32 {0x2710}
  owi: [INFO] stack         : [ i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {1});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : br_if 0
  owi: [DEBUG] jt            :  0 -> 
                 context: Context{id=1,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  }
                 stack  : 
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 {1};
                 i32 {0x2710}
  owi: [DEBUG] serializing locals (join) : 
                first : (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
               (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
               (5 -> i32 {0}); 
               (6 -> i32 {0x2710}) 
                second : (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
               (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
               (5 -> i32 {1});  (6 -> i32 {0x2710})
  owi: [DEBUG] serializing stacks (join) : 
                first :  
                second : 
  owi: [DEBUG] serializing locals (widen) : 
                first : (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
               (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
               (5 -> i32 {0}); 
               (6 -> i32 {0x2710}) 
                second : (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
               (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
               (5 -> i32 {0; 1});  (6 -> i32 {0x2710})
  owi: [DEBUG] serializing stacks (widen) : 
                first :  
                second : 
  owi: [DEBUG] abstract state : 
                 context: Context{id=3,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  <(B:26)> -> {0; 1}
                                  }
                 stack  : 
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 {0; 1};
                 i32 {0x2710}
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {0; 1});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : i32.const 100
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=3,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  <(B:26)> -> {0; 1}
                                  }
                 stack  : i32 {100}
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 {0; 1};
                 i32 {0x2710}
  owi: [INFO] stack         : [ i32 {100} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {0; 1});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : local.get 4
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=3,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  <(B:26)> -> {0; 1}
                                  }
                 stack  : i32 {1} ; i32 {100}
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 {0; 1};
                 i32 {0x2710}
  owi: [INFO] stack         : [ i32 {1} ; i32 {100} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {0; 1});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : i32.div_s
  owi: [DEBUG] marked 24 as IMPOSSIBLE divide by zero
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=3,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  <(B:26)> -> {0; 1}
                                  }
                 stack  : i32 {100}
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 {0; 1};
                 i32 {0x2710}
  owi: [INFO] stack         : [ i32 {100} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {0; 1});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : drop
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=3,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  <(B:26)> -> {0; 1}
                                  }
                 stack  : 
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 {0; 1};
                 i32 {0x2710}
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {0; 1});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : local.get 5
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=3,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  <(B:26)> -> {0; 1}
                                  }
                 stack  : i32 {0; 1}
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 {0; 1};
                 i32 {0x2710}
  owi: [INFO] stack         : [ i32 {0; 1} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {0; 1});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : i32.const 1
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=3,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  <(B:26)> -> {0; 1}
                                  }
                 stack  : i32 {1} ; i32 {0; 1}
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 {0; 1};
                 i32 {0x2710}
  owi: [INFO] stack         : [ i32 {1} ; i32 {0; 1} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {0; 1});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : i32.add
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=3,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  <(B:26)> -> {0; 1}
                                  <(B:27)> -> {1; 2}
                                  }
                 stack  : i32 {1; 2}
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 {0; 1};
                 i32 {0x2710}
  owi: [INFO] stack         : [ i32 {1; 2} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {0; 1});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : local.tee 5
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=3,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  <(B:26)> -> {0; 1}
                                  <(B:27)> -> {1; 2}
                                  }
                 stack  : i32 {1; 2}
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 {1; 2};
                 i32 {0x2710}
  owi: [INFO] stack         : [ i32 {1; 2} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {1; 2});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : local.get 6
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=3,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  <(B:26)> -> {0; 1}
                                  <(B:27)> -> {1; 2}
                                  }
                 stack  : i32 {0x2710} ; i32 {1; 2}
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 {1; 2};
                 i32 {0x2710}
  owi: [INFO] stack         : [ i32 {0x2710} ; i32 {1; 2} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {1; 2});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : i32.lt_s
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=3,
                                  <(b:28)> -> {false};
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  <(B:26)> -> {0; 1}
                                  <(B:27)> -> {1; 2}
                                  }
                 stack  : i32 {1}
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 {1; 2};
                 i32 {0x2710}
  owi: [INFO] stack         : [ i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {1; 2});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : br_if 0
  owi: [DEBUG] jt            :  0 -> 
                 context: Context{id=3,
                                  <(b:28)> -> {false};
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  <(B:26)> -> {0; 1}
                                  <(B:27)> -> {1; 2}
                                  }
                 stack  : 
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 {1; 2};
                 i32 {0x2710}
  owi: [DEBUG] serializing locals (join) : 
                first : (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
               (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
               (5 -> i32 {0}); 
               (6 -> i32 {0x2710}) 
                second : (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
               (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
               (5 -> i32 {1; 2});  (6 -> i32 {0x2710})
  owi: [DEBUG] serializing stacks (join) : 
                first :  
                second : 
  owi: [DEBUG] serializing locals (widen) : 
                first : (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
               (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
               (5 -> i32 {0; 1}); 
               (6 -> i32 {0x2710}) 
                second : (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
               (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
               (5 -> i32 {0; 1; 2});  (6 -> i32 {0x2710})
  owi: [DEBUG] serializing stacks (widen) : 
                first :  
                second : 
  owi: [DEBUG] abstract state : 
                 context: Context{id=5,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  <(B:26)> -> [0..0x7FFFFFFF]
                                  }
                 stack  : 
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 [0..0x7FFFFFFF];
                 i32 {0x2710}
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 [0..0x7FFFFFFF]);  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : i32.const 100
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=5,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  <(B:26)> -> [0..0x7FFFFFFF]
                                  }
                 stack  : i32 {100}
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 [0..0x7FFFFFFF];
                 i32 {0x2710}
  owi: [INFO] stack         : [ i32 {100} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 [0..0x7FFFFFFF]);  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : local.get 4
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=5,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  <(B:26)> -> [0..0x7FFFFFFF]
                                  }
                 stack  : i32 {1} ; i32 {100}
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 [0..0x7FFFFFFF];
                 i32 {0x2710}
  owi: [INFO] stack         : [ i32 {1} ; i32 {100} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 [0..0x7FFFFFFF]);  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : i32.div_s
  owi: [DEBUG] marked 24 as IMPOSSIBLE divide by zero
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=5,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  <(B:26)> -> [0..0x7FFFFFFF]
                                  }
                 stack  : i32 {100}
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 [0..0x7FFFFFFF];
                 i32 {0x2710}
  owi: [INFO] stack         : [ i32 {100} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 [0..0x7FFFFFFF]);  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : drop
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=5,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  <(B:26)> -> [0..0x7FFFFFFF]
                                  }
                 stack  : 
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 [0..0x7FFFFFFF];
                 i32 {0x2710}
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 [0..0x7FFFFFFF]);  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : local.get 5
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=5,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  <(B:26)> -> [0..0x7FFFFFFF]
                                  }
                 stack  : i32 [0..0x7FFFFFFF]
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 [0..0x7FFFFFFF];
                 i32 {0x2710}
  owi: [INFO] stack         : [ i32 [0..0x7FFFFFFF] ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 [0..0x7FFFFFFF]);  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : i32.const 1
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=5,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  <(B:26)> -> [0..0x7FFFFFFF]
                                  }
                 stack  : i32 {1} ; i32 [0..0x7FFFFFFF]
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 [0..0x7FFFFFFF];
                 i32 {0x2710}
  owi: [INFO] stack         : [ i32 {1} ; i32 [0..0x7FFFFFFF] ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 [0..0x7FFFFFFF]);  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : i32.add
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=5,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  <(B:26)> -> [0..0x7FFFFFFF]
                                  <(B:27)> -> [1..0x80000000]
                                  }
                 stack  : i32 [1..0x80000000]
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 [0..0x7FFFFFFF];
                 i32 {0x2710}
  owi: [INFO] stack         : [ i32 [1..0x80000000] ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 [0..0x7FFFFFFF]);  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : local.tee 5
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=5,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  <(B:26)> -> [0..0x7FFFFFFF]
                                  <(B:27)> -> [1..0x80000000]
                                  }
                 stack  : i32 [1..0x80000000]
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 [1..0x80000000];
                 i32 {0x2710}
  owi: [INFO] stack         : [ i32 [1..0x80000000] ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 [1..0x80000000]);  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : local.get 6
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=5,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  <(B:26)> -> [0..0x7FFFFFFF]
                                  <(B:27)> -> [1..0x80000000]
                                  }
                 stack  : i32 {0x2710} ; i32 [1..0x80000000]
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 [1..0x80000000];
                 i32 {0x2710}
  owi: [INFO] stack         : [ i32 {0x2710} ; i32 [1..0x80000000] ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 [1..0x80000000]);  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : i32.lt_s
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=5,
                                  <(b:28)> -> {true;false};
                                  <(b:31)> -> {true;false};
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  <(B:26)> -> [0..0x7FFFFFFF]
                                  <(B:27)> -> [1..0x80000000]
                                  <(B:32)> -> {0; 1}
                                  }
                 stack  : i32 {0; 1}
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 [1..0x80000000];
                 i32 {0x2710}
  owi: [INFO] stack         : [ i32 {0; 1} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 [1..0x80000000]);  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : br_if 0
  owi: [DEBUG] jt            :  0 -> 
                 context: Context{id=6,
                                  <(b:28)> -> {false}; <(b:31)> -> {true};
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  <(B:26)> -> [0..0x7FFFFFFF]
                                  <(B:27)> -> {signed: [-0x80000000..9999]; unsigned: [1..0x80000000]}
                                  <(B:32)> -> {0; 1}
                                  }
                 stack  : 
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 {signed: [-0x80000000..9999]; unsigned: [1..0x80000000]};
                 i32 {0x2710}
  owi: [DEBUG] serializing locals (join) : 
                first : (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
               (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
               (5 -> i32 {0}); 
               (6 -> i32 {0x2710}) 
                second : (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
               (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
               (5 -> i32 {signed: [-0x80000000..9999]; unsigned: [1..0x80000000]}); 
               (6 -> i32 {0x2710})
  owi: [DEBUG] serializing stacks (join) : 
                first :  
                second : 
  owi: [DEBUG] serializing locals (widen) : 
                first : (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
               (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
               (5 -> i32 [0..0x7FFFFFFF]); 
               (6 -> i32 {0x2710}) 
                second : (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
               (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
               (5 -> i32 {signed: [-0x80000000..9999]; unsigned: [0..0x80000000]}); 
               (6 -> i32 {0x2710})
  owi: [DEBUG] serializing stacks (widen) : 
                first :  
                second : 
  owi: [DEBUG] abstract state : 
                 context: Context{id=9,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  <(B:26)> -> [--..--]
                                  }
                 stack  : 
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 [--..--];
                 i32 {0x2710}
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 [--..--]);  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : i32.const 100
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=9,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  <(B:26)> -> [--..--]
                                  }
                 stack  : i32 {100}
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 [--..--];
                 i32 {0x2710}
  owi: [INFO] stack         : [ i32 {100} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 [--..--]);  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : local.get 4
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=9,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  <(B:26)> -> [--..--]
                                  }
                 stack  : i32 {1} ; i32 {100}
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 [--..--];
                 i32 {0x2710}
  owi: [INFO] stack         : [ i32 {1} ; i32 {100} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 [--..--]);  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : i32.div_s
  owi: [DEBUG] marked 24 as IMPOSSIBLE divide by zero
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=9,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  <(B:26)> -> [--..--]
                                  }
                 stack  : i32 {100}
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 [--..--];
                 i32 {0x2710}
  owi: [INFO] stack         : [ i32 {100} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 [--..--]);  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : drop
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=9,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  <(B:26)> -> [--..--]
                                  }
                 stack  : 
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 [--..--];
                 i32 {0x2710}
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 [--..--]);  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : local.get 5
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=9,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  <(B:26)> -> [--..--]
                                  }
                 stack  : i32 [--..--]
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 [--..--];
                 i32 {0x2710}
  owi: [INFO] stack         : [ i32 [--..--] ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 [--..--]);  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : i32.const 1
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=9,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  <(B:26)> -> [--..--]
                                  }
                 stack  : i32 {1} ; i32 [--..--]
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 [--..--];
                 i32 {0x2710}
  owi: [INFO] stack         : [ i32 {1} ; i32 [--..--] ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 [--..--]);  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : i32.add
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=9,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  <(B:26)> -> [--..--]
                                  <(B:27)> -> [--..--]
                                  }
                 stack  : i32 [--..--]
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 [--..--];
                 i32 {0x2710}
  owi: [INFO] stack         : [ i32 [--..--] ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 [--..--]);  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : local.tee 5
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=9,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  <(B:26)> -> [--..--]
                                  <(B:27)> -> [--..--]
                                  }
                 stack  : i32 [--..--]
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 [--..--];
                 i32 {0x2710}
  owi: [INFO] stack         : [ i32 [--..--] ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 [--..--]);  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : local.get 6
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=9,
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  <(B:26)> -> [--..--]
                                  <(B:27)> -> [--..--]
                                  }
                 stack  : i32 {0x2710} ; i32 [--..--]
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 [--..--];
                 i32 {0x2710}
  owi: [INFO] stack         : [ i32 {0x2710} ; i32 [--..--] ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 [--..--]);  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : i32.lt_s
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=9,
                                  <(b:28)> -> {true;false};
                                  <(b:31)> -> {true;false};
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  <(B:26)> -> [--..--]
                                  <(B:27)> -> [--..--]
                                  <(B:32)> -> {0; 1}
                                  }
                 stack  : i32 {0; 1}
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 [--..--];
                 i32 {0x2710}
  owi: [INFO] stack         : [ i32 {0; 1} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 [--..--]);  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : br_if 0
  owi: [DEBUG] jt            :  0 -> 
                 context: Context{id=10,
                                  <(b:28)> -> {false}; <(b:31)> -> {true};
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  <(B:26)> -> [--..--]
                                  <(B:27)> -> [-0x80000000..9999]
                                  <(B:32)> -> {0; 1}
                                  }
                 stack  : 
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 [-0x80000000..9999];
                 i32 {0x2710}
  owi: [DEBUG] serializing locals (join) : 
                first : (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
               (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
               (5 -> i32 {0}); 
               (6 -> i32 {0x2710}) 
                second : (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
               (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
               (5 -> i32 [-0x80000000..9999]);  (6 -> i32 {0x2710})
  owi: [DEBUG] serializing stacks (join) : 
                first :  
                second : 
  owi: [DEBUG] serializing locals (widen) : 
                first : (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
               (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
               (5 -> i32 [--..--]); 
               (6 -> i32 {0x2710}) 
                second : (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
               (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
               (5 -> i32 [-0x80000000..9999]);  (6 -> i32 {0x2710})
  owi: [DEBUG] serializing stacks (widen) : 
                first :  
                second : 
  owi: [DEBUG] jt            :  
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=11,
                                  <(b:28)> -> {true}; <(b:31)> -> {true;false};
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  <(B:26)> -> [9999..0x7FFFFFFE]
                                  <(B:27)> -> [0x2710..0x7FFFFFFF]
                                  <(B:32)> -> {0; 1}
                                  }
                 stack  : 
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 [0x2710..0x7FFFFFFF];
                 i32 {0x2710}
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 [0x2710..0x7FFFFFFF]);  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=11,
                                  <(b:28)> -> {true}; <(b:31)> -> {true;false};
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  <(B:26)> -> [9999..0x7FFFFFFE]
                                  <(B:27)> -> [0x2710..0x7FFFFFFF]
                                  <(B:32)> -> {0; 1}
                                  }
                 stack  : i32 [--..--]
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 [0x2710..0x7FFFFFFF];
                 i32 {0x2710}
  owi: [INFO] stack         : [ i32 [--..--] ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 [0x2710..0x7FFFFFFF]);  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : i32.const 4242
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=11,
                                  <(b:28)> -> {true}; <(b:31)> -> {true;false};
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  <(B:26)> -> [9999..0x7FFFFFFE]
                                  <(B:27)> -> [0x2710..0x7FFFFFFF]
                                  <(B:32)> -> {0; 1}
                                  }
                 stack  : i32 {4242} ; i32 [--..--]
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 [0x2710..0x7FFFFFFF];
                 i32 {0x2710}
  owi: [INFO] stack         : [ i32 {4242} ; i32 [--..--] ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 [0x2710..0x7FFFFFFF]);  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : i32.eq
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=11,
                                  <(b:28)> -> {true}; <(b:31)> -> {true;false};
                                  <(b:42)> -> {true;false};
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  <(B:26)> -> [9999..0x7FFFFFFE]
                                  <(B:27)> -> [0x2710..0x7FFFFFFF]
                                  <(B:32)> -> {0; 1}
                                  <(B:43)> -> {0; 1}
                                  }
                 stack  : i32 {0; 1}
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 [0x2710..0x7FFFFFFF];
                 i32 {0x2710}
  owi: [INFO] stack         : [ i32 {0; 1} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 [0x2710..0x7FFFFFFF]);  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : if
  owi: [DEBUG] abstract state : 
                 context: Context{id=15,
                                  <(b:28)> -> {true}; <(b:31)> -> {true;false};
                                  <(b:42)> -> {false}; <(b:44)> -> {true};
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  <(B:26)> -> [9999..0x7FFFFFFE]
                                  <(B:27)> -> [0x2710..0x7FFFFFFF]
                                  <(B:32)> -> {0; 1}
                                  <(B:43)> -> {0; 1}
                                  }
                 stack  : 
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 [0x2710..0x7FFFFFFF];
                 i32 {0x2710}
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 [0x2710..0x7FFFFFFF]);  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : block
  owi: [DEBUG] abstract state : 
                 context: Context{id=14,
                                  <(b:28)> -> {true}; <(b:31)> -> {true;false};
                                  <(b:42)> -> {true}; <(b:44)> -> {true;false};
                                  <(B:9)> -> {4242}
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  <(B:26)> -> [9999..0x7FFFFFFE]
                                  <(B:27)> -> [0x2710..0x7FFFFFFF]
                                  <(B:32)> -> {0; 1}
                                  <(B:43)> -> {0; 1}
                                  }
                 stack  : 
                 locals : i32 {4242};
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 [0x2710..0x7FFFFFFF];
                 i32 {0x2710}
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {4242});  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 [0x2710..0x7FFFFFFF]);  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : block
  owi: [DEBUG] abstract state : 
                 context: Context{id=14,
                                  <(b:28)> -> {true}; <(b:31)> -> {true;false};
                                  <(b:42)> -> {true}; <(b:44)> -> {true;false};
                                  <(B:9)> -> {4242}
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  <(B:26)> -> [9999..0x7FFFFFFE]
                                  <(B:27)> -> [0x2710..0x7FFFFFFF]
                                  <(B:32)> -> {0; 1}
                                  <(B:43)> -> {0; 1}
                                  }
                 stack  : 
                 locals : i32 {4242};
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 [0x2710..0x7FFFFFFF];
                 i32 {0x2710}
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {4242});  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 [0x2710..0x7FFFFFFF]);  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : i32.const 0
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=14,
                                  <(b:28)> -> {true}; <(b:31)> -> {true;false};
                                  <(b:42)> -> {true}; <(b:44)> -> {true;false};
                                  <(B:9)> -> {4242}
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  <(B:26)> -> [9999..0x7FFFFFFE]
                                  <(B:27)> -> [0x2710..0x7FFFFFFF]
                                  <(B:32)> -> {0; 1}
                                  <(B:43)> -> {0; 1}
                                  }
                 stack  : i32 {0}
                 locals : i32 {4242};
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {1};
                 i32 [0x2710..0x7FFFFFFF];
                 i32 {0x2710}
  owi: [INFO] stack         : [ i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {4242});  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 [0x2710..0x7FFFFFFF]);  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : local.set 4
  owi: [DEBUG] jt            :  
  owi: [DEBUG] serializing locals (join) : 
                first : (0 -> i32 {4242});  (1 -> i32 [--..--]); 
               (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {0}); 
               (5 -> i32 [0x2710..0x7FFFFFFF]); 
               (6 -> i32 {0x2710}) 
                second : (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
               (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
               (5 -> i32 [0x2710..0x7FFFFFFF]);  (6 -> i32 {0x2710})
  owi: [DEBUG] serializing stacks (join) : 
                first :  
                second : 
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=16,
                                  <(b:28)> -> {true}; <(b:31)> -> {true;false};
                                  <(b:42)> -> {true;false};
                                  <(b:44)> -> {true;false};
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  <(B:26)> -> [9999..0x7FFFFFFE]
                                  <(B:27)> -> [0x2710..0x7FFFFFFF]
                                  <(B:32)> -> {0; 1}
                                  <(B:43)> -> {0; 1}
                                  <(B:49)> -> {0; 1}
                                  }
                 stack  : 
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {0; 1};
                 i32 [0x2710..0x7FFFFFFF];
                 i32 {0x2710}
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {0; 1}); 
              (5 -> i32 [0x2710..0x7FFFFFFF]);  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : i32.const 42
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=16,
                                  <(b:28)> -> {true}; <(b:31)> -> {true;false};
                                  <(b:42)> -> {true;false};
                                  <(b:44)> -> {true;false};
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  <(B:26)> -> [9999..0x7FFFFFFE]
                                  <(B:27)> -> [0x2710..0x7FFFFFFF]
                                  <(B:32)> -> {0; 1}
                                  <(B:43)> -> {0; 1}
                                  <(B:49)> -> {0; 1}
                                  }
                 stack  : i32 {42}
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {0; 1};
                 i32 [0x2710..0x7FFFFFFF];
                 i32 {0x2710}
  owi: [INFO] stack         : [ i32 {42} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {0; 1}); 
              (5 -> i32 [0x2710..0x7FFFFFFF]);  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : local.get 4
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=16,
                                  <(b:28)> -> {true}; <(b:31)> -> {true;false};
                                  <(b:42)> -> {true;false};
                                  <(b:44)> -> {true;false};
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  <(B:26)> -> [9999..0x7FFFFFFE]
                                  <(B:27)> -> [0x2710..0x7FFFFFFF]
                                  <(B:32)> -> {0; 1}
                                  <(B:43)> -> {0; 1}
                                  <(B:49)> -> {0; 1}
                                  }
                 stack  : i32 {0; 1} ; i32 {42}
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {0; 1};
                 i32 [0x2710..0x7FFFFFFF];
                 i32 {0x2710}
  owi: [INFO] stack         : [ i32 {0; 1} ; i32 {42} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {0; 1}); 
              (5 -> i32 [0x2710..0x7FFFFFFF]);  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : i32.div_s
  owi: [DEBUG] marked 47 as POSSIBLE divide by zero
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=16,
                                  <(b:28)> -> {true}; <(b:31)> -> {true;false};
                                  <(b:42)> -> {true;false};
                                  <(b:44)> -> {true;false};
                                  <(b:52)> -> {true;false};
                                  <(b:54)> -> {true;false};
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  <(B:26)> -> [9999..0x7FFFFFFE]
                                  <(B:27)> -> [0x2710..0x7FFFFFFF]
                                  <(B:32)> -> {0; 1}
                                  <(B:43)> -> {0; 1}
                                  <(B:49)> -> {0; 1}
                                  <(B:56)> -> {42}
                                  }
                 stack  : i32 {42}
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {0; 1};
                 i32 [0x2710..0x7FFFFFFF];
                 i32 {0x2710}
  owi: [INFO] stack         : [ i32 {42} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {0; 1}); 
              (5 -> i32 [0x2710..0x7FFFFFFF]);  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : drop
  owi: [DEBUG] jt            :  
  owi: [DEBUG] after call(start): abstract state : 
                 context: Context{id=16,
                                  <(b:28)> -> {true}; <(b:31)> -> {true;false};
                                  <(b:42)> -> {true;false};
                                  <(b:44)> -> {true;false};
                                  <(b:52)> -> {true;false};
                                  <(b:54)> -> {true;false};
                                  <(B:9)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  <(B:26)> -> [9999..0x7FFFFFFE]
                                  <(B:27)> -> [0x2710..0x7FFFFFFF]
                                  <(B:32)> -> {0; 1}
                                  <(B:43)> -> {0; 1}
                                  <(B:49)> -> {0; 1}
                                  <(B:56)> -> {42}
                                  }
                 stack  : 
                 locals : i32 [--..--];
                 i32 [--..--];
                 i32 [--..--];
                 i32 {30};
                 i32 {0; 1};
                 i32 [0x2710..0x7FFFFFFF];
                 i32 {0x2710}
  
  owi: [DEBUG] jt            :  
  owi: [INFO] Passed division by zero check for expression:(uuid: 24) i32.div_s
  owi: [WARNING] Possible division by zero for expression:(uuid: 47) i32.div_s
