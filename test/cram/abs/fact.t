  $ owi abs fact.wat -vv 
  owi: [INFO] parsing      ...
  owi: [INFO] checking     ...
  owi: [DEBUG] grouping     ...
  owi: [DEBUG] {id: 
                 typ: 
                 decl_types: (func)
                 type_checks: 
                 global: 
                 table: 
                 mem: 
                 func: Local ((func $start (local $n i32) (local $result i32)
                   i32.const 2
                   local.set $n
                   i32.const 1
                   local.set $result
                   (block $done
                     (loop $continue
                       local.get $n
                       i32.const 1
                       i32.le_s
                       br_if $done
                       local.get $result
                       local.get $n
                       i32.mul
                       local.set $result
                       local.get $n
                       i32.const 1
                       i32.sub
                       local.set $n
                       br $continue))
                   local.get $result
                   drop
                 ))
                 elem: 
                 data: 
                 start: $start
                 }
  owi: [DEBUG] assigning    ...
  owi: [DEBUG] Types: (func)
               Types names: 
               Global names: 
               Table names: 
               Mem names: 
               Func names: ("start", 0)
               Elem names: 
               Data names: 
               Tag names: 
               
  owi: [DEBUG] rewriting    ...
  owi: [INFO] typechecking ...
  owi: [DEBUG] stack             : []
  owi: [DEBUG] typechecking instr: i32.const 2
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: local.set 0
  owi: [DEBUG] stack             : []
  owi: [DEBUG] typechecking instr: i32.const 1
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: local.set 1
  owi: [DEBUG] stack             : []
  owi: [DEBUG] typechecking instr: block $done
  owi: [DEBUG] stack             : []
  owi: [DEBUG] typechecking instr: loop $continue
  owi: [DEBUG] stack             : []
  owi: [DEBUG] typechecking instr: local.get 0
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: i32.const 1
  owi: [DEBUG] stack             : [i32 i32]
  owi: [DEBUG] typechecking instr: i32.le_s
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: br_if 1
  owi: [DEBUG] stack             : []
  owi: [DEBUG] typechecking instr: local.get 1
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: local.get 0
  owi: [DEBUG] stack             : [i32 i32]
  owi: [DEBUG] typechecking instr: i32.mul
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: local.set 1
  owi: [DEBUG] stack             : []
  owi: [DEBUG] typechecking instr: local.get 0
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: i32.const 1
  owi: [DEBUG] stack             : [i32 i32]
  owi: [DEBUG] typechecking instr: i32.sub
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: local.set 0
  owi: [DEBUG] stack             : []
  owi: [DEBUG] typechecking instr: br 0
  owi: [DEBUG] stack             : []
  owi: [DEBUG] typechecking instr: local.get 1
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: drop
  owi: [INFO] linking      ...
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : 
                 locals : 
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : call 0
  owi: [INFO] calling func  : func start
  owi: [DEBUG] call (start): abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : 
                 locals : i32 {0};
                 i32 {0}
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : 
                 locals : i32 {0};
                 i32 {0}
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {0});  (1 -> i32 {0}) ]
  owi: [INFO] running instr : i32.const 2
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {2}
                 locals : i32 {0};
                 i32 {0}
  owi: [INFO] stack         : [ i32 {2} ]
  owi: [INFO] locals        : [ (0 -> i32 {0});  (1 -> i32 {0}) ]
  owi: [INFO] running instr : local.set 0
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : 
                 locals : i32 {2};
                 i32 {0}
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {2});  (1 -> i32 {0}) ]
  owi: [INFO] running instr : i32.const 1
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {1}
                 locals : i32 {2};
                 i32 {0}
  owi: [INFO] stack         : [ i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 {2});  (1 -> i32 {0}) ]
  owi: [INFO] running instr : local.set 1
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : 
                 locals : i32 {2};
                 i32 {1}
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {2});  (1 -> i32 {1}) ]
  owi: [INFO] running instr : block $done
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : 
                 locals : i32 {2};
                 i32 {1}
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {2});  (1 -> i32 {1}) ]
  owi: [INFO] running instr : loop $continue
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : 
                 locals : i32 {2};
                 i32 {1}
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {2});  (1 -> i32 {1}) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {2}
                 locals : i32 {2};
                 i32 {1}
  owi: [INFO] stack         : [ i32 {2} ]
  owi: [INFO] locals        : [ (0 -> i32 {2});  (1 -> i32 {1}) ]
  owi: [INFO] running instr : i32.const 1
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {1} ; i32 {2}
                 locals : i32 {2};
                 i32 {1}
  owi: [INFO] stack         : [ i32 {1} ; i32 {2} ]
  owi: [INFO] locals        : [ (0 -> i32 {2});  (1 -> i32 {1}) ]
  owi: [INFO] running instr : i32.le_s
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {0}
                 locals : i32 {2};
                 i32 {1}
  owi: [INFO] stack         : [ i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {2});  (1 -> i32 {1}) ]
  owi: [INFO] running instr : br_if 1
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : 
                 locals : i32 {2};
                 i32 {1}
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {2});  (1 -> i32 {1}) ]
  owi: [INFO] running instr : local.get 1
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {1}
                 locals : i32 {2};
                 i32 {1}
  owi: [INFO] stack         : [ i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 {2});  (1 -> i32 {1}) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {2} ; i32 {1}
                 locals : i32 {2};
                 i32 {1}
  owi: [INFO] stack         : [ i32 {2} ; i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 {2});  (1 -> i32 {1}) ]
  owi: [INFO] running instr : i32.mul
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {2}
                 locals : i32 {2};
                 i32 {1}
  owi: [INFO] stack         : [ i32 {2} ]
  owi: [INFO] locals        : [ (0 -> i32 {2});  (1 -> i32 {1}) ]
  owi: [INFO] running instr : local.set 1
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : 
                 locals : i32 {2};
                 i32 {2}
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {2});  (1 -> i32 {2}) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {2}
                 locals : i32 {2};
                 i32 {2}
  owi: [INFO] stack         : [ i32 {2} ]
  owi: [INFO] locals        : [ (0 -> i32 {2});  (1 -> i32 {2}) ]
  owi: [INFO] running instr : i32.const 1
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {1} ; i32 {2}
                 locals : i32 {2};
                 i32 {2}
  owi: [INFO] stack         : [ i32 {1} ; i32 {2} ]
  owi: [INFO] locals        : [ (0 -> i32 {2});  (1 -> i32 {2}) ]
  owi: [INFO] running instr : i32.sub
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {1}
                 locals : i32 {2};
                 i32 {2}
  owi: [INFO] stack         : [ i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 {2});  (1 -> i32 {2}) ]
  owi: [INFO] running instr : local.set 0
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : 
                 locals : i32 {1};
                 i32 {2}
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {1});  (1 -> i32 {2}) ]
  owi: [INFO] running instr : br 0
  owi: [DEBUG] jt            :  0 -> 
                 context: Context{id=1, <empty>}
                 stack  : 
                 locals : i32 {1};
                 i32 {2}
  owi: [DEBUG] serializing locals (join) : 
                first : (0 -> i32 {2}); 
               (1 -> i32 {1}) 
                second : (0 -> i32 {1});  (1 -> i32 {2})
  owi: [DEBUG] serializing stacks (join) : 
                first :  
                second : 
  owi: [DEBUG] serializing locals (widen) : 
                first : (0 -> i32 {2}); 
               (1 -> i32 {1}) 
                second : (0 -> i32 {1; 2});  (1 -> i32 {1; 2})
  owi: [DEBUG] serializing stacks (widen) : 
                first :  
                second : 
  owi: [DEBUG] abstract state : 
                 context: Context{id=3, <(B:18)> -> {1; 2}
                                        <(B:19)> -> {1; 2}
                                        }
                 stack  : 
                 locals : i32 {1; 2};
                 i32 {1; 2}
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {1; 2});  (1 -> i32 {1; 2}) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=3, <(B:18)> -> {1; 2}
                                        <(B:19)> -> {1; 2}
                                        }
                 stack  : i32 {1; 2}
                 locals : i32 {1; 2};
                 i32 {1; 2}
  owi: [INFO] stack         : [ i32 {1; 2} ]
  owi: [INFO] locals        : [ (0 -> i32 {1; 2});  (1 -> i32 {1; 2}) ]
  owi: [INFO] running instr : i32.const 1
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=3, <(B:18)> -> {1; 2}
                                        <(B:19)> -> {1; 2}
                                        }
                 stack  : i32 {1} ; i32 {1; 2}
                 locals : i32 {1; 2};
                 i32 {1; 2}
  owi: [INFO] stack         : [ i32 {1} ; i32 {1; 2} ]
  owi: [INFO] locals        : [ (0 -> i32 {1; 2});  (1 -> i32 {1; 2}) ]
  owi: [INFO] running instr : i32.le_s
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=3,
                                  <(b:20)> -> {true;false};
                                  <(B:18)> -> {1; 2}
                                  <(B:19)> -> {1; 2}
                                  <(B:21)> -> {0; 1}
                                  }
                 stack  : i32 {0; 1}
                 locals : i32 {1; 2};
                 i32 {1; 2}
  owi: [INFO] stack         : [ i32 {0; 1} ]
  owi: [INFO] locals        : [ (0 -> i32 {1; 2});  (1 -> i32 {1; 2}) ]
  owi: [INFO] running instr : br_if 1
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=4,
                                  <(b:20)> -> {true}; <(b:22)> -> {true;false};
                                  <(B:18)> -> {1}
                                  <(B:19)> -> {1; 2}
                                  <(B:21)> -> {0; 1}
                                  }
                 stack  : 
                 locals : i32 {1};
                 i32 {1; 2}
  owi: [DEBUG] abstract state : 
                 context: Context{id=5,
                                  <(b:20)> -> {false}; <(b:22)> -> {true};
                                  <(B:18)> -> {2}
                                  <(B:19)> -> {1; 2}
                                  <(B:21)> -> {0; 1}
                                  }
                 stack  : 
                 locals : i32 {2};
                 i32 {1; 2}
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {2});  (1 -> i32 {1; 2}) ]
  owi: [INFO] running instr : local.get 1
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=4,
                                  <(b:20)> -> {true}; <(b:22)> -> {true;false};
                                  <(B:18)> -> {1}
                                  <(B:19)> -> {1; 2}
                                  <(B:21)> -> {0; 1}
                                  }
                 stack  : 
                 locals : i32 {1};
                 i32 {1; 2}
  owi: [DEBUG] abstract state : 
                 context: Context{id=5,
                                  <(b:20)> -> {false}; <(b:22)> -> {true};
                                  <(B:18)> -> {2}
                                  <(B:19)> -> {1; 2}
                                  <(B:21)> -> {0; 1}
                                  }
                 stack  : i32 {1; 2}
                 locals : i32 {2};
                 i32 {1; 2}
  owi: [INFO] stack         : [ i32 {1; 2} ]
  owi: [INFO] locals        : [ (0 -> i32 {2});  (1 -> i32 {1; 2}) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=4,
                                  <(b:20)> -> {true}; <(b:22)> -> {true;false};
                                  <(B:18)> -> {1}
                                  <(B:19)> -> {1; 2}
                                  <(B:21)> -> {0; 1}
                                  }
                 stack  : 
                 locals : i32 {1};
                 i32 {1; 2}
  owi: [DEBUG] abstract state : 
                 context: Context{id=5,
                                  <(b:20)> -> {false}; <(b:22)> -> {true};
                                  <(B:18)> -> {2}
                                  <(B:19)> -> {1; 2}
                                  <(B:21)> -> {0; 1}
                                  }
                 stack  : i32 {2} ; i32 {1; 2}
                 locals : i32 {2};
                 i32 {1; 2}
  owi: [INFO] stack         : [ i32 {2} ; i32 {1; 2} ]
  owi: [INFO] locals        : [ (0 -> i32 {2});  (1 -> i32 {1; 2}) ]
  owi: [INFO] running instr : i32.mul
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=4,
                                  <(b:20)> -> {true}; <(b:22)> -> {true;false};
                                  <(B:18)> -> {1}
                                  <(B:19)> -> {1; 2}
                                  <(B:21)> -> {0; 1}
                                  }
                 stack  : 
                 locals : i32 {1};
                 i32 {1; 2}
  owi: [DEBUG] abstract state : 
                 context: Context{id=5,
                                  <(b:20)> -> {false}; <(b:22)> -> {true};
                                  <(B:18)> -> {2}
                                  <(B:19)> -> {1; 2}
                                  <(B:21)> -> {0; 1}
                                  <(B:24)> -> {2; 4}
                                  }
                 stack  : i32 {2; 4}
                 locals : i32 {2};
                 i32 {1; 2}
  owi: [INFO] stack         : [ i32 {2; 4} ]
  owi: [INFO] locals        : [ (0 -> i32 {2});  (1 -> i32 {1; 2}) ]
  owi: [INFO] running instr : local.set 1
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=4,
                                  <(b:20)> -> {true}; <(b:22)> -> {true;false};
                                  <(B:18)> -> {1}
                                  <(B:19)> -> {1; 2}
                                  <(B:21)> -> {0; 1}
                                  }
                 stack  : 
                 locals : i32 {1};
                 i32 {1; 2}
  owi: [DEBUG] abstract state : 
                 context: Context{id=5,
                                  <(b:20)> -> {false}; <(b:22)> -> {true};
                                  <(B:18)> -> {2}
                                  <(B:19)> -> {1; 2}
                                  <(B:21)> -> {0; 1}
                                  <(B:24)> -> {2; 4}
                                  }
                 stack  : 
                 locals : i32 {2};
                 i32 {2; 4}
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {2});  (1 -> i32 {2; 4}) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=4,
                                  <(b:20)> -> {true}; <(b:22)> -> {true;false};
                                  <(B:18)> -> {1}
                                  <(B:19)> -> {1; 2}
                                  <(B:21)> -> {0; 1}
                                  }
                 stack  : 
                 locals : i32 {1};
                 i32 {1; 2}
  owi: [DEBUG] abstract state : 
                 context: Context{id=5,
                                  <(b:20)> -> {false}; <(b:22)> -> {true};
                                  <(B:18)> -> {2}
                                  <(B:19)> -> {1; 2}
                                  <(B:21)> -> {0; 1}
                                  <(B:24)> -> {2; 4}
                                  }
                 stack  : i32 {2}
                 locals : i32 {2};
                 i32 {2; 4}
  owi: [INFO] stack         : [ i32 {2} ]
  owi: [INFO] locals        : [ (0 -> i32 {2});  (1 -> i32 {2; 4}) ]
  owi: [INFO] running instr : i32.const 1
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=4,
                                  <(b:20)> -> {true}; <(b:22)> -> {true;false};
                                  <(B:18)> -> {1}
                                  <(B:19)> -> {1; 2}
                                  <(B:21)> -> {0; 1}
                                  }
                 stack  : 
                 locals : i32 {1};
                 i32 {1; 2}
  owi: [DEBUG] abstract state : 
                 context: Context{id=5,
                                  <(b:20)> -> {false}; <(b:22)> -> {true};
                                  <(B:18)> -> {2}
                                  <(B:19)> -> {1; 2}
                                  <(B:21)> -> {0; 1}
                                  <(B:24)> -> {2; 4}
                                  }
                 stack  : i32 {1} ; i32 {2}
                 locals : i32 {2};
                 i32 {2; 4}
  owi: [INFO] stack         : [ i32 {1} ; i32 {2} ]
  owi: [INFO] locals        : [ (0 -> i32 {2});  (1 -> i32 {2; 4}) ]
  owi: [INFO] running instr : i32.sub
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=4,
                                  <(b:20)> -> {true}; <(b:22)> -> {true;false};
                                  <(B:18)> -> {1}
                                  <(B:19)> -> {1; 2}
                                  <(B:21)> -> {0; 1}
                                  }
                 stack  : 
                 locals : i32 {1};
                 i32 {1; 2}
  owi: [DEBUG] abstract state : 
                 context: Context{id=5,
                                  <(b:20)> -> {false}; <(b:22)> -> {true};
                                  <(B:18)> -> {2}
                                  <(B:19)> -> {1; 2}
                                  <(B:21)> -> {0; 1}
                                  <(B:24)> -> {2; 4}
                                  <(B:25)> -> {1}
                                  }
                 stack  : i32 {1}
                 locals : i32 {2};
                 i32 {2; 4}
  owi: [INFO] stack         : [ i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 {2});  (1 -> i32 {2; 4}) ]
  owi: [INFO] running instr : local.set 0
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=4,
                                  <(b:20)> -> {true}; <(b:22)> -> {true;false};
                                  <(B:18)> -> {1}
                                  <(B:19)> -> {1; 2}
                                  <(B:21)> -> {0; 1}
                                  }
                 stack  : 
                 locals : i32 {1};
                 i32 {1; 2}
  owi: [DEBUG] abstract state : 
                 context: Context{id=5,
                                  <(b:20)> -> {false}; <(b:22)> -> {true};
                                  <(B:18)> -> {2}
                                  <(B:19)> -> {1; 2}
                                  <(B:21)> -> {0; 1}
                                  <(B:24)> -> {2; 4}
                                  <(B:25)> -> {1}
                                  }
                 stack  : 
                 locals : i32 {1};
                 i32 {2; 4}
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {1});  (1 -> i32 {2; 4}) ]
  owi: [INFO] running instr : br 0
  owi: [DEBUG] jt            :  0 -> 
                 context: Context{id=5,
                                  <(b:20)> -> {false}; <(b:22)> -> {true};
                                  <(B:18)> -> {2}
                                  <(B:19)> -> {1; 2}
                                  <(B:21)> -> {0; 1}
                                  <(B:24)> -> {2; 4}
                                  <(B:25)> -> {1}
                                  }
                 stack  : 
                 locals : i32 {1};
                 i32 {2; 4}
               1 -> 
                 context: Context{id=4,
                                  <(b:20)> -> {true}; <(b:22)> -> {true;false};
                                  <(B:18)> -> {1}
                                  <(B:19)> -> {1; 2}
                                  <(B:21)> -> {0; 1}
                                  }
                 stack  : 
                 locals : i32 {1};
                 i32 {1; 2}
  owi: [DEBUG] serializing locals (join) : 
                first : (0 -> i32 {2}); 
               (1 -> i32 {1}) 
                second : (0 -> i32 {1});  (1 -> i32 {2; 4})
  owi: [DEBUG] serializing stacks (join) : 
                first :  
                second : 
  owi: [DEBUG] serializing locals (widen) : 
                first : (0 -> i32 {1; 2}); 
               (1 -> i32 {1; 2}) 
                second : (0 -> i32 {1; 2});  (1 -> i32 {1; 2; 4})
  owi: [DEBUG] serializing stacks (widen) : 
                first :  
                second : 
  owi: [DEBUG] abstract state : 
                 context: Context{id=7,
                                  <(B:18)> -> {1; 2}
                                  <(B:19)> -> [1..0x7FFFFFFF]
                                  }
                 stack  : 
                 locals : i32 {1; 2};
                 i32 [1..0x7FFFFFFF]
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {1; 2}); 
              (1 -> i32 [1..0x7FFFFFFF]) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=7,
                                  <(B:18)> -> {1; 2}
                                  <(B:19)> -> [1..0x7FFFFFFF]
                                  }
                 stack  : i32 {1; 2}
                 locals : i32 {1; 2};
                 i32 [1..0x7FFFFFFF]
  owi: [INFO] stack         : [ i32 {1; 2} ]
  owi: [INFO] locals        : [ (0 -> i32 {1; 2}); 
              (1 -> i32 [1..0x7FFFFFFF]) ]
  owi: [INFO] running instr : i32.const 1
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=7,
                                  <(B:18)> -> {1; 2}
                                  <(B:19)> -> [1..0x7FFFFFFF]
                                  }
                 stack  : i32 {1} ; i32 {1; 2}
                 locals : i32 {1; 2};
                 i32 [1..0x7FFFFFFF]
  owi: [INFO] stack         : [ i32 {1} ; i32 {1; 2} ]
  owi: [INFO] locals        : [ (0 -> i32 {1; 2}); 
              (1 -> i32 [1..0x7FFFFFFF]) ]
  owi: [INFO] running instr : i32.le_s
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=7,
                                  <(b:20)> -> {true;false};
                                  <(B:18)> -> {1; 2}
                                  <(B:19)> -> [1..0x7FFFFFFF]
                                  <(B:21)> -> {0; 1}
                                  }
                 stack  : i32 {0; 1}
                 locals : i32 {1; 2};
                 i32 [1..0x7FFFFFFF]
  owi: [INFO] stack         : [ i32 {0; 1} ]
  owi: [INFO] locals        : [ (0 -> i32 {1; 2}); 
              (1 -> i32 [1..0x7FFFFFFF]) ]
  owi: [INFO] running instr : br_if 1
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=8,
                                  <(b:20)> -> {true}; <(b:22)> -> {true;false};
                                  <(B:18)> -> {1}
                                  <(B:19)> -> [1..0x7FFFFFFF]
                                  <(B:21)> -> {0; 1}
                                  }
                 stack  : 
                 locals : i32 {1};
                 i32 [1..0x7FFFFFFF]
  owi: [DEBUG] abstract state : 
                 context: Context{id=9,
                                  <(b:20)> -> {false}; <(b:22)> -> {true};
                                  <(B:18)> -> {2}
                                  <(B:19)> -> [1..0x7FFFFFFF]
                                  <(B:21)> -> {0; 1}
                                  }
                 stack  : 
                 locals : i32 {2};
                 i32 [1..0x7FFFFFFF]
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {2});  (1 -> i32 [1..0x7FFFFFFF]) ]
  owi: [INFO] running instr : local.get 1
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=8,
                                  <(b:20)> -> {true}; <(b:22)> -> {true;false};
                                  <(B:18)> -> {1}
                                  <(B:19)> -> [1..0x7FFFFFFF]
                                  <(B:21)> -> {0; 1}
                                  }
                 stack  : 
                 locals : i32 {1};
                 i32 [1..0x7FFFFFFF]
  owi: [DEBUG] abstract state : 
                 context: Context{id=9,
                                  <(b:20)> -> {false}; <(b:22)> -> {true};
                                  <(B:18)> -> {2}
                                  <(B:19)> -> [1..0x7FFFFFFF]
                                  <(B:21)> -> {0; 1}
                                  }
                 stack  : i32 [1..0x7FFFFFFF]
                 locals : i32 {2};
                 i32 [1..0x7FFFFFFF]
  owi: [INFO] stack         : [ i32 [1..0x7FFFFFFF] ]
  owi: [INFO] locals        : [ (0 -> i32 {2});  (1 -> i32 [1..0x7FFFFFFF]) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=8,
                                  <(b:20)> -> {true}; <(b:22)> -> {true;false};
                                  <(B:18)> -> {1}
                                  <(B:19)> -> [1..0x7FFFFFFF]
                                  <(B:21)> -> {0; 1}
                                  }
                 stack  : 
                 locals : i32 {1};
                 i32 [1..0x7FFFFFFF]
  owi: [DEBUG] abstract state : 
                 context: Context{id=9,
                                  <(b:20)> -> {false}; <(b:22)> -> {true};
                                  <(B:18)> -> {2}
                                  <(B:19)> -> [1..0x7FFFFFFF]
                                  <(B:21)> -> {0; 1}
                                  }
                 stack  : i32 {2} ; i32 [1..0x7FFFFFFF]
                 locals : i32 {2};
                 i32 [1..0x7FFFFFFF]
  owi: [INFO] stack         : [ i32 {2} ; i32 [1..0x7FFFFFFF] ]
  owi: [INFO] locals        : [ (0 -> i32 {2});  (1 -> i32 [1..0x7FFFFFFF]) ]
  owi: [INFO] running instr : i32.mul
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=8,
                                  <(b:20)> -> {true}; <(b:22)> -> {true;false};
                                  <(B:18)> -> {1}
                                  <(B:19)> -> [1..0x7FFFFFFF]
                                  <(B:21)> -> {0; 1}
                                  }
                 stack  : 
                 locals : i32 {1};
                 i32 [1..0x7FFFFFFF]
  owi: [DEBUG] abstract state : 
                 context: Context{id=9,
                                  <(b:20)> -> {false}; <(b:22)> -> {true};
                                  <(B:18)> -> {2}
                                  <(B:19)> -> [1..0x7FFFFFFF]
                                  <(B:21)> -> {0; 1}
                                  <(B:24)> -> [2..0x7FFFFFFE],0%2
                                  }
                 stack  : i32 [2..0x7FFFFFFE],0%2
                 locals : i32 {2};
                 i32 [1..0x7FFFFFFF]
  owi: [INFO] stack         : [ i32 [2..0x7FFFFFFE],0%2 ]
  owi: [INFO] locals        : [ (0 -> i32 {2});  (1 -> i32 [1..0x7FFFFFFF]) ]
  owi: [INFO] running instr : local.set 1
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=8,
                                  <(b:20)> -> {true}; <(b:22)> -> {true;false};
                                  <(B:18)> -> {1}
                                  <(B:19)> -> [1..0x7FFFFFFF]
                                  <(B:21)> -> {0; 1}
                                  }
                 stack  : 
                 locals : i32 {1};
                 i32 [1..0x7FFFFFFF]
  owi: [DEBUG] abstract state : 
                 context: Context{id=9,
                                  <(b:20)> -> {false}; <(b:22)> -> {true};
                                  <(B:18)> -> {2}
                                  <(B:19)> -> [1..0x7FFFFFFF]
                                  <(B:21)> -> {0; 1}
                                  <(B:24)> -> [2..0x7FFFFFFE],0%2
                                  }
                 stack  : 
                 locals : i32 {2};
                 i32 [2..0x7FFFFFFE],0%2
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {2}); 
              (1 -> i32 [2..0x7FFFFFFE],0%2) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=8,
                                  <(b:20)> -> {true}; <(b:22)> -> {true;false};
                                  <(B:18)> -> {1}
                                  <(B:19)> -> [1..0x7FFFFFFF]
                                  <(B:21)> -> {0; 1}
                                  }
                 stack  : 
                 locals : i32 {1};
                 i32 [1..0x7FFFFFFF]
  owi: [DEBUG] abstract state : 
                 context: Context{id=9,
                                  <(b:20)> -> {false}; <(b:22)> -> {true};
                                  <(B:18)> -> {2}
                                  <(B:19)> -> [1..0x7FFFFFFF]
                                  <(B:21)> -> {0; 1}
                                  <(B:24)> -> [2..0x7FFFFFFE],0%2
                                  }
                 stack  : i32 {2}
                 locals : i32 {2};
                 i32 [2..0x7FFFFFFE],0%2
  owi: [INFO] stack         : [ i32 {2} ]
  owi: [INFO] locals        : [ (0 -> i32 {2}); 
              (1 -> i32 [2..0x7FFFFFFE],0%2) ]
  owi: [INFO] running instr : i32.const 1
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=8,
                                  <(b:20)> -> {true}; <(b:22)> -> {true;false};
                                  <(B:18)> -> {1}
                                  <(B:19)> -> [1..0x7FFFFFFF]
                                  <(B:21)> -> {0; 1}
                                  }
                 stack  : 
                 locals : i32 {1};
                 i32 [1..0x7FFFFFFF]
  owi: [DEBUG] abstract state : 
                 context: Context{id=9,
                                  <(b:20)> -> {false}; <(b:22)> -> {true};
                                  <(B:18)> -> {2}
                                  <(B:19)> -> [1..0x7FFFFFFF]
                                  <(B:21)> -> {0; 1}
                                  <(B:24)> -> [2..0x7FFFFFFE],0%2
                                  }
                 stack  : i32 {1} ; i32 {2}
                 locals : i32 {2};
                 i32 [2..0x7FFFFFFE],0%2
  owi: [INFO] stack         : [ i32 {1} ; i32 {2} ]
  owi: [INFO] locals        : [ (0 -> i32 {2}); 
              (1 -> i32 [2..0x7FFFFFFE],0%2) ]
  owi: [INFO] running instr : i32.sub
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=8,
                                  <(b:20)> -> {true}; <(b:22)> -> {true;false};
                                  <(B:18)> -> {1}
                                  <(B:19)> -> [1..0x7FFFFFFF]
                                  <(B:21)> -> {0; 1}
                                  }
                 stack  : 
                 locals : i32 {1};
                 i32 [1..0x7FFFFFFF]
  owi: [DEBUG] abstract state : 
                 context: Context{id=9,
                                  <(b:20)> -> {false}; <(b:22)> -> {true};
                                  <(B:18)> -> {2}
                                  <(B:19)> -> [1..0x7FFFFFFF]
                                  <(B:21)> -> {0; 1}
                                  <(B:24)> -> [2..0x7FFFFFFE],0%2
                                  <(B:25)> -> {1}
                                  }
                 stack  : i32 {1}
                 locals : i32 {2};
                 i32 [2..0x7FFFFFFE],0%2
  owi: [INFO] stack         : [ i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 {2}); 
              (1 -> i32 [2..0x7FFFFFFE],0%2) ]
  owi: [INFO] running instr : local.set 0
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=8,
                                  <(b:20)> -> {true}; <(b:22)> -> {true;false};
                                  <(B:18)> -> {1}
                                  <(B:19)> -> [1..0x7FFFFFFF]
                                  <(B:21)> -> {0; 1}
                                  }
                 stack  : 
                 locals : i32 {1};
                 i32 [1..0x7FFFFFFF]
  owi: [DEBUG] abstract state : 
                 context: Context{id=9,
                                  <(b:20)> -> {false}; <(b:22)> -> {true};
                                  <(B:18)> -> {2}
                                  <(B:19)> -> [1..0x7FFFFFFF]
                                  <(B:21)> -> {0; 1}
                                  <(B:24)> -> [2..0x7FFFFFFE],0%2
                                  <(B:25)> -> {1}
                                  }
                 stack  : 
                 locals : i32 {1};
                 i32 [2..0x7FFFFFFE],0%2
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {1}); 
              (1 -> i32 [2..0x7FFFFFFE],0%2) ]
  owi: [INFO] running instr : br 0
  owi: [DEBUG] jt            :  0 -> 
                 context: Context{id=9,
                                  <(b:20)> -> {false}; <(b:22)> -> {true};
                                  <(B:18)> -> {2}
                                  <(B:19)> -> [1..0x7FFFFFFF]
                                  <(B:21)> -> {0; 1}
                                  <(B:24)> -> [2..0x7FFFFFFE],0%2
                                  <(B:25)> -> {1}
                                  }
                 stack  : 
                 locals : i32 {1};
                 i32 [2..0x7FFFFFFE],0%2
               1 -> 
                 context: Context{id=8,
                                  <(b:20)> -> {true}; <(b:22)> -> {true;false};
                                  <(B:18)> -> {1}
                                  <(B:19)> -> [1..0x7FFFFFFF]
                                  <(B:21)> -> {0; 1}
                                  }
                 stack  : 
                 locals : i32 {1};
                 i32 [1..0x7FFFFFFF]
  owi: [DEBUG] serializing locals (join) : 
                first : (0 -> i32 {2}); 
               (1 -> i32 {1}) 
                second : (0 -> i32 {1});  (1 -> i32 [2..0x7FFFFFFE],0%2)
  owi: [DEBUG] serializing stacks (join) : 
                first :  
                second : 
  owi: [DEBUG] serializing locals (widen) : 
                first : (0 -> i32 {1; 2}); 
               (1 -> i32 [1..0x7FFFFFFF]) 
                second : (0 -> i32 {1; 2});  (1 -> i32 [1..0x7FFFFFFE])
  owi: [DEBUG] serializing stacks (widen) : 
                first :  
                second : 
  owi: [DEBUG] jt            :  0 -> 
                 context: Context{id=8,
                                  <(b:20)> -> {true}; <(b:22)> -> {true;false};
                                  <(B:18)> -> {1}
                                  <(B:19)> -> [1..0x7FFFFFFF]
                                  <(B:21)> -> {0; 1}
                                  }
                 stack  : 
                 locals : i32 {1};
                 i32 [1..0x7FFFFFFF]
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : None 
  
  owi: [DEBUG] jt            :  
