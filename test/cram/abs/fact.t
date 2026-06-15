  $ owi drun fact.wat -vv
  owi: [INFO] parsing      ...
  owi: [INFO] checking     ...
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
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : call 0
  owi: [INFO] calling func  : func start
  owi: [DEBUG] Func start state : σ:[]; ρ:0->i32.const 0 1->i32.const 0 
  
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ 0->i32.const 0 1->i32.const 0  ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32.const 2 ]
  owi: [INFO] locals        : [ 0->i32.const 0 1->i32.const 0  ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ 0->i32.const 2 1->i32.const 0  ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 2 1->i32.const 0  ]
  owi: [INFO] running instr : local.set 1
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ 0->i32.const 2 1->i32.const 1  ]
  owi: [INFO] running instr : block $done
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ 0->i32.const 2 1->i32.const 1  ]
  owi: [INFO] running instr : loop $continue
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ 0->i32.const 2 1->i32.const 1  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 2 ]
  owi: [INFO] locals        : [ 0->i32.const 2 1->i32.const 1  ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 2 ]
  owi: [INFO] locals        : [ 0->i32.const 2 1->i32.const 1  ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32.const 0 ]
  owi: [INFO] locals        : [ 0->i32.const 2 1->i32.const 1  ]
  owi: [INFO] running instr : br_if 1
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ 0->i32.const 2 1->i32.const 1  ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 2 1->i32.const 1  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 2 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 2 1->i32.const 1  ]
  owi: [INFO] running instr : i32.mul
  owi: [INFO] stack         : [ i32.const 2 ]
  owi: [INFO] locals        : [ 0->i32.const 2 1->i32.const 1  ]
  owi: [INFO] running instr : local.set 1
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ 0->i32.const 2 1->i32.const 2  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 2 ]
  owi: [INFO] locals        : [ 0->i32.const 2 1->i32.const 2  ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 2 ]
  owi: [INFO] locals        : [ 0->i32.const 2 1->i32.const 2  ]
  owi: [INFO] running instr : i32.sub
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 2 1->i32.const 2  ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ 0->i32.const 1 1->i32.const 2  ]
  owi: [INFO] running instr : br 0
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ 0->i32.const 1 1->i32.const 2  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 1 1->i32.const 2  ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 1 1->i32.const 2  ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 1 1->i32.const 2  ]
  owi: [INFO] running instr : br_if 1
  owi: [DEBUG] Func end state : None
  
  owi: [INFO] End Abstract_state : none
  

  $ owi abs fact.wat -vv 
  owi: [INFO] parsing      ...
  owi: [INFO] checking     ...
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
                 context: Context{id=3, <(B:17)> -> {1; 2}
                                        <(B:18)> -> {1; 2}
                                        }
                 stack  : 
                 locals : i32 {1; 2};
                 i32 {1; 2}
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {1; 2});  (1 -> i32 {1; 2}) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=3, <(B:17)> -> {1; 2}
                                        <(B:18)> -> {1; 2}
                                        }
                 stack  : i32 {1; 2}
                 locals : i32 {1; 2};
                 i32 {1; 2}
  owi: [INFO] stack         : [ i32 {1; 2} ]
  owi: [INFO] locals        : [ (0 -> i32 {1; 2});  (1 -> i32 {1; 2}) ]
  owi: [INFO] running instr : i32.const 1
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=3, <(B:17)> -> {1; 2}
                                        <(B:18)> -> {1; 2}
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
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(B:17)> -> {1; 2}
                                  <(B:18)> -> {1; 2}
                                  <(B:21)> -> [--..--]
                                  }
                 stack  : i32 [--..--]
                 locals : i32 {1; 2};
                 i32 {1; 2}
  owi: [INFO] stack         : [ i32 [--..--] ]
  owi: [INFO] locals        : [ (0 -> i32 {1; 2});  (1 -> i32 {1; 2}) ]
  owi: [INFO] running instr : br_if 1
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=6,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(b:22)> -> {false}; <(b:23)> -> {true};
                                  <(B:17)> -> {1; 2}
                                  <(B:18)> -> {1; 2}
                                  <(B:21)> -> [1..0xFFFFFFFF]
                                  }
                 stack  : 
                 locals : i32 {1; 2};
                 i32 {1; 2}
  owi: [DEBUG] abstract state : 
                 context: Context{id=7,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false}; <(b:22)> -> {true};
                                  <(b:23)> -> {true;false};
                                  <(B:17)> -> {1; 2}
                                  <(B:18)> -> {1; 2}
                                  <(B:21)> -> {0}
                                  }
                 stack  : 
                 locals : i32 {1; 2};
                 i32 {1; 2}
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {1; 2});  (1 -> i32 {1; 2}) ]
  owi: [INFO] running instr : local.get 1
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=6,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(b:22)> -> {false}; <(b:23)> -> {true};
                                  <(B:17)> -> {1; 2}
                                  <(B:18)> -> {1; 2}
                                  <(B:21)> -> [1..0xFFFFFFFF]
                                  }
                 stack  : 
                 locals : i32 {1; 2};
                 i32 {1; 2}
  owi: [DEBUG] abstract state : 
                 context: Context{id=7,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false}; <(b:22)> -> {true};
                                  <(b:23)> -> {true;false};
                                  <(B:17)> -> {1; 2}
                                  <(B:18)> -> {1; 2}
                                  <(B:21)> -> {0}
                                  }
                 stack  : i32 {1; 2}
                 locals : i32 {1; 2};
                 i32 {1; 2}
  owi: [INFO] stack         : [ i32 {1; 2} ]
  owi: [INFO] locals        : [ (0 -> i32 {1; 2});  (1 -> i32 {1; 2}) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=6,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(b:22)> -> {false}; <(b:23)> -> {true};
                                  <(B:17)> -> {1; 2}
                                  <(B:18)> -> {1; 2}
                                  <(B:21)> -> [1..0xFFFFFFFF]
                                  }
                 stack  : 
                 locals : i32 {1; 2};
                 i32 {1; 2}
  owi: [DEBUG] abstract state : 
                 context: Context{id=7,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false}; <(b:22)> -> {true};
                                  <(b:23)> -> {true;false};
                                  <(B:17)> -> {1; 2}
                                  <(B:18)> -> {1; 2}
                                  <(B:21)> -> {0}
                                  }
                 stack  : i32 {1; 2} ; i32 {1; 2}
                 locals : i32 {1; 2};
                 i32 {1; 2}
  owi: [INFO] stack         : [ i32 {1; 2} ; i32 {1; 2} ]
  owi: [INFO] locals        : [ (0 -> i32 {1; 2});  (1 -> i32 {1; 2}) ]
  owi: [INFO] running instr : i32.mul
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=6,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(b:22)> -> {false}; <(b:23)> -> {true};
                                  <(B:17)> -> {1; 2}
                                  <(B:18)> -> {1; 2}
                                  <(B:21)> -> [1..0xFFFFFFFF]
                                  }
                 stack  : 
                 locals : i32 {1; 2};
                 i32 {1; 2}
  owi: [DEBUG] abstract state : 
                 context: Context{id=7,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false}; <(b:22)> -> {true};
                                  <(b:23)> -> {true;false};
                                  <(B:17)> -> {1; 2}
                                  <(B:18)> -> {1; 2}
                                  <(B:21)> -> {0}
                                  <(B:24)> -> {1; 2; 4}
                                  }
                 stack  : i32 {1; 2; 4}
                 locals : i32 {1; 2};
                 i32 {1; 2}
  owi: [INFO] stack         : [ i32 {1; 2; 4} ]
  owi: [INFO] locals        : [ (0 -> i32 {1; 2});  (1 -> i32 {1; 2}) ]
  owi: [INFO] running instr : local.set 1
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=6,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(b:22)> -> {false}; <(b:23)> -> {true};
                                  <(B:17)> -> {1; 2}
                                  <(B:18)> -> {1; 2}
                                  <(B:21)> -> [1..0xFFFFFFFF]
                                  }
                 stack  : 
                 locals : i32 {1; 2};
                 i32 {1; 2}
  owi: [DEBUG] abstract state : 
                 context: Context{id=7,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false}; <(b:22)> -> {true};
                                  <(b:23)> -> {true;false};
                                  <(B:17)> -> {1; 2}
                                  <(B:18)> -> {1; 2}
                                  <(B:21)> -> {0}
                                  <(B:24)> -> {1; 2; 4}
                                  }
                 stack  : 
                 locals : i32 {1; 2};
                 i32 {1; 2; 4}
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {1; 2});  (1 -> i32 {1; 2; 4}) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=6,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(b:22)> -> {false}; <(b:23)> -> {true};
                                  <(B:17)> -> {1; 2}
                                  <(B:18)> -> {1; 2}
                                  <(B:21)> -> [1..0xFFFFFFFF]
                                  }
                 stack  : 
                 locals : i32 {1; 2};
                 i32 {1; 2}
  owi: [DEBUG] abstract state : 
                 context: Context{id=7,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false}; <(b:22)> -> {true};
                                  <(b:23)> -> {true;false};
                                  <(B:17)> -> {1; 2}
                                  <(B:18)> -> {1; 2}
                                  <(B:21)> -> {0}
                                  <(B:24)> -> {1; 2; 4}
                                  }
                 stack  : i32 {1; 2}
                 locals : i32 {1; 2};
                 i32 {1; 2; 4}
  owi: [INFO] stack         : [ i32 {1; 2} ]
  owi: [INFO] locals        : [ (0 -> i32 {1; 2});  (1 -> i32 {1; 2; 4}) ]
  owi: [INFO] running instr : i32.const 1
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=6,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(b:22)> -> {false}; <(b:23)> -> {true};
                                  <(B:17)> -> {1; 2}
                                  <(B:18)> -> {1; 2}
                                  <(B:21)> -> [1..0xFFFFFFFF]
                                  }
                 stack  : 
                 locals : i32 {1; 2};
                 i32 {1; 2}
  owi: [DEBUG] abstract state : 
                 context: Context{id=7,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false}; <(b:22)> -> {true};
                                  <(b:23)> -> {true;false};
                                  <(B:17)> -> {1; 2}
                                  <(B:18)> -> {1; 2}
                                  <(B:21)> -> {0}
                                  <(B:24)> -> {1; 2; 4}
                                  }
                 stack  : i32 {1} ; i32 {1; 2}
                 locals : i32 {1; 2};
                 i32 {1; 2; 4}
  owi: [INFO] stack         : [ i32 {1} ; i32 {1; 2} ]
  owi: [INFO] locals        : [ (0 -> i32 {1; 2});  (1 -> i32 {1; 2; 4}) ]
  owi: [INFO] running instr : i32.sub
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=6,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(b:22)> -> {false}; <(b:23)> -> {true};
                                  <(B:17)> -> {1; 2}
                                  <(B:18)> -> {1; 2}
                                  <(B:21)> -> [1..0xFFFFFFFF]
                                  }
                 stack  : 
                 locals : i32 {1; 2};
                 i32 {1; 2}
  owi: [DEBUG] abstract state : 
                 context: Context{id=7,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false}; <(b:22)> -> {true};
                                  <(b:23)> -> {true;false};
                                  <(B:17)> -> {1; 2}
                                  <(B:18)> -> {1; 2}
                                  <(B:21)> -> {0}
                                  <(B:24)> -> {1; 2; 4}
                                  <(B:25)> -> {0; 1}
                                  }
                 stack  : i32 {0; 1}
                 locals : i32 {1; 2};
                 i32 {1; 2; 4}
  owi: [INFO] stack         : [ i32 {0; 1} ]
  owi: [INFO] locals        : [ (0 -> i32 {1; 2});  (1 -> i32 {1; 2; 4}) ]
  owi: [INFO] running instr : local.set 0
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=6,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(b:22)> -> {false}; <(b:23)> -> {true};
                                  <(B:17)> -> {1; 2}
                                  <(B:18)> -> {1; 2}
                                  <(B:21)> -> [1..0xFFFFFFFF]
                                  }
                 stack  : 
                 locals : i32 {1; 2};
                 i32 {1; 2}
  owi: [DEBUG] abstract state : 
                 context: Context{id=7,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false}; <(b:22)> -> {true};
                                  <(b:23)> -> {true;false};
                                  <(B:17)> -> {1; 2}
                                  <(B:18)> -> {1; 2}
                                  <(B:21)> -> {0}
                                  <(B:24)> -> {1; 2; 4}
                                  <(B:25)> -> {0; 1}
                                  }
                 stack  : 
                 locals : i32 {0; 1};
                 i32 {1; 2; 4}
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {0; 1});  (1 -> i32 {1; 2; 4}) ]
  owi: [INFO] running instr : br 0
  owi: [DEBUG] jt            :  0 -> 
                 context: Context{id=7,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false}; <(b:22)> -> {true};
                                  <(b:23)> -> {true;false};
                                  <(B:17)> -> {1; 2}
                                  <(B:18)> -> {1; 2}
                                  <(B:21)> -> {0}
                                  <(B:24)> -> {1; 2; 4}
                                  <(B:25)> -> {0; 1}
                                  }
                 stack  : 
                 locals : i32 {0; 1};
                 i32 {1; 2; 4}
               1 -> 
                 context: Context{id=6,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(b:22)> -> {false}; <(b:23)> -> {true};
                                  <(B:17)> -> {1; 2}
                                  <(B:18)> -> {1; 2}
                                  <(B:21)> -> [1..0xFFFFFFFF]
                                  }
                 stack  : 
                 locals : i32 {1; 2};
                 i32 {1; 2}
  owi: [DEBUG] serializing locals (join) : 
                first : (0 -> i32 {2}); 
               (1 -> i32 {1}) 
                second : (0 -> i32 {0; 1});  (1 -> i32 {1; 2; 4})
  owi: [DEBUG] serializing stacks (join) : 
                first :  
                second : 
  owi: [DEBUG] serializing locals (widen) : 
                first : (0 -> i32 {1; 2}); 
               (1 -> i32 {1; 2}) 
                second : (0 -> i32 {0; 1; 2});  (1 -> i32 {1; 2; 4})
  owi: [DEBUG] serializing stacks (widen) : 
                first :  
                second : 
  owi: [DEBUG] abstract state : 
                 context: Context{id=9,
                                  <(B:17)> -> {0; 1; 2}
                                  <(B:18)> -> [1..0x7FFFFFFF]
                                  }
                 stack  : 
                 locals : i32 {0; 1; 2};
                 i32 [1..0x7FFFFFFF]
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {0; 1; 2}); 
              (1 -> i32 [1..0x7FFFFFFF]) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=9,
                                  <(B:17)> -> {0; 1; 2}
                                  <(B:18)> -> [1..0x7FFFFFFF]
                                  }
                 stack  : i32 {0; 1; 2}
                 locals : i32 {0; 1; 2};
                 i32 [1..0x7FFFFFFF]
  owi: [INFO] stack         : [ i32 {0; 1; 2} ]
  owi: [INFO] locals        : [ (0 -> i32 {0; 1; 2}); 
              (1 -> i32 [1..0x7FFFFFFF]) ]
  owi: [INFO] running instr : i32.const 1
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=9,
                                  <(B:17)> -> {0; 1; 2}
                                  <(B:18)> -> [1..0x7FFFFFFF]
                                  }
                 stack  : i32 {1} ; i32 {0; 1; 2}
                 locals : i32 {0; 1; 2};
                 i32 [1..0x7FFFFFFF]
  owi: [INFO] stack         : [ i32 {1} ; i32 {0; 1; 2} ]
  owi: [INFO] locals        : [ (0 -> i32 {0; 1; 2}); 
              (1 -> i32 [1..0x7FFFFFFF]) ]
  owi: [INFO] running instr : i32.le_s
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=9,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(B:17)> -> {0; 1; 2}
                                  <(B:18)> -> [1..0x7FFFFFFF]
                                  <(B:29)> -> [--..--]
                                  }
                 stack  : i32 [--..--]
                 locals : i32 {0; 1; 2};
                 i32 [1..0x7FFFFFFF]
  owi: [INFO] stack         : [ i32 [--..--] ]
  owi: [INFO] locals        : [ (0 -> i32 {0; 1; 2}); 
              (1 -> i32 [1..0x7FFFFFFF]) ]
  owi: [INFO] running instr : br_if 1
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=12,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(b:30)> -> {false}; <(b:31)> -> {true};
                                  <(B:17)> -> {0; 1; 2}
                                  <(B:18)> -> [1..0x7FFFFFFF]
                                  <(B:29)> -> [1..0xFFFFFFFF]
                                  }
                 stack  : 
                 locals : i32 {0; 1; 2};
                 i32 [1..0x7FFFFFFF]
  owi: [DEBUG] abstract state : 
                 context: Context{id=13,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false}; <(b:30)> -> {true};
                                  <(b:31)> -> {true;false};
                                  <(B:17)> -> {0; 1; 2}
                                  <(B:18)> -> [1..0x7FFFFFFF]
                                  <(B:29)> -> {0}
                                  }
                 stack  : 
                 locals : i32 {0; 1; 2};
                 i32 [1..0x7FFFFFFF]
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {0; 1; 2}); 
              (1 -> i32 [1..0x7FFFFFFF]) ]
  owi: [INFO] running instr : local.get 1
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=12,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(b:30)> -> {false}; <(b:31)> -> {true};
                                  <(B:17)> -> {0; 1; 2}
                                  <(B:18)> -> [1..0x7FFFFFFF]
                                  <(B:29)> -> [1..0xFFFFFFFF]
                                  }
                 stack  : 
                 locals : i32 {0; 1; 2};
                 i32 [1..0x7FFFFFFF]
  owi: [DEBUG] abstract state : 
                 context: Context{id=13,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false}; <(b:30)> -> {true};
                                  <(b:31)> -> {true;false};
                                  <(B:17)> -> {0; 1; 2}
                                  <(B:18)> -> [1..0x7FFFFFFF]
                                  <(B:29)> -> {0}
                                  }
                 stack  : i32 [1..0x7FFFFFFF]
                 locals : i32 {0; 1; 2};
                 i32 [1..0x7FFFFFFF]
  owi: [INFO] stack         : [ i32 [1..0x7FFFFFFF] ]
  owi: [INFO] locals        : [ (0 -> i32 {0; 1; 2}); 
              (1 -> i32 [1..0x7FFFFFFF]) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=12,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(b:30)> -> {false}; <(b:31)> -> {true};
                                  <(B:17)> -> {0; 1; 2}
                                  <(B:18)> -> [1..0x7FFFFFFF]
                                  <(B:29)> -> [1..0xFFFFFFFF]
                                  }
                 stack  : 
                 locals : i32 {0; 1; 2};
                 i32 [1..0x7FFFFFFF]
  owi: [DEBUG] abstract state : 
                 context: Context{id=13,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false}; <(b:30)> -> {true};
                                  <(b:31)> -> {true;false};
                                  <(B:17)> -> {0; 1; 2}
                                  <(B:18)> -> [1..0x7FFFFFFF]
                                  <(B:29)> -> {0}
                                  }
                 stack  : i32 {0; 1; 2} ; i32 [1..0x7FFFFFFF]
                 locals : i32 {0; 1; 2};
                 i32 [1..0x7FFFFFFF]
  owi: [INFO] stack         : [ i32 {0; 1; 2} ; i32 [1..0x7FFFFFFF] ]
  owi: [INFO] locals        : [ (0 -> i32 {0; 1; 2}); 
              (1 -> i32 [1..0x7FFFFFFF]) ]
  owi: [INFO] running instr : i32.mul
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=12,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(b:30)> -> {false}; <(b:31)> -> {true};
                                  <(B:17)> -> {0; 1; 2}
                                  <(B:18)> -> [1..0x7FFFFFFF]
                                  <(B:29)> -> [1..0xFFFFFFFF]
                                  }
                 stack  : 
                 locals : i32 {0; 1; 2};
                 i32 [1..0x7FFFFFFF]
  owi: [DEBUG] abstract state : 
                 context: Context{id=13,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false}; <(b:30)> -> {true};
                                  <(b:31)> -> {true;false};
                                  <(B:17)> -> {0; 1; 2}
                                  <(B:18)> -> [1..0x7FFFFFFF]
                                  <(B:24)> -> [0..0x7FFFFFFF]
                                  <(B:29)> -> {0}
                                  }
                 stack  : i32 [0..0x7FFFFFFF]
                 locals : i32 {0; 1; 2};
                 i32 [1..0x7FFFFFFF]
  owi: [INFO] stack         : [ i32 [0..0x7FFFFFFF] ]
  owi: [INFO] locals        : [ (0 -> i32 {0; 1; 2}); 
              (1 -> i32 [1..0x7FFFFFFF]) ]
  owi: [INFO] running instr : local.set 1
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=12,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(b:30)> -> {false}; <(b:31)> -> {true};
                                  <(B:17)> -> {0; 1; 2}
                                  <(B:18)> -> [1..0x7FFFFFFF]
                                  <(B:29)> -> [1..0xFFFFFFFF]
                                  }
                 stack  : 
                 locals : i32 {0; 1; 2};
                 i32 [1..0x7FFFFFFF]
  owi: [DEBUG] abstract state : 
                 context: Context{id=13,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false}; <(b:30)> -> {true};
                                  <(b:31)> -> {true;false};
                                  <(B:17)> -> {0; 1; 2}
                                  <(B:18)> -> [1..0x7FFFFFFF]
                                  <(B:24)> -> [0..0x7FFFFFFF]
                                  <(B:29)> -> {0}
                                  }
                 stack  : 
                 locals : i32 {0; 1; 2};
                 i32 [0..0x7FFFFFFF]
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {0; 1; 2}); 
              (1 -> i32 [0..0x7FFFFFFF]) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=12,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(b:30)> -> {false}; <(b:31)> -> {true};
                                  <(B:17)> -> {0; 1; 2}
                                  <(B:18)> -> [1..0x7FFFFFFF]
                                  <(B:29)> -> [1..0xFFFFFFFF]
                                  }
                 stack  : 
                 locals : i32 {0; 1; 2};
                 i32 [1..0x7FFFFFFF]
  owi: [DEBUG] abstract state : 
                 context: Context{id=13,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false}; <(b:30)> -> {true};
                                  <(b:31)> -> {true;false};
                                  <(B:17)> -> {0; 1; 2}
                                  <(B:18)> -> [1..0x7FFFFFFF]
                                  <(B:24)> -> [0..0x7FFFFFFF]
                                  <(B:29)> -> {0}
                                  }
                 stack  : i32 {0; 1; 2}
                 locals : i32 {0; 1; 2};
                 i32 [0..0x7FFFFFFF]
  owi: [INFO] stack         : [ i32 {0; 1; 2} ]
  owi: [INFO] locals        : [ (0 -> i32 {0; 1; 2}); 
              (1 -> i32 [0..0x7FFFFFFF]) ]
  owi: [INFO] running instr : i32.const 1
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=12,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(b:30)> -> {false}; <(b:31)> -> {true};
                                  <(B:17)> -> {0; 1; 2}
                                  <(B:18)> -> [1..0x7FFFFFFF]
                                  <(B:29)> -> [1..0xFFFFFFFF]
                                  }
                 stack  : 
                 locals : i32 {0; 1; 2};
                 i32 [1..0x7FFFFFFF]
  owi: [DEBUG] abstract state : 
                 context: Context{id=13,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false}; <(b:30)> -> {true};
                                  <(b:31)> -> {true;false};
                                  <(B:17)> -> {0; 1; 2}
                                  <(B:18)> -> [1..0x7FFFFFFF]
                                  <(B:24)> -> [0..0x7FFFFFFF]
                                  <(B:29)> -> {0}
                                  }
                 stack  : i32 {1} ; i32 {0; 1; 2}
                 locals : i32 {0; 1; 2};
                 i32 [0..0x7FFFFFFF]
  owi: [INFO] stack         : [ i32 {1} ; i32 {0; 1; 2} ]
  owi: [INFO] locals        : [ (0 -> i32 {0; 1; 2}); 
              (1 -> i32 [0..0x7FFFFFFF]) ]
  owi: [INFO] running instr : i32.sub
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=12,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(b:30)> -> {false}; <(b:31)> -> {true};
                                  <(B:17)> -> {0; 1; 2}
                                  <(B:18)> -> [1..0x7FFFFFFF]
                                  <(B:29)> -> [1..0xFFFFFFFF]
                                  }
                 stack  : 
                 locals : i32 {0; 1; 2};
                 i32 [1..0x7FFFFFFF]
  owi: [DEBUG] abstract state : 
                 context: Context{id=13,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false}; <(b:30)> -> {true};
                                  <(b:31)> -> {true;false};
                                  <(B:17)> -> {0; 1; 2}
                                  <(B:18)> -> [1..0x7FFFFFFF]
                                  <(B:24)> -> [0..0x7FFFFFFF]
                                  <(B:25)> -> {-1; 0; 1}
                                  <(B:29)> -> {0}
                                  }
                 stack  : i32 {-1; 0; 1}
                 locals : i32 {0; 1; 2};
                 i32 [0..0x7FFFFFFF]
  owi: [INFO] stack         : [ i32 {-1; 0; 1} ]
  owi: [INFO] locals        : [ (0 -> i32 {0; 1; 2}); 
              (1 -> i32 [0..0x7FFFFFFF]) ]
  owi: [INFO] running instr : local.set 0
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=12,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(b:30)> -> {false}; <(b:31)> -> {true};
                                  <(B:17)> -> {0; 1; 2}
                                  <(B:18)> -> [1..0x7FFFFFFF]
                                  <(B:29)> -> [1..0xFFFFFFFF]
                                  }
                 stack  : 
                 locals : i32 {0; 1; 2};
                 i32 [1..0x7FFFFFFF]
  owi: [DEBUG] abstract state : 
                 context: Context{id=13,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false}; <(b:30)> -> {true};
                                  <(b:31)> -> {true;false};
                                  <(B:17)> -> {0; 1; 2}
                                  <(B:18)> -> [1..0x7FFFFFFF]
                                  <(B:24)> -> [0..0x7FFFFFFF]
                                  <(B:25)> -> {-1; 0; 1}
                                  <(B:29)> -> {0}
                                  }
                 stack  : 
                 locals : i32 {-1; 0; 1};
                 i32 [0..0x7FFFFFFF]
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {-1; 0; 1}); 
              (1 -> i32 [0..0x7FFFFFFF]) ]
  owi: [INFO] running instr : br 0
  owi: [DEBUG] jt            :  0 -> 
                 context: Context{id=13,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false}; <(b:30)> -> {true};
                                  <(b:31)> -> {true;false};
                                  <(B:17)> -> {0; 1; 2}
                                  <(B:18)> -> [1..0x7FFFFFFF]
                                  <(B:24)> -> [0..0x7FFFFFFF]
                                  <(B:25)> -> {-1; 0; 1}
                                  <(B:29)> -> {0}
                                  }
                 stack  : 
                 locals : i32 {-1; 0; 1};
                 i32 [0..0x7FFFFFFF]
               1 -> 
                 context: Context{id=12,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(b:30)> -> {false}; <(b:31)> -> {true};
                                  <(B:17)> -> {0; 1; 2}
                                  <(B:18)> -> [1..0x7FFFFFFF]
                                  <(B:29)> -> [1..0xFFFFFFFF]
                                  }
                 stack  : 
                 locals : i32 {0; 1; 2};
                 i32 [1..0x7FFFFFFF]
  owi: [DEBUG] serializing locals (join) : 
                first : (0 -> i32 {2}); 
               (1 -> i32 {1}) 
                second : (0 -> i32 {-1; 0; 1});  (1 -> i32 [0..0x7FFFFFFF])
  owi: [DEBUG] serializing stacks (join) : 
                first :  
                second : 
  owi: [DEBUG] serializing locals (widen) : 
                first : (0 -> i32 {0; 1; 2}); 
               (1 -> i32 [1..0x7FFFFFFF]) 
                second : (0 -> i32 {-1; 0; 1; 2});  (1 -> i32 [0..0x7FFFFFFF])
  owi: [DEBUG] serializing stacks (widen) : 
                first :  
                second : 
  owi: [DEBUG] abstract state : 
                 context: Context{id=15,
                                  <(B:17)> -> {-1; 0; 1; 2}
                                  <(B:18)> -> [0..0x7FFFFFFF]
                                  }
                 stack  : 
                 locals : i32 {-1; 0; 1; 2};
                 i32 [0..0x7FFFFFFF]
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {-1; 0; 1; 2}); 
              (1 -> i32 [0..0x7FFFFFFF]) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=15,
                                  <(B:17)> -> {-1; 0; 1; 2}
                                  <(B:18)> -> [0..0x7FFFFFFF]
                                  }
                 stack  : i32 {-1; 0; 1; 2}
                 locals : i32 {-1; 0; 1; 2};
                 i32 [0..0x7FFFFFFF]
  owi: [INFO] stack         : [ i32 {-1; 0; 1; 2} ]
  owi: [INFO] locals        : [ (0 -> i32 {-1; 0; 1; 2}); 
              (1 -> i32 [0..0x7FFFFFFF]) ]
  owi: [INFO] running instr : i32.const 1
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=15,
                                  <(B:17)> -> {-1; 0; 1; 2}
                                  <(B:18)> -> [0..0x7FFFFFFF]
                                  }
                 stack  : i32 {1} ; i32 {-1; 0; 1; 2}
                 locals : i32 {-1; 0; 1; 2};
                 i32 [0..0x7FFFFFFF]
  owi: [INFO] stack         : [ i32 {1} ; i32 {-1; 0; 1; 2} ]
  owi: [INFO] locals        : [ (0 -> i32 {-1; 0; 1; 2}); 
              (1 -> i32 [0..0x7FFFFFFF]) ]
  owi: [INFO] running instr : i32.le_s
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=15,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(B:17)> -> {-1; 0; 1; 2}
                                  <(B:18)> -> [0..0x7FFFFFFF]
                                  <(B:35)> -> [--..--]
                                  }
                 stack  : i32 [--..--]
                 locals : i32 {-1; 0; 1; 2};
                 i32 [0..0x7FFFFFFF]
  owi: [INFO] stack         : [ i32 [--..--] ]
  owi: [INFO] locals        : [ (0 -> i32 {-1; 0; 1; 2}); 
              (1 -> i32 [0..0x7FFFFFFF]) ]
  owi: [INFO] running instr : br_if 1
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=18,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(b:36)> -> {false}; <(b:37)> -> {true};
                                  <(B:17)> -> {-1; 0; 1; 2}
                                  <(B:18)> -> [0..0x7FFFFFFF]
                                  <(B:35)> -> [1..0xFFFFFFFF]
                                  }
                 stack  : 
                 locals : i32 {-1; 0; 1; 2};
                 i32 [0..0x7FFFFFFF]
  owi: [DEBUG] abstract state : 
                 context: Context{id=19,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false}; <(b:36)> -> {true};
                                  <(b:37)> -> {true;false};
                                  <(B:17)> -> {-1; 0; 1; 2}
                                  <(B:18)> -> [0..0x7FFFFFFF]
                                  <(B:35)> -> {0}
                                  }
                 stack  : 
                 locals : i32 {-1; 0; 1; 2};
                 i32 [0..0x7FFFFFFF]
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {-1; 0; 1; 2}); 
              (1 -> i32 [0..0x7FFFFFFF]) ]
  owi: [INFO] running instr : local.get 1
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=18,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(b:36)> -> {false}; <(b:37)> -> {true};
                                  <(B:17)> -> {-1; 0; 1; 2}
                                  <(B:18)> -> [0..0x7FFFFFFF]
                                  <(B:35)> -> [1..0xFFFFFFFF]
                                  }
                 stack  : 
                 locals : i32 {-1; 0; 1; 2};
                 i32 [0..0x7FFFFFFF]
  owi: [DEBUG] abstract state : 
                 context: Context{id=19,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false}; <(b:36)> -> {true};
                                  <(b:37)> -> {true;false};
                                  <(B:17)> -> {-1; 0; 1; 2}
                                  <(B:18)> -> [0..0x7FFFFFFF]
                                  <(B:35)> -> {0}
                                  }
                 stack  : i32 [0..0x7FFFFFFF]
                 locals : i32 {-1; 0; 1; 2};
                 i32 [0..0x7FFFFFFF]
  owi: [INFO] stack         : [ i32 [0..0x7FFFFFFF] ]
  owi: [INFO] locals        : [ (0 -> i32 {-1; 0; 1; 2}); 
              (1 -> i32 [0..0x7FFFFFFF]) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=18,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(b:36)> -> {false}; <(b:37)> -> {true};
                                  <(B:17)> -> {-1; 0; 1; 2}
                                  <(B:18)> -> [0..0x7FFFFFFF]
                                  <(B:35)> -> [1..0xFFFFFFFF]
                                  }
                 stack  : 
                 locals : i32 {-1; 0; 1; 2};
                 i32 [0..0x7FFFFFFF]
  owi: [DEBUG] abstract state : 
                 context: Context{id=19,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false}; <(b:36)> -> {true};
                                  <(b:37)> -> {true;false};
                                  <(B:17)> -> {-1; 0; 1; 2}
                                  <(B:18)> -> [0..0x7FFFFFFF]
                                  <(B:35)> -> {0}
                                  }
                 stack  : i32 {-1; 0; 1; 2} ; i32 [0..0x7FFFFFFF]
                 locals : i32 {-1; 0; 1; 2};
                 i32 [0..0x7FFFFFFF]
  owi: [INFO] stack         : [ i32 {-1; 0; 1; 2} ; i32 [0..0x7FFFFFFF] ]
  owi: [INFO] locals        : [ (0 -> i32 {-1; 0; 1; 2}); 
              (1 -> i32 [0..0x7FFFFFFF]) ]
  owi: [INFO] running instr : i32.mul
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=18,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(b:36)> -> {false}; <(b:37)> -> {true};
                                  <(B:17)> -> {-1; 0; 1; 2}
                                  <(B:18)> -> [0..0x7FFFFFFF]
                                  <(B:35)> -> [1..0xFFFFFFFF]
                                  }
                 stack  : 
                 locals : i32 {-1; 0; 1; 2};
                 i32 [0..0x7FFFFFFF]
  owi: [DEBUG] abstract state : 
                 context: Context{id=19,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false}; <(b:36)> -> {true};
                                  <(b:37)> -> {true;false};
                                  <(B:17)> -> {-1; 0; 1; 2}
                                  <(B:18)> -> [0..0x7FFFFFFF]
                                  <(B:24)> -> [-0x7FFFFFFF..0x7FFFFFFF]
                                  <(B:35)> -> {0}
                                  }
                 stack  : i32 [-0x7FFFFFFF..0x7FFFFFFF]
                 locals : i32 {-1; 0; 1; 2};
                 i32 [0..0x7FFFFFFF]
  owi: [INFO] stack         : [ i32 [-0x7FFFFFFF..0x7FFFFFFF] ]
  owi: [INFO] locals        : [ (0 -> i32 {-1; 0; 1; 2}); 
              (1 -> i32 [0..0x7FFFFFFF]) ]
  owi: [INFO] running instr : local.set 1
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=18,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(b:36)> -> {false}; <(b:37)> -> {true};
                                  <(B:17)> -> {-1; 0; 1; 2}
                                  <(B:18)> -> [0..0x7FFFFFFF]
                                  <(B:35)> -> [1..0xFFFFFFFF]
                                  }
                 stack  : 
                 locals : i32 {-1; 0; 1; 2};
                 i32 [0..0x7FFFFFFF]
  owi: [DEBUG] abstract state : 
                 context: Context{id=19,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false}; <(b:36)> -> {true};
                                  <(b:37)> -> {true;false};
                                  <(B:17)> -> {-1; 0; 1; 2}
                                  <(B:18)> -> [0..0x7FFFFFFF]
                                  <(B:24)> -> [-0x7FFFFFFF..0x7FFFFFFF]
                                  <(B:35)> -> {0}
                                  }
                 stack  : 
                 locals : i32 {-1; 0; 1; 2};
                 i32 [-0x7FFFFFFF..0x7FFFFFFF]
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {-1; 0; 1; 2}); 
              (1 -> i32 [-0x7FFFFFFF..0x7FFFFFFF]) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=18,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(b:36)> -> {false}; <(b:37)> -> {true};
                                  <(B:17)> -> {-1; 0; 1; 2}
                                  <(B:18)> -> [0..0x7FFFFFFF]
                                  <(B:35)> -> [1..0xFFFFFFFF]
                                  }
                 stack  : 
                 locals : i32 {-1; 0; 1; 2};
                 i32 [0..0x7FFFFFFF]
  owi: [DEBUG] abstract state : 
                 context: Context{id=19,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false}; <(b:36)> -> {true};
                                  <(b:37)> -> {true;false};
                                  <(B:17)> -> {-1; 0; 1; 2}
                                  <(B:18)> -> [0..0x7FFFFFFF]
                                  <(B:24)> -> [-0x7FFFFFFF..0x7FFFFFFF]
                                  <(B:35)> -> {0}
                                  }
                 stack  : i32 {-1; 0; 1; 2}
                 locals : i32 {-1; 0; 1; 2};
                 i32 [-0x7FFFFFFF..0x7FFFFFFF]
  owi: [INFO] stack         : [ i32 {-1; 0; 1; 2} ]
  owi: [INFO] locals        : [ (0 -> i32 {-1; 0; 1; 2}); 
              (1 -> i32 [-0x7FFFFFFF..0x7FFFFFFF]) ]
  owi: [INFO] running instr : i32.const 1
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=18,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(b:36)> -> {false}; <(b:37)> -> {true};
                                  <(B:17)> -> {-1; 0; 1; 2}
                                  <(B:18)> -> [0..0x7FFFFFFF]
                                  <(B:35)> -> [1..0xFFFFFFFF]
                                  }
                 stack  : 
                 locals : i32 {-1; 0; 1; 2};
                 i32 [0..0x7FFFFFFF]
  owi: [DEBUG] abstract state : 
                 context: Context{id=19,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false}; <(b:36)> -> {true};
                                  <(b:37)> -> {true;false};
                                  <(B:17)> -> {-1; 0; 1; 2}
                                  <(B:18)> -> [0..0x7FFFFFFF]
                                  <(B:24)> -> [-0x7FFFFFFF..0x7FFFFFFF]
                                  <(B:35)> -> {0}
                                  }
                 stack  : i32 {1} ; i32 {-1; 0; 1; 2}
                 locals : i32 {-1; 0; 1; 2};
                 i32 [-0x7FFFFFFF..0x7FFFFFFF]
  owi: [INFO] stack         : [ i32 {1} ; i32 {-1; 0; 1; 2} ]
  owi: [INFO] locals        : [ (0 -> i32 {-1; 0; 1; 2}); 
              (1 -> i32 [-0x7FFFFFFF..0x7FFFFFFF]) ]
  owi: [INFO] running instr : i32.sub
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=18,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(b:36)> -> {false}; <(b:37)> -> {true};
                                  <(B:17)> -> {-1; 0; 1; 2}
                                  <(B:18)> -> [0..0x7FFFFFFF]
                                  <(B:35)> -> [1..0xFFFFFFFF]
                                  }
                 stack  : 
                 locals : i32 {-1; 0; 1; 2};
                 i32 [0..0x7FFFFFFF]
  owi: [DEBUG] abstract state : 
                 context: Context{id=19,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false}; <(b:36)> -> {true};
                                  <(b:37)> -> {true;false};
                                  <(B:17)> -> {-1; 0; 1; 2}
                                  <(B:18)> -> [0..0x7FFFFFFF]
                                  <(B:24)> -> [-0x7FFFFFFF..0x7FFFFFFF]
                                  <(B:25)> -> {-2; -1; 0; 1}
                                  <(B:35)> -> {0}
                                  }
                 stack  : i32 {-2; -1; 0; 1}
                 locals : i32 {-1; 0; 1; 2};
                 i32 [-0x7FFFFFFF..0x7FFFFFFF]
  owi: [INFO] stack         : [ i32 {-2; -1; 0; 1} ]
  owi: [INFO] locals        : [ (0 -> i32 {-1; 0; 1; 2}); 
              (1 -> i32 [-0x7FFFFFFF..0x7FFFFFFF]) ]
  owi: [INFO] running instr : local.set 0
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=18,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(b:36)> -> {false}; <(b:37)> -> {true};
                                  <(B:17)> -> {-1; 0; 1; 2}
                                  <(B:18)> -> [0..0x7FFFFFFF]
                                  <(B:35)> -> [1..0xFFFFFFFF]
                                  }
                 stack  : 
                 locals : i32 {-1; 0; 1; 2};
                 i32 [0..0x7FFFFFFF]
  owi: [DEBUG] abstract state : 
                 context: Context{id=19,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false}; <(b:36)> -> {true};
                                  <(b:37)> -> {true;false};
                                  <(B:17)> -> {-1; 0; 1; 2}
                                  <(B:18)> -> [0..0x7FFFFFFF]
                                  <(B:24)> -> [-0x7FFFFFFF..0x7FFFFFFF]
                                  <(B:25)> -> {-2; -1; 0; 1}
                                  <(B:35)> -> {0}
                                  }
                 stack  : 
                 locals : i32 {-2; -1; 0; 1};
                 i32 [-0x7FFFFFFF..0x7FFFFFFF]
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {-2; -1; 0; 1}); 
              (1 -> i32 [-0x7FFFFFFF..0x7FFFFFFF]) ]
  owi: [INFO] running instr : br 0
  owi: [DEBUG] jt            :  0 -> 
                 context: Context{id=19,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false}; <(b:36)> -> {true};
                                  <(b:37)> -> {true;false};
                                  <(B:17)> -> {-1; 0; 1; 2}
                                  <(B:18)> -> [0..0x7FFFFFFF]
                                  <(B:24)> -> [-0x7FFFFFFF..0x7FFFFFFF]
                                  <(B:25)> -> {-2; -1; 0; 1}
                                  <(B:35)> -> {0}
                                  }
                 stack  : 
                 locals : i32 {-2; -1; 0; 1};
                 i32 [-0x7FFFFFFF..0x7FFFFFFF]
               1 -> 
                 context: Context{id=18,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(b:36)> -> {false}; <(b:37)> -> {true};
                                  <(B:17)> -> {-1; 0; 1; 2}
                                  <(B:18)> -> [0..0x7FFFFFFF]
                                  <(B:35)> -> [1..0xFFFFFFFF]
                                  }
                 stack  : 
                 locals : i32 {-1; 0; 1; 2};
                 i32 [0..0x7FFFFFFF]
  owi: [DEBUG] serializing locals (join) : 
                first : (0 -> i32 {2}); 
               (1 -> i32 {1}) 
                second : (0 -> i32 {-2; -1; 0; 1}); 
               (1 -> i32 [-0x7FFFFFFF..0x7FFFFFFF])
  owi: [DEBUG] serializing stacks (join) : 
                first :  
                second : 
  owi: [DEBUG] serializing locals (widen) : 
                first : (0 -> i32 {-1; 0; 1; 2}); 
               (1 -> i32 [0..0x7FFFFFFF]) 
                second : (0 -> i32 {-2; -1; 0; 1; 2}); 
               (1 -> i32 [-0x7FFFFFFF..0x7FFFFFFF])
  owi: [DEBUG] serializing stacks (widen) : 
                first :  
                second : 
  owi: [DEBUG] abstract state : 
                 context: Context{id=21,
                                  <(B:17)> -> [-0x80000000..2]
                                  <(B:18)> -> [--..--]
                                  }
                 stack  : 
                 locals : i32 [-0x80000000..2];
                 i32 [--..--]
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 [-0x80000000..2]); 
              (1 -> i32 [--..--]) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=21,
                                  <(B:17)> -> [-0x80000000..2]
                                  <(B:18)> -> [--..--]
                                  }
                 stack  : i32 [-0x80000000..2]
                 locals : i32 [-0x80000000..2];
                 i32 [--..--]
  owi: [INFO] stack         : [ i32 [-0x80000000..2] ]
  owi: [INFO] locals        : [ (0 -> i32 [-0x80000000..2]); 
              (1 -> i32 [--..--]) ]
  owi: [INFO] running instr : i32.const 1
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=21,
                                  <(B:17)> -> [-0x80000000..2]
                                  <(B:18)> -> [--..--]
                                  }
                 stack  : i32 {1} ; i32 [-0x80000000..2]
                 locals : i32 [-0x80000000..2];
                 i32 [--..--]
  owi: [INFO] stack         : [ i32 {1} ; i32 [-0x80000000..2] ]
  owi: [INFO] locals        : [ (0 -> i32 [-0x80000000..2]); 
              (1 -> i32 [--..--]) ]
  owi: [INFO] running instr : i32.le_s
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=21,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(B:17)> -> [-0x80000000..2]
                                  <(B:18)> -> [--..--]
                                  <(B:41)> -> [--..--]
                                  }
                 stack  : i32 [--..--]
                 locals : i32 [-0x80000000..2];
                 i32 [--..--]
  owi: [INFO] stack         : [ i32 [--..--] ]
  owi: [INFO] locals        : [ (0 -> i32 [-0x80000000..2]); 
              (1 -> i32 [--..--]) ]
  owi: [INFO] running instr : br_if 1
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=24,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(b:42)> -> {false}; <(b:43)> -> {true};
                                  <(B:17)> -> [-0x80000000..2]
                                  <(B:18)> -> [--..--]
                                  <(B:41)> -> [1..0xFFFFFFFF]
                                  }
                 stack  : 
                 locals : i32 [-0x80000000..2];
                 i32 [--..--]
  owi: [DEBUG] abstract state : 
                 context: Context{id=25,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false}; <(b:42)> -> {true};
                                  <(b:43)> -> {true;false};
                                  <(B:17)> -> [-0x80000000..2]
                                  <(B:18)> -> [--..--]
                                  <(B:41)> -> {0}
                                  }
                 stack  : 
                 locals : i32 [-0x80000000..2];
                 i32 [--..--]
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 [-0x80000000..2]); 
              (1 -> i32 [--..--]) ]
  owi: [INFO] running instr : local.get 1
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=24,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(b:42)> -> {false}; <(b:43)> -> {true};
                                  <(B:17)> -> [-0x80000000..2]
                                  <(B:18)> -> [--..--]
                                  <(B:41)> -> [1..0xFFFFFFFF]
                                  }
                 stack  : 
                 locals : i32 [-0x80000000..2];
                 i32 [--..--]
  owi: [DEBUG] abstract state : 
                 context: Context{id=25,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false}; <(b:42)> -> {true};
                                  <(b:43)> -> {true;false};
                                  <(B:17)> -> [-0x80000000..2]
                                  <(B:18)> -> [--..--]
                                  <(B:41)> -> {0}
                                  }
                 stack  : i32 [--..--]
                 locals : i32 [-0x80000000..2];
                 i32 [--..--]
  owi: [INFO] stack         : [ i32 [--..--] ]
  owi: [INFO] locals        : [ (0 -> i32 [-0x80000000..2]); 
              (1 -> i32 [--..--]) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=24,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(b:42)> -> {false}; <(b:43)> -> {true};
                                  <(B:17)> -> [-0x80000000..2]
                                  <(B:18)> -> [--..--]
                                  <(B:41)> -> [1..0xFFFFFFFF]
                                  }
                 stack  : 
                 locals : i32 [-0x80000000..2];
                 i32 [--..--]
  owi: [DEBUG] abstract state : 
                 context: Context{id=25,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false}; <(b:42)> -> {true};
                                  <(b:43)> -> {true;false};
                                  <(B:17)> -> [-0x80000000..2]
                                  <(B:18)> -> [--..--]
                                  <(B:41)> -> {0}
                                  }
                 stack  : i32 [-0x80000000..2] ; i32 [--..--]
                 locals : i32 [-0x80000000..2];
                 i32 [--..--]
  owi: [INFO] stack         : [ i32 [-0x80000000..2] ; i32 [--..--] ]
  owi: [INFO] locals        : [ (0 -> i32 [-0x80000000..2]); 
              (1 -> i32 [--..--]) ]
  owi: [INFO] running instr : i32.mul
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=24,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(b:42)> -> {false}; <(b:43)> -> {true};
                                  <(B:17)> -> [-0x80000000..2]
                                  <(B:18)> -> [--..--]
                                  <(B:41)> -> [1..0xFFFFFFFF]
                                  }
                 stack  : 
                 locals : i32 [-0x80000000..2];
                 i32 [--..--]
  owi: [DEBUG] abstract state : 
                 context: Context{id=25,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false}; <(b:42)> -> {true};
                                  <(b:43)> -> {true;false};
                                  <(B:17)> -> [-0x80000000..2]
                                  <(B:18)> -> [--..--]
                                  <(B:24)> -> [--..--]
                                  <(B:41)> -> {0}
                                  }
                 stack  : i32 [--..--]
                 locals : i32 [-0x80000000..2];
                 i32 [--..--]
  owi: [INFO] stack         : [ i32 [--..--] ]
  owi: [INFO] locals        : [ (0 -> i32 [-0x80000000..2]); 
              (1 -> i32 [--..--]) ]
  owi: [INFO] running instr : local.set 1
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=24,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(b:42)> -> {false}; <(b:43)> -> {true};
                                  <(B:17)> -> [-0x80000000..2]
                                  <(B:18)> -> [--..--]
                                  <(B:41)> -> [1..0xFFFFFFFF]
                                  }
                 stack  : 
                 locals : i32 [-0x80000000..2];
                 i32 [--..--]
  owi: [DEBUG] abstract state : 
                 context: Context{id=25,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false}; <(b:42)> -> {true};
                                  <(b:43)> -> {true;false};
                                  <(B:17)> -> [-0x80000000..2]
                                  <(B:18)> -> [--..--]
                                  <(B:24)> -> [--..--]
                                  <(B:41)> -> {0}
                                  }
                 stack  : 
                 locals : i32 [-0x80000000..2];
                 i32 [--..--]
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 [-0x80000000..2]); 
              (1 -> i32 [--..--]) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=24,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(b:42)> -> {false}; <(b:43)> -> {true};
                                  <(B:17)> -> [-0x80000000..2]
                                  <(B:18)> -> [--..--]
                                  <(B:41)> -> [1..0xFFFFFFFF]
                                  }
                 stack  : 
                 locals : i32 [-0x80000000..2];
                 i32 [--..--]
  owi: [DEBUG] abstract state : 
                 context: Context{id=25,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false}; <(b:42)> -> {true};
                                  <(b:43)> -> {true;false};
                                  <(B:17)> -> [-0x80000000..2]
                                  <(B:18)> -> [--..--]
                                  <(B:24)> -> [--..--]
                                  <(B:41)> -> {0}
                                  }
                 stack  : i32 [-0x80000000..2]
                 locals : i32 [-0x80000000..2];
                 i32 [--..--]
  owi: [INFO] stack         : [ i32 [-0x80000000..2] ]
  owi: [INFO] locals        : [ (0 -> i32 [-0x80000000..2]); 
              (1 -> i32 [--..--]) ]
  owi: [INFO] running instr : i32.const 1
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=24,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(b:42)> -> {false}; <(b:43)> -> {true};
                                  <(B:17)> -> [-0x80000000..2]
                                  <(B:18)> -> [--..--]
                                  <(B:41)> -> [1..0xFFFFFFFF]
                                  }
                 stack  : 
                 locals : i32 [-0x80000000..2];
                 i32 [--..--]
  owi: [DEBUG] abstract state : 
                 context: Context{id=25,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false}; <(b:42)> -> {true};
                                  <(b:43)> -> {true;false};
                                  <(B:17)> -> [-0x80000000..2]
                                  <(B:18)> -> [--..--]
                                  <(B:24)> -> [--..--]
                                  <(B:41)> -> {0}
                                  }
                 stack  : i32 {1} ; i32 [-0x80000000..2]
                 locals : i32 [-0x80000000..2];
                 i32 [--..--]
  owi: [INFO] stack         : [ i32 {1} ; i32 [-0x80000000..2] ]
  owi: [INFO] locals        : [ (0 -> i32 [-0x80000000..2]); 
              (1 -> i32 [--..--]) ]
  owi: [INFO] running instr : i32.sub
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=24,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(b:42)> -> {false}; <(b:43)> -> {true};
                                  <(B:17)> -> [-0x80000000..2]
                                  <(B:18)> -> [--..--]
                                  <(B:41)> -> [1..0xFFFFFFFF]
                                  }
                 stack  : 
                 locals : i32 [-0x80000000..2];
                 i32 [--..--]
  owi: [DEBUG] abstract state : 
                 context: Context{id=25,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false}; <(b:42)> -> {true};
                                  <(b:43)> -> {true;false};
                                  <(B:17)> -> [-0x80000000..2]
                                  <(B:18)> -> [--..--]
                                  <(B:24)> -> [--..--]
                                  <(B:25)> -> [--..--]
                                  <(B:41)> -> {0}
                                  }
                 stack  : i32 [--..--]
                 locals : i32 [-0x80000000..2];
                 i32 [--..--]
  owi: [INFO] stack         : [ i32 [--..--] ]
  owi: [INFO] locals        : [ (0 -> i32 [-0x80000000..2]); 
              (1 -> i32 [--..--]) ]
  owi: [INFO] running instr : local.set 0
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=24,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(b:42)> -> {false}; <(b:43)> -> {true};
                                  <(B:17)> -> [-0x80000000..2]
                                  <(B:18)> -> [--..--]
                                  <(B:41)> -> [1..0xFFFFFFFF]
                                  }
                 stack  : 
                 locals : i32 [-0x80000000..2];
                 i32 [--..--]
  owi: [DEBUG] abstract state : 
                 context: Context{id=25,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false}; <(b:42)> -> {true};
                                  <(b:43)> -> {true;false};
                                  <(B:17)> -> [-0x80000000..2]
                                  <(B:18)> -> [--..--]
                                  <(B:24)> -> [--..--]
                                  <(B:25)> -> [--..--]
                                  <(B:41)> -> {0}
                                  }
                 stack  : 
                 locals : i32 [--..--];
                 i32 [--..--]
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]) ]
  owi: [INFO] running instr : br 0
  owi: [DEBUG] jt            :  0 -> 
                 context: Context{id=25,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false}; <(b:42)> -> {true};
                                  <(b:43)> -> {true;false};
                                  <(B:17)> -> [-0x80000000..2]
                                  <(B:18)> -> [--..--]
                                  <(B:24)> -> [--..--]
                                  <(B:25)> -> [--..--]
                                  <(B:41)> -> {0}
                                  }
                 stack  : 
                 locals : i32 [--..--];
                 i32 [--..--]
               1 -> 
                 context: Context{id=24,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(b:42)> -> {false}; <(b:43)> -> {true};
                                  <(B:17)> -> [-0x80000000..2]
                                  <(B:18)> -> [--..--]
                                  <(B:41)> -> [1..0xFFFFFFFF]
                                  }
                 stack  : 
                 locals : i32 [-0x80000000..2];
                 i32 [--..--]
  owi: [DEBUG] serializing locals (join) : 
                first : (0 -> i32 {2}); 
               (1 -> i32 {1}) 
                second : (0 -> i32 [--..--]);  (1 -> i32 [--..--])
  owi: [DEBUG] serializing stacks (join) : 
                first :  
                second : 
  owi: [DEBUG] serializing locals (widen) : 
                first : (0 -> i32 [-0x80000000..2]); 
               (1 -> i32 [--..--]) 
                second : (0 -> i32 [--..--]);  (1 -> i32 [--..--])
  owi: [DEBUG] serializing stacks (widen) : 
                first :  
                second : 
  owi: [DEBUG] abstract state : 
                 context: Context{id=27,
                                  <(B:17)> -> [--..--]
                                  <(B:18)> -> [--..--]
                                  }
                 stack  : 
                 locals : i32 [--..--];
                 i32 [--..--]
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=27,
                                  <(B:17)> -> [--..--]
                                  <(B:18)> -> [--..--]
                                  }
                 stack  : i32 [--..--]
                 locals : i32 [--..--];
                 i32 [--..--]
  owi: [INFO] stack         : [ i32 [--..--] ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]) ]
  owi: [INFO] running instr : i32.const 1
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=27,
                                  <(B:17)> -> [--..--]
                                  <(B:18)> -> [--..--]
                                  }
                 stack  : i32 {1} ; i32 [--..--]
                 locals : i32 [--..--];
                 i32 [--..--]
  owi: [INFO] stack         : [ i32 {1} ; i32 [--..--] ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]) ]
  owi: [INFO] running instr : i32.le_s
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=27,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(B:17)> -> [--..--]
                                  <(B:18)> -> [--..--]
                                  <(B:47)> -> [--..--]
                                  }
                 stack  : i32 [--..--]
                 locals : i32 [--..--];
                 i32 [--..--]
  owi: [INFO] stack         : [ i32 [--..--] ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]) ]
  owi: [INFO] running instr : br_if 1
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=30,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(b:48)> -> {false}; <(b:49)> -> {true};
                                  <(B:17)> -> [--..--]
                                  <(B:18)> -> [--..--]
                                  <(B:47)> -> [1..0xFFFFFFFF]
                                  }
                 stack  : 
                 locals : i32 [--..--];
                 i32 [--..--]
  owi: [DEBUG] abstract state : 
                 context: Context{id=31,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false}; <(b:48)> -> {true};
                                  <(b:49)> -> {true;false};
                                  <(B:17)> -> [--..--]
                                  <(B:18)> -> [--..--]
                                  <(B:47)> -> {0}
                                  }
                 stack  : 
                 locals : i32 [--..--];
                 i32 [--..--]
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]) ]
  owi: [INFO] running instr : local.get 1
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=30,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(b:48)> -> {false}; <(b:49)> -> {true};
                                  <(B:17)> -> [--..--]
                                  <(B:18)> -> [--..--]
                                  <(B:47)> -> [1..0xFFFFFFFF]
                                  }
                 stack  : 
                 locals : i32 [--..--];
                 i32 [--..--]
  owi: [DEBUG] abstract state : 
                 context: Context{id=31,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false}; <(b:48)> -> {true};
                                  <(b:49)> -> {true;false};
                                  <(B:17)> -> [--..--]
                                  <(B:18)> -> [--..--]
                                  <(B:47)> -> {0}
                                  }
                 stack  : i32 [--..--]
                 locals : i32 [--..--];
                 i32 [--..--]
  owi: [INFO] stack         : [ i32 [--..--] ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=30,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(b:48)> -> {false}; <(b:49)> -> {true};
                                  <(B:17)> -> [--..--]
                                  <(B:18)> -> [--..--]
                                  <(B:47)> -> [1..0xFFFFFFFF]
                                  }
                 stack  : 
                 locals : i32 [--..--];
                 i32 [--..--]
  owi: [DEBUG] abstract state : 
                 context: Context{id=31,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false}; <(b:48)> -> {true};
                                  <(b:49)> -> {true;false};
                                  <(B:17)> -> [--..--]
                                  <(B:18)> -> [--..--]
                                  <(B:47)> -> {0}
                                  }
                 stack  : i32 [--..--] ; i32 [--..--]
                 locals : i32 [--..--];
                 i32 [--..--]
  owi: [INFO] stack         : [ i32 [--..--] ; i32 [--..--] ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]) ]
  owi: [INFO] running instr : i32.mul
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=30,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(b:48)> -> {false}; <(b:49)> -> {true};
                                  <(B:17)> -> [--..--]
                                  <(B:18)> -> [--..--]
                                  <(B:47)> -> [1..0xFFFFFFFF]
                                  }
                 stack  : 
                 locals : i32 [--..--];
                 i32 [--..--]
  owi: [DEBUG] abstract state : 
                 context: Context{id=31,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false}; <(b:48)> -> {true};
                                  <(b:49)> -> {true;false};
                                  <(B:17)> -> [--..--]
                                  <(B:18)> -> [--..--]
                                  <(B:24)> -> [--..--]
                                  <(B:47)> -> {0}
                                  }
                 stack  : i32 [--..--]
                 locals : i32 [--..--];
                 i32 [--..--]
  owi: [INFO] stack         : [ i32 [--..--] ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]) ]
  owi: [INFO] running instr : local.set 1
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=30,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(b:48)> -> {false}; <(b:49)> -> {true};
                                  <(B:17)> -> [--..--]
                                  <(B:18)> -> [--..--]
                                  <(B:47)> -> [1..0xFFFFFFFF]
                                  }
                 stack  : 
                 locals : i32 [--..--];
                 i32 [--..--]
  owi: [DEBUG] abstract state : 
                 context: Context{id=31,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false}; <(b:48)> -> {true};
                                  <(b:49)> -> {true;false};
                                  <(B:17)> -> [--..--]
                                  <(B:18)> -> [--..--]
                                  <(B:24)> -> [--..--]
                                  <(B:47)> -> {0}
                                  }
                 stack  : 
                 locals : i32 [--..--];
                 i32 [--..--]
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=30,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(b:48)> -> {false}; <(b:49)> -> {true};
                                  <(B:17)> -> [--..--]
                                  <(B:18)> -> [--..--]
                                  <(B:47)> -> [1..0xFFFFFFFF]
                                  }
                 stack  : 
                 locals : i32 [--..--];
                 i32 [--..--]
  owi: [DEBUG] abstract state : 
                 context: Context{id=31,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false}; <(b:48)> -> {true};
                                  <(b:49)> -> {true;false};
                                  <(B:17)> -> [--..--]
                                  <(B:18)> -> [--..--]
                                  <(B:24)> -> [--..--]
                                  <(B:47)> -> {0}
                                  }
                 stack  : i32 [--..--]
                 locals : i32 [--..--];
                 i32 [--..--]
  owi: [INFO] stack         : [ i32 [--..--] ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]) ]
  owi: [INFO] running instr : i32.const 1
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=30,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(b:48)> -> {false}; <(b:49)> -> {true};
                                  <(B:17)> -> [--..--]
                                  <(B:18)> -> [--..--]
                                  <(B:47)> -> [1..0xFFFFFFFF]
                                  }
                 stack  : 
                 locals : i32 [--..--];
                 i32 [--..--]
  owi: [DEBUG] abstract state : 
                 context: Context{id=31,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false}; <(b:48)> -> {true};
                                  <(b:49)> -> {true;false};
                                  <(B:17)> -> [--..--]
                                  <(B:18)> -> [--..--]
                                  <(B:24)> -> [--..--]
                                  <(B:47)> -> {0}
                                  }
                 stack  : i32 {1} ; i32 [--..--]
                 locals : i32 [--..--];
                 i32 [--..--]
  owi: [INFO] stack         : [ i32 {1} ; i32 [--..--] ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]) ]
  owi: [INFO] running instr : i32.sub
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=30,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(b:48)> -> {false}; <(b:49)> -> {true};
                                  <(B:17)> -> [--..--]
                                  <(B:18)> -> [--..--]
                                  <(B:47)> -> [1..0xFFFFFFFF]
                                  }
                 stack  : 
                 locals : i32 [--..--];
                 i32 [--..--]
  owi: [DEBUG] abstract state : 
                 context: Context{id=31,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false}; <(b:48)> -> {true};
                                  <(b:49)> -> {true;false};
                                  <(B:17)> -> [--..--]
                                  <(B:18)> -> [--..--]
                                  <(B:24)> -> [--..--]
                                  <(B:25)> -> [--..--]
                                  <(B:47)> -> {0}
                                  }
                 stack  : i32 [--..--]
                 locals : i32 [--..--];
                 i32 [--..--]
  owi: [INFO] stack         : [ i32 [--..--] ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]) ]
  owi: [INFO] running instr : local.set 0
  owi: [DEBUG] jt            :  1 -> 
                 context: Context{id=30,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(b:48)> -> {false}; <(b:49)> -> {true};
                                  <(B:17)> -> [--..--]
                                  <(B:18)> -> [--..--]
                                  <(B:47)> -> [1..0xFFFFFFFF]
                                  }
                 stack  : 
                 locals : i32 [--..--];
                 i32 [--..--]
  owi: [DEBUG] abstract state : 
                 context: Context{id=31,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false}; <(b:48)> -> {true};
                                  <(b:49)> -> {true;false};
                                  <(B:17)> -> [--..--]
                                  <(B:18)> -> [--..--]
                                  <(B:24)> -> [--..--]
                                  <(B:25)> -> [--..--]
                                  <(B:47)> -> {0}
                                  }
                 stack  : 
                 locals : i32 [--..--];
                 i32 [--..--]
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]) ]
  owi: [INFO] running instr : br 0
  owi: [DEBUG] jt            :  0 -> 
                 context: Context{id=31,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false}; <(b:48)> -> {true};
                                  <(b:49)> -> {true;false};
                                  <(B:17)> -> [--..--]
                                  <(B:18)> -> [--..--]
                                  <(B:24)> -> [--..--]
                                  <(B:25)> -> [--..--]
                                  <(B:47)> -> {0}
                                  }
                 stack  : 
                 locals : i32 [--..--];
                 i32 [--..--]
               1 -> 
                 context: Context{id=30,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(b:48)> -> {false}; <(b:49)> -> {true};
                                  <(B:17)> -> [--..--]
                                  <(B:18)> -> [--..--]
                                  <(B:47)> -> [1..0xFFFFFFFF]
                                  }
                 stack  : 
                 locals : i32 [--..--];
                 i32 [--..--]
  owi: [DEBUG] serializing locals (join) : 
                first : (0 -> i32 {2}); 
               (1 -> i32 {1}) 
                second : (0 -> i32 [--..--]);  (1 -> i32 [--..--])
  owi: [DEBUG] serializing stacks (join) : 
                first :  
                second : 
  owi: [DEBUG] serializing locals (widen) : 
                first : (0 -> i32 [--..--]); 
               (1 -> i32 [--..--]) 
                second : (0 -> i32 [--..--]);  (1 -> i32 [--..--])
  owi: [DEBUG] serializing stacks (widen) : 
                first :  
                second : 
  owi: [DEBUG] jt            :  0 -> 
                 context: Context{id=30,
                                  <(b:19)> -> {true;false};
                                  <(b:20)> -> {true;false};
                                  <(b:48)> -> {false}; <(b:49)> -> {true};
                                  <(B:17)> -> [--..--]
                                  <(B:18)> -> [--..--]
                                  <(B:47)> -> [1..0xFFFFFFFF]
                                  }
                 stack  : 
                 locals : i32 [--..--];
                 i32 [--..--]
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : None 
  
  owi: [DEBUG] jt            :  
