  $ owi abs floats.wat -vv
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
                 func: Local ((func $start
                   f32.const 4
                   f32.const 100
                   f32.sqrt
                   f32.add
                   i32.trunc_f32_s
                   f32.convert_i32_s
                   f64.promote_f32
                   f64.const 4_294_967_296
                   f64.add
                   f64.const 4_294_967_295
                   f64.sub
                   return
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
  owi: [DEBUG] typechecking instr: f32.const 4
  owi: [DEBUG] stack             : [f32]
  owi: [DEBUG] typechecking instr: f32.const 100
  owi: [DEBUG] stack             : [f32 f32]
  owi: [DEBUG] typechecking instr: f32.sqrt
  owi: [DEBUG] stack             : [f32 f32]
  owi: [DEBUG] typechecking instr: f32.add
  owi: [DEBUG] stack             : [f32]
  owi: [DEBUG] typechecking instr: i32.trunc_f32_s
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: f32.convert_i32_s
  owi: [DEBUG] stack             : [f32]
  owi: [DEBUG] typechecking instr: f64.promote_f32
  owi: [DEBUG] stack             : [f64]
  owi: [DEBUG] typechecking instr: f64.const 4_294_967_296
  owi: [DEBUG] stack             : [f64 f64]
  owi: [DEBUG] typechecking instr: f64.add
  owi: [DEBUG] stack             : [f64]
  owi: [DEBUG] typechecking instr: f64.const 4_294_967_295
  owi: [DEBUG] stack             : [f64 f64]
  owi: [DEBUG] typechecking instr: f64.sub
  owi: [DEBUG] stack             : [f64]
  owi: [DEBUG] typechecking instr: return
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
                 locals : 
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : 
                 locals : 
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : f32.const 4
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <(B:8)> -> [--..--]
                                        }
                 stack  : f32 ...
                 locals : 
  owi: [INFO] stack         : [ f32 ... ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : f32.const 100
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <(B:8)> -> [--..--]
                                        <(B:9)> -> [--..--]
                                        }
                 stack  : f32 ... ; f32 ...
                 locals : 
  owi: [INFO] stack         : [ f32 ... ; f32 ... ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : f32.sqrt
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1,
                                  <(B:8)> -> [--..--]
                                  <(B:9)> -> [--..--]
                                  <(B:10)> -> [--..--]
                                  }
                 stack  : f32 ... ; f32 ...
                 locals : 
  owi: [INFO] stack         : [ f32 ... ; f32 ... ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : f32.add
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1,
                                  <(B:8)> -> [--..--]
                                  <(B:9)> -> [--..--]
                                  <(B:10)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  }
                 stack  : f32 ...
                 locals : 
  owi: [INFO] stack         : [ f32 ... ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : i32.trunc_f32_s
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1,
                                  <(B:8)> -> [--..--]
                                  <(B:9)> -> [--..--]
                                  <(B:10)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:12)> -> [--..--]
                                  }
                 stack  : i32 [--..--]
                 locals : 
  owi: [INFO] stack         : [ i32 [--..--] ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : f32.convert_i32_s
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1,
                                  <(B:8)> -> [--..--]
                                  <(B:9)> -> [--..--]
                                  <(B:10)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:12)> -> [--..--]
                                  <(B:13)> -> [--..--]
                                  }
                 stack  : f32 ...
                 locals : 
  owi: [INFO] stack         : [ f32 ... ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : f64.promote_f32
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1,
                                  <(B:8)> -> [--..--]
                                  <(B:9)> -> [--..--]
                                  <(B:10)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:12)> -> [--..--]
                                  <(B:13)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  }
                 stack  : f64 ...
                 locals : 
  owi: [INFO] stack         : [ f64 ... ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : f64.const 4_294_967_296
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1,
                                  <(B:8)> -> [--..--]
                                  <(B:9)> -> [--..--]
                                  <(B:10)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:12)> -> [--..--]
                                  <(B:13)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  <(B:15)> -> [--..--]
                                  }
                 stack  : f64 ... ; f64 ...
                 locals : 
  owi: [INFO] stack         : [ f64 ... ; f64 ... ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : f64.add
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1,
                                  <(B:8)> -> [--..--]
                                  <(B:9)> -> [--..--]
                                  <(B:10)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:12)> -> [--..--]
                                  <(B:13)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  <(B:15)> -> [--..--]
                                  <(B:16)> -> [--..--]
                                  }
                 stack  : f64 ...
                 locals : 
  owi: [INFO] stack         : [ f64 ... ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : f64.const 4_294_967_295
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1,
                                  <(B:8)> -> [--..--]
                                  <(B:9)> -> [--..--]
                                  <(B:10)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:12)> -> [--..--]
                                  <(B:13)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  <(B:15)> -> [--..--]
                                  <(B:16)> -> [--..--]
                                  <(B:17)> -> [--..--]
                                  }
                 stack  : f64 ... ; f64 ...
                 locals : 
  owi: [INFO] stack         : [ f64 ... ; f64 ... ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : f64.sub
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1,
                                  <(B:8)> -> [--..--]
                                  <(B:9)> -> [--..--]
                                  <(B:10)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:12)> -> [--..--]
                                  <(B:13)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  <(B:15)> -> [--..--]
                                  <(B:16)> -> [--..--]
                                  <(B:17)> -> [--..--]
                                  <(B:18)> -> [--..--]
                                  }
                 stack  : f64 ...
                 locals : 
  owi: [INFO] stack         : [ f64 ... ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : return
  owi: [DEBUG] jt            :  ret -> 
                 context: Context{id=1,
                                  <(B:8)> -> [--..--]
                                  <(B:9)> -> [--..--]
                                  <(B:10)> -> [--..--]
                                  <(B:11)> -> [--..--]
                                  <(B:12)> -> [--..--]
                                  <(B:13)> -> [--..--]
                                  <(B:14)> -> [--..--]
                                  <(B:15)> -> [--..--]
                                  <(B:16)> -> [--..--]
                                  <(B:17)> -> [--..--]
                                  <(B:18)> -> [--..--]
                                  }
                 stack  : f64 ...
                 locals : 
  owi: [DEBUG] abstract state : None 
  
  owi: [DEBUG] jt            :  
