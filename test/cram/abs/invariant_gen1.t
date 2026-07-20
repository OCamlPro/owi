  $ owi sym --generate-abstract-invariant ./invariant_gen1.wat
  owi: [ERROR] Trap: integer divide by zero
  model {
    symbol symbol_0 i32 4242
  }
  owi: [ERROR] Reached problem!
  [13]
  $ owi abs ./invariant_gen1.wat -v
  owi: [INFO] parsing      ...
  owi: [INFO] checking     ...
  owi: [INFO] typechecking ...
  owi: [INFO] linking      ...
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : call 1
  owi: [INFO] calling func  : func start
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {0});  (1 -> i32 {0}); 
              (2 -> i32 {0});  (3 -> i32 {0});  (4 -> i32 {0}); 
              (5 -> i32 {0});  (6 -> i32 {0}) ]
  owi: [INFO] running instr : call 0
  owi: [INFO] stack         : [ i32 [--..--] ]
  owi: [INFO] locals        : [ (0 -> i32 {0});  (1 -> i32 {0}); 
              (2 -> i32 {0});  (3 -> i32 {0});  (4 -> i32 {0}); 
              (5 -> i32 {0});  (6 -> i32 {0}) ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 {0}); 
              (2 -> i32 {0});  (3 -> i32 {0});  (4 -> i32 {0}); 
              (5 -> i32 {0});  (6 -> i32 {0}) ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32 [--..--] ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 {0}); 
              (2 -> i32 {0});  (3 -> i32 {0});  (4 -> i32 {0}); 
              (5 -> i32 {0});  (6 -> i32 {0}) ]
  owi: [INFO] running instr : i32.const 10
  owi: [INFO] stack         : [ i32 {10} ; i32 [--..--] ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 {0}); 
              (2 -> i32 {0});  (3 -> i32 {0});  (4 -> i32 {0}); 
              (5 -> i32 {0});  (6 -> i32 {0}) ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32 [--..--] ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 {0}); 
              (2 -> i32 {0});  (3 -> i32 {0});  (4 -> i32 {0}); 
              (5 -> i32 {0});  (6 -> i32 {0}) ]
  owi: [INFO] running instr : local.set 1
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 {0});  (3 -> i32 {0});  (4 -> i32 {0}); 
              (5 -> i32 {0});  (6 -> i32 {0}) ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ i32 [--..--] ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 {0});  (3 -> i32 {0});  (4 -> i32 {0}); 
              (5 -> i32 {0});  (6 -> i32 {0}) ]
  owi: [INFO] running instr : i32.const 20
  owi: [INFO] stack         : [ i32 {20} ; i32 [--..--] ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 {0});  (3 -> i32 {0});  (4 -> i32 {0}); 
              (5 -> i32 {0});  (6 -> i32 {0}) ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32 [--..--] ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 {0});  (3 -> i32 {0});  (4 -> i32 {0}); 
              (5 -> i32 {0});  (6 -> i32 {0}) ]
  owi: [INFO] running instr : local.set 2
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {0});  (4 -> i32 {0}); 
              (5 -> i32 {0});  (6 -> i32 {0}) ]
  owi: [INFO] running instr : local.get 2
  owi: [INFO] stack         : [ i32 [--..--] ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {0});  (4 -> i32 {0}); 
              (5 -> i32 {0});  (6 -> i32 {0}) ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32 [--..--] ; i32 [--..--] ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {0});  (4 -> i32 {0}); 
              (5 -> i32 {0});  (6 -> i32 {0}) ]
  owi: [INFO] running instr : i32.sub
  owi: [INFO] stack         : [ i32 {30} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {0});  (4 -> i32 {0}); 
              (5 -> i32 {0});  (6 -> i32 {0}) ]
  owi: [INFO] running instr : local.set 3
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {0}); 
              (5 -> i32 {0});  (6 -> i32 {0}) ]
  owi: [INFO] running instr : local.get 3
  owi: [INFO] stack         : [ i32 {30} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {0}); 
              (5 -> i32 {0});  (6 -> i32 {0}) ]
  owi: [INFO] running instr : i32.const 29
  owi: [INFO] stack         : [ i32 {29} ; i32 {30} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {0}); 
              (5 -> i32 {0});  (6 -> i32 {0}) ]
  owi: [INFO] running instr : i32.sub
  owi: [INFO] stack         : [ i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {0}); 
              (5 -> i32 {0});  (6 -> i32 {0}) ]
  owi: [INFO] running instr : local.set 4
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {0});  (6 -> i32 {0}) ]
  owi: [INFO] running instr : i32.const 10000
  owi: [INFO] stack         : [ i32 {0x2710} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {0});  (6 -> i32 {0}) ]
  owi: [INFO] running instr : local.set 6
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {0});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : i32.const 0
  owi: [INFO] stack         : [ i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {0});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : local.set 5
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {0});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : block $break
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {0});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : loop $loop
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {0});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32 {100} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {0});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : local.get 4
  owi: [INFO] stack         : [ i32 {1} ; i32 {100} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {0});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : i32.div_s
  owi: [INFO] stack         : [ i32 {100} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {0});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : drop
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {0});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : local.get 5
  owi: [INFO] stack         : [ i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {0});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ i32 {1} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {0});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {0});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : local.tee 5
  owi: [INFO] stack         : [ i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {1});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : local.get 6
  owi: [INFO] stack         : [ i32 {0x2710} ; i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {1});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : i32.lt_s
  owi: [INFO] stack         : [ i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {1});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {1});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32 {100} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {1});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : local.get 4
  owi: [INFO] stack         : [ i32 {1} ; i32 {100} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {1});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : i32.div_s
  owi: [INFO] stack         : [ i32 {100} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {1});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : drop
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {1});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : local.get 5
  owi: [INFO] stack         : [ i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {1});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ i32 {1} ; i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {1});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32 {2} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {1});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : local.tee 5
  owi: [INFO] stack         : [ i32 {2} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {2});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : local.get 6
  owi: [INFO] stack         : [ i32 {0x2710} ; i32 {2} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {2});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : i32.lt_s
  owi: [INFO] stack         : [ i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {2});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {2});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32 {100} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {2});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : local.get 4
  owi: [INFO] stack         : [ i32 {1} ; i32 {100} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {2});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : i32.div_s
  owi: [INFO] stack         : [ i32 {100} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {2});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : drop
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {2});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : local.get 5
  owi: [INFO] stack         : [ i32 {2} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {2});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ i32 {1} ; i32 {2} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {2});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32 BottomMod ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 {2});  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : local.tee 5
  owi: [INFO] stack         : [ i32 BottomMod ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 BottomMod);  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : local.get 6
  owi: [INFO] stack         : [ i32 {0x2710} ; i32 BottomMod ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 BottomMod);  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : i32.lt_s
  owi: [INFO] stack         : [ i32 BottomMod ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 [--..--]); 
              (2 -> i32 [--..--]);  (3 -> i32 {30});  (4 -> i32 {1}); 
              (5 -> i32 BottomMod);  (6 -> i32 {0x2710}) ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] Passed division by zero check for expression:(uuid: 24) i32.div_s
  owi: [WARNING] Possible division by zero for expression:(uuid: 47) i32.div_s
