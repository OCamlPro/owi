This one is from https://github.com/OCamlPro/owi/issues/1132
  $ owi wasm run file.wat -v
  owi: [INFO] parsing      ...
  owi: [INFO] checking     ...
  owi: [INFO] checking     ...
  owi: [INFO] typechecking ...
  owi: [INFO] linking      ...
  owi: [INFO] interpreting ...
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : call 0 (executed 0 times)
  owi: [INFO] calling func  : func main
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : i32.const 5 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 5 ]
  owi: [INFO] running instr : i32.const -1 (executed 0 times)
  owi: [INFO] stack         : [ i32.const -1 ; i32.const 5 ]
  owi: [INFO] running instr : i32.const 1 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 1 ; i32.const -1 ; i32.const 5 ]
  owi: [INFO] running instr : memory.fill (executed 0 times)
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : i32.const 5 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 5 ]
  owi: [INFO] running instr : i32.load8_u align=1 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 255 ]
  owi: [INFO] running instr : i32.const 255 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 255 ; i32.const 255 ]
  owi: [INFO] running instr : i32.ne (executed 0 times)
  owi: [INFO] stack         : [ i32.const 0 ]
  owi: [INFO] running instr : if (executed 0 times)
