block loop instructions:
  $ owi opt block_loop.wat > block_loop.opt.wat
  $ cat block_loop.opt.wat
  (module
    
    (type (sub final  (func)))
    (func $start
      
    )
    (start 0)
  )
  $ owi run block_loop.opt.wat
