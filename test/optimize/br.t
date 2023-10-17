br instructions:
  $ dune exec -- owi opt br.wast
  (module
    (func $br   
      i32.const 42
      br 0
    )
    (func $br_if   
      br 0
    )
    (func $start   
      call 0
      call 1
    )
    (start 2)
  )
