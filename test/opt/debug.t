test debug printing:
  $ owi opt drop.wat --debug
  parsing      ...
  checking     ...
  grouping     ...
  assigning    ...
  rewriting    ...
  typechecking ...
  optimizing   ...
  (module
    (type (sub final  (func)))
    (global $g i32 i32.const 0)
    (func $start
      call 1
      call 2
    )
    (func $const
      
    )
    (func $local_global
      
    )
    (start 0)
  )
