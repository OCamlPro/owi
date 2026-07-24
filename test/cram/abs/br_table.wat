(module
  (func $start
    (block $b2
      (block $b1
        (block $b0
          i32.const 1
          (br_table $b0 $b1 $b2))
        i32.const 0
        return
      )
      i32.const 1
      return
    )
    i32.const 2
    return
  )

	(start $start)
)
