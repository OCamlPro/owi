(module
  (func $f
    (loop
      br 0
      br 1
      br 2
      (loop
        br 0
        br 1
        br 2
        br 3
        br 4
      )
    )
  ))
