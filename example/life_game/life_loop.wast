(module $life_loop

  (import "life_ext" "sleep" (func $sleep (param i32)))
  (import "life_ext" "init_window" (func $init_window (param i32)))

  (import "life" "life_print_step" (func $life_print_step))

  (func $life_game
    (local $i i32)
    (loop $l
      call $life_print_step
      (call $sleep (i32.const 0))
      (local.tee $i (i32.add (local.get $i) (i32.const 1)))
      i32.const 100
      i32.ne
      br_if $l
    )
  )

  (func $start
    (call $init_window (i32.const 0))
    call $life_game
  )

  (start $start)
)
