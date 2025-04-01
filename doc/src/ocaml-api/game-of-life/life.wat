(module $life

  (import "life_ext" "height" (func $height (param i32) (result i32)))
  (import "life_ext" "width" (func $width (param i32) (result i32)))
  (import "life_ext" "newline" (func $newline (param i32)))
  (import "life_ext" "clear_screen" (func $clear_screen (param i32)))
  (import "life_ext" "cell_print" (func $cell_print (param i32)))
  (import "life_ext" "rand_val" (func $rand_val (param i32) (result i32)))

  (memory 0)

  (global $h (mut i32) (i32.const 0))
  (global $w (mut i32) (i32.const 0))
  (global $size (mut i32) (i32.const 0))

  (func $size_init
    (global.set $h (call $height (i32.const 0)))
    (global.set $w (call $width (i32.const 0)))
    (global.set $size (i32.mul (global.get $h) (global.get $w)))
  )

  (func $memory_init
    global.get $size
    i32.const 8       ;; 8 = 4 (bytes) * 2 (arrays)
    i32.mul
    f32.convert_i32_u
    f32.const 65536   ;; memory page size
    f32.div
    f32.ceil
    i32.trunc_f32_u
    memory.grow
    drop
  )

  (func $idx2coords (param $idx i32) (result i32) (result i32)
    (local $div i32)
    local.get $idx
    local.get $idx
    global.get $w
    i32.div_s
    local.tee $div
    global.get $w
    i32.mul
    i32.sub
    local.get $div
  )

  (func $coords2idx (param $x i32) (param $y i32) (result i32)
    local.get $x
    global.get $h
    i32.mul
    local.get $y
    i32.add
  )

  (func $get_cell (param $x i32) (param $y i32) (result i32)
    (call $mem_get (local.get $x) (local.get $y) (i32.const 0))
  )

  (func $mem_get (param $x i32) (param $y i32) (param $t i32) (result i32)
    local.get $t
    (if (result i32)
      (then (i32.add (global.get $size) (call $coords2idx (local.get $x) (local.get $y))))
      (else (call $coords2idx (local.get $x) (local.get $y)))
    )
    i32.const 4
    i32.mul
    i32.load
  )

  (func $mem_set (param $x i32) (param $y i32) (param $t i32) (param $val i32)
    local.get $t
    (if (result i32)
      (then (i32.add (global.get $size) (call $coords2idx (local.get $x) (local.get $y))))
      (else (call $coords2idx (local.get $x) (local.get $y)))
    )
    i32.const 4
    i32.mul
    local.get $val
    i32.store
  )

  (func $mem_set_idx (param $i i32) (param $t i32) (param $val i32)
    (call $mem_set (call $idx2coords (local.get $i)) (local.get $t) (local.get $val))
  )

  (func $mem_init
    (local $i i32)
    (loop $l
      (call $mem_set_idx (local.get $i) (i32.const 0) (call $rand_val (i32.const 5)))
      (local.tee $i (i32.add (local.get $i) (i32.const 1)))
      global.get $size
      i32.ne
      br_if $l
    )
  )

  (func $is_alive (param $x i32) (param $y i32) (result i32)
    (call $mem_get (local.get $x) (local.get $y) (i32.const 0))
    i32.const 1
    i32.eq
  )

  (func $cell_process (param $x i32) (param $y i32) (result i32)
    i32.const 0
    local.get $x
    i32.const 0
    i32.lt_s
    br_if 0
    (i32.ge_s (local.get $x) (global.get $w))
    br_if 0
    local.get $y
    i32.const 0
    i32.lt_s
    br_if 0
    (i32.ge_s (local.get $y) (global.get $h))
    br_if 0
    drop
    (call $is_alive (local.get $x) (local.get $y))
  )

  (func $count_alive (param $x i32) (param $y i32) (result i32)
    (call $cell_process (i32.sub (local.get $x) (i32.const 1)) (i32.sub (local.get $y) (i32.const 1)))
    (call $cell_process (i32.sub (local.get $x) (i32.const 1)) (local.get $y))
    (call $cell_process (i32.sub (local.get $x) (i32.const 1)) (i32.add (local.get $y) (i32.const 1)))
    (call $cell_process (local.get $x) (i32.sub (local.get $y) (i32.const 1)))
    (call $cell_process (local.get $x) (i32.add (local.get $y) (i32.const 1)))
    (call $cell_process (i32.add (local.get $x) (i32.const 1)) (i32.sub (local.get $y) (i32.const 1)))
    (call $cell_process (i32.add (local.get $x) (i32.const 1)) (local.get $y))
    (call $cell_process (i32.add (local.get $x) (i32.const 1)) (i32.add (local.get $y) (i32.const 1)))
    i32.add
    i32.add
    i32.add
    i32.add
    i32.add
    i32.add
    i32.add
  )

  (func $compute_count_alive
    (local $i i32)
    (local.set $i (i32.const 0))
    (loop $l
      (call $idx2coords (local.get $i))
      i32.const 1
      (call $count_alive (call $idx2coords (local.get $i)))
      call $mem_set
      (local.tee $i (i32.add (local.get $i) (i32.const 1)))
      global.get $size
      i32.ne
      br_if $l
    )
  )

  (func $life_step
    (local $nb_alive i32)
    (local $i i32)
    (call $compute_count_alive)
    (loop $l
      (call $mem_get (call $idx2coords (local.get $i)) (i32.const 1))
      local.set $nb_alive
      (call $is_alive (call $idx2coords (local.get $i)))
      (if
        (then
          local.get $nb_alive
          i32.const 2
          i32.eq
          (if
            (then (call $mem_set_idx (local.get $i) (i32.const 0) (i32.const 1)))
            (else
              local.get $nb_alive
              i32.const 3
              i32.eq
              (if
                (then (call $mem_set_idx (local.get $i) (i32.const 0) (i32.const 1)))
                (else (call $mem_set_idx (local.get $i) (i32.const 0) (i32.const 0)))
              )
            )
          )
        )
        (else
          local.get $nb_alive
          i32.const 3
          i32.eq
          (if
            (then (call $mem_set_idx (local.get $i) (i32.const 0) (i32.const 1)))
            (else (call $mem_set_idx (local.get $i) (i32.const 0) (i32.const 0)))
          )
        )
      )
      (local.tee $i (i32.add (local.get $i) (i32.const 1)))
      global.get $size
      i32.ne
      br_if $l
    )
  )

  (func $life_print
    (local $i i32)
    (call $clear_screen (i32.const 0))
    (loop $l
      (call $get_cell (call $idx2coords (local.get $i)))
      call $cell_print
      (local.tee $i (i32.add (local.get $i) (i32.const 1)))
      call $idx2coords
      drop
      i32.eqz
      (if
        (then (call $newline (i32.const 0)))
      )
      local.get $i
      global.get $size
      i32.ne
      br_if $l
    )
  )

  (func $life_print_step (export "life_print_step")
    call $life_print
    call $life_step
  )

  (func $start
    call $size_init
    call $memory_init
    call $mem_init
  )

  (start $start)
)
