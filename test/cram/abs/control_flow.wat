(module
  (func $start (param $mode i32) (param $value i32) (result i32)
    (local $result i32)
    
    ;; br_table to select operation mode
    (block $end
      (block $mode2
        (block $mode1
          (block $mode0
            (br_table $mode0 $mode1 $mode2 $end
              (local.get $mode))
          )
          ;; mode 0: if-then-else to check if value is positive
          (local.get $value)
          (i32.const 0)
          (i32.gt_s)
          (if (result i32)
            (then
              (local.get $value)
              (i32.const 10)
              (i32.add)
            )
            (else
              (i32.const 0)
            )
          )
          (local.set $result)
          (br $end)
        )
        ;; mode 1: loop with br 0 to count down
        (local.get $value)
        (local.set $result)
        (loop $countdown
          (local.get $result)
          (i32.const 0)
          (i32.le_s)
          (if
            (then
              (br 2)  ;; br 0 would break loop, br 2 breaks to $end
            )
          )
          (local.set $result
            (i32.sub (local.get $result) (i32.const 1)))
          (br $countdown)
        )
        (br $end)
      )
      ;; mode 2: multiply by 2
      (local.get $value)
      (i32.const 2)
      (i32.mul)
      (local.set $result)
    )
    (local.get $result)
  )
  (start $start)
)
