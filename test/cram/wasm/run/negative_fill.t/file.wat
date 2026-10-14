;; taken from https://github.com/OCamlPro/owi/issues/1132
(module
  (memory 1)
  (func $main
    ;; write 0xFF (the low bit of -1) at address 0x05
    (memory.fill (i32.const 5) (i32.const -1) (i32.const 1))
    ;; the spec writes 0xFF
    (if (i32.ne (i32.load8_u (i32.const 5)) (i32.const 255))
      (then unreachable)))
  (start $main))
