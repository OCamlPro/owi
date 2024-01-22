(module
  (func $f (result f64) (f64.const 0))  ;; adds implicit type definition
  (func $g (param i32))                 ;; reuses explicit type definition
  (type $t (func (param i32)))

  (func $i32->void (type 0))                ;; (param i32)
  (func $void->f64 (type 1) (f64.const 0))  ;; (result f64)
  (func $check
    (call $i32->void (i32.const 0))
    (drop (call $void->f64))
  )
)
