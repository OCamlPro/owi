;; (module 
;;   (global  (mut i32) i32.const 192218630)
;;   (global  (mut i32) i32.const -1370779334)
;;   ;; (global  i32 (i32.add (i32.const 12) (i32.const 0)))
;;   (func    (local  f32)
    
;;   )
;;   (start 0)
;; )

;; (module $modul
;;   (global $g4817 i64 (i64.const -7054719741964086982))
;;   ;; (global $g4821 i64 (i64.const 9180578221245992184))
;;   ;; (global $g4822 i64 (i64.const 0))
;;   (func $f4834   
;;     global.get $g4817
;;     drop
;;   )
;;   (func $f4835   
;;     i64.const 0
;;     i64.const 0
;;     i64.add
;;     drop
;;   )
;;   (start 0)
;; )

;; (module $modul
;;   (global $g0 (mut i32) (i32.const 1577979128))
;;   (func $f0   
;;     i64.const 0
;;     i64.const 0
;;     i64.add
;;     drop
;;   )
;;   (func $f1   
;;     i64.const 0
;;     drop
;;   )
;;   (start 0)
;; )

;; (module $modul
;;   (global $g0 (mut i32) (i32.const 576776528))
;;   (func $f0 (param $l0 i64)  
;;     local.get $l0
;;     drop
;;   )
;;   (func $f1 (param $l1 i32)  
;;     i32.const 0
;;     i32.const 0
;;     i32.add
;;     drop
;;   )
;;   (func $start   
;;     i32.const 0
;;     call $f1
;;   )
;;   (start $start)
;; )

;; (module $modul
;;   (func $f0 (result i32)  (local $l0 i32)
;;     local.get $l0
;;     ;; drop
;;   )
;;   (func $f1   
;;     i64.const 0
;;     i64.const 0
;;     i64.add
;;     drop
;;   )
;;   (func $f2   
;;     i32.const 0
;;     drop
;;   )
;;   (func $f3   
;;     i32.const 0
;;     drop
;;   )
;;   (func $start
;;     call $f0
;;     drop
;;   )
;;   (start $start)
;; )

(module $m
  (start 0)
  (global $g0 f32 f32.const -0)
  (global $g1 f32 f32.const 7.348_124_312_909_468_2e+23)
  (global $g2 i32 i32.const -866556934)
  (func $start   
    f64.const 5.107_300_398_339_497_1e-55
    i64.trunc_sat_f64_s
    i32.wrap_i64
    i64.const 0
    i32.wrap_i64
    i64.const 0
    i32.const 0
    i32.eqz
    i32.eqz
    f64.const 0
    i64.trunc_f64_u
    f32.const 0
    i64.trunc_f32_u
    i32.wrap_i64
    i64.const 0
    i32.const 0
    i32.clz
    f64.const 0
    i32.trunc_f64_u
    f32.const 0
    drop
    drop
    drop
    drop
    drop
    drop
    drop
    drop
    drop
    drop
  )
  (func $f0   (local $l0 i64)
    nop
  )
  (func $f1   (local $l1 i32) (local $l2 i32)
    f64.const 0
    i64.trunc_sat_f64_u
    f32.const 0
    i32.trunc_f32_u
    f32.convert_i32_u
    i64.const 0
    f32.const 0
    f64.const 0
    i64.trunc_f64_u
    nop
    nop
    f32.const 0
    i32.const 0
    i32.extend8_s
    f32.convert_i32_u
    f64.const 0
    i64.trunc_f64_u
    f32.convert_i64_u
    i32.const 0
    drop
    drop
    drop
    drop
    drop
    drop
    drop
    drop
    drop
  )
)
