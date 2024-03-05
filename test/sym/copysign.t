float copysign:
  $ owi sym copysign_f32.wat --deterministic-result-order
  All OK
;; Deactivated for now as pretty time consuming and a bit
;; redundant with the 32 bit version
;;  $ owi sym copysign_f64.wat
;;  All OK
