(module
  (func $f)
  (func $g)

  (table $t funcref (elem (ref.func $f) (ref.null func) (ref.func $g)))
)
