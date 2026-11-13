(module
  (type $i32-i32 (func (param i32) (result i32)))

  ;; Make $incr referenceable by ref.func
  (elem declare func $incr)

  (func $incr (type $i32-i32)
    i32.const 1
    local.get 0
    i32.add 
  )

  (func $start
    i32.const 1
    ref.func $incr
    call_ref $i32-i32
    drop
  )

	(start $start)
)
