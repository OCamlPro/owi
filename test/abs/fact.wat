(module
  (func $start (result i32)
    (local $n i32)
    (local $result i32)
    
    i32.const 5
    local.set $n
    
    i32.const 1
    local.set $result
    
    (block $done
      (loop $continue
        ;; if n <= 1, exit
        local.get $n
        i32.const 1
        i32.le_s
        br_if $done
        
        ;; result *= n
        local.get $result
        local.get $n
        i32.mul
        local.set $result
        
        ;; n--
        local.get $n
        i32.const 1
        i32.sub
        local.set $n
        
        br $continue
      )
    )
    
    local.get $result
  )
  
  (start $start)
)
