    (module $m
      (data $d0  "tmp")
      (memory $m0 4 6)
      (table $t0 5 funcref)
      (global $g0 i64 i64.const -5833796675884788010)
      (start 0)
      (func $start
        i64.const -7005356104961843280
        f64.const 4.203_333_479_845_560_9e+168
        i32.trunc_sat_f64_s
        memory.size
        data.drop $d0
        global.get $g0
        i64.store32 align=1
        i64.load16_u align=1
        unreachable
        data.drop $d0
        (loop $b0
          f64.const 0
          unreachable
          f32.const 0
          i32.reinterpret_f32
          (block $b1
            nop
            (loop $b2
              f64.const 0
              f64.abs
              br $b1
              unreachable
              i64.const 0
              global.get $g0
              f64.const 0
              i32.trunc_f64_u
              i32.const 0
              (block $b3
                memory.size
                i64.extend_i32_u
                i32.wrap_i64
                br_if $b2
                br $b1
                br $b3
                br $b3
                (block $b4
                  nop
                  data.drop $d0
                  br $b1
                  br $b3
                  br $b4
                  br $b0
                  global.get $g0
                  unreachable
                  f64.const 0
                  f32.const 0
                  f32.const 0
                  (block $b5
                    (block $b6
                      unreachable
                      br $b2
                      br $b5
                      br $b6
                      f32.const 0
                      i64.trunc_f32_u
                      i64.clz
                      br $b3
                      br $b3
                      unreachable
                      i64.const 0
                      f32.const 0
                      i64.const 0
                      i32.wrap_i64
                      i64.load align=1
                      f32.convert_i64_u
                      (block $b7
                        br $b1
                        memory.size
                        br $b2
                        br $b5
                        br $b5
                        data.drop $d0
                        unreachable
                        global.get $g0
                        f64.convert_i64_u
                        i32.trunc_f64_u
                        (block $b8
                          br $b8
                          unreachable
                          f64.const 0
                          br $b4
                          unreachable
                          br $b6
                          (block $b9
                            br $b7
                            br $b4
                            nop
                            br $b5
                            br $b0
                            global.get $g0
                            nop
                            br $b5
                            br $b7
                            br $b2
                            data.drop $d0
                            i64.const 0
                            f32.convert_i64_u
                            nop
                            i64.trunc_f32_u
                            drop)
                          nop)
                        drop)
                      drop
                      drop
                      drop)
                    nop)
                  drop
                  drop
                  drop)
                nop)
              drop
              drop
              drop
              drop)
            nop)
          drop)
        nop
      )
    )
