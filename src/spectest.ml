open Types

let m =
  Module
    { id = Some "spectest"
    ; fields =
        [ MMem (Some "memory", { min = 1; max = Some 2 })
        ; MFunc
            { type_f = Bt_raw (None, ([], []))
            ; locals = []
            ; body = []
            ; id = Some "print"
            }
        ; MFunc
            { type_f = Bt_raw (None, ([ (None, Num_type I32) ], []))
            ; locals = []
            ; body = []
            ; id = Some "print_i32"
            }
        ; MFunc
            { type_f = Bt_raw (None, ([ (None, Num_type I64) ], []))
            ; locals = []
            ; body = []
            ; id = Some "print_i64"
            }
        ; MFunc
            { type_f = Bt_raw (None, ([ (None, Num_type F32) ], []))
            ; locals = []
            ; body = []
            ; id = Some "print_f32"
            }
        ; MFunc
            { type_f = Bt_raw (None, ([ (None, Num_type F64) ], []))
            ; locals = []
            ; body = []
            ; id = Some "print_f64"
            }
        ; MFunc
            { type_f =
                Bt_raw
                  (None, ([ (None, Num_type I32); (None, Num_type F32) ], []))
            ; locals = []
            ; body = []
            ; id = Some "print_i32_f32"
            }
        ; MFunc
            { type_f =
                Bt_raw
                  (None, ([ (None, Num_type F64); (None, Num_type F64) ], []))
            ; locals = []
            ; body = []
            ; id = Some "print_f64_f64"
            }
        ; MFunc
            { type_f = Bt_raw (None, ([], []))
            ; locals = []
            ; body = []
            ; id = Some "func"
            }
        ; MFunc
            { type_f = Bt_raw (None, ([ (None, Num_type I32) ], []))
            ; locals = []
            ; body = []
            ; id = Some "func-i32"
            }
        ; MFunc
            { type_f = Bt_raw (None, ([], [ Num_type I32 ]))
            ; locals = []
            ; body = [ I32_const 1l ]
            ; id = Some "func->i32"
            }
        ; MFunc
            { type_f =
                Bt_raw (None, ([ (None, Num_type I32) ], [ Num_type I32 ]))
            ; locals = []
            ; body = [ I32_const 1l ]
            ; id = Some "func-i32->i32"
            }
        ; MTable (Some "table", ({ min = 10; max = Some 20 }, Func_ref))
        ; MGlobal
            { type_ = (Const, Num_type I32)
            ; init = [ I32_const 666l ]
            ; id = Some "global_i32"
            }
        ; MGlobal
            { type_ = (Const, Num_type I64)
            ; init = [ I64_const 666L ]
            ; id = Some "global_i64"
            }
        ; MGlobal
            { type_ = (Const, Num_type F32)
            ; init = [ F32_const Float32.zero ]
            ; id = Some "global_f32"
            }
        ; MGlobal
            { type_ = (Const, Num_type F64)
            ; init = [ F64_const Float64.zero ]
            ; id = Some "global_f64"
            }
        ; MExport { name = "func"; desc = Export_func (Some (Symbolic "func")) }
        ; MExport
            { name = "func-i32"
            ; desc = Export_func (Some (Symbolic "func-i32"))
            }
        ; MExport
            { name = "func->i32"
            ; desc = Export_func (Some (Symbolic "func->i32"))
            }
        ; MExport
            { name = "func-i32->i32"
            ; desc = Export_func (Some (Symbolic "func-i32->i32"))
            }
        ; MExport
            { name = "memory"; desc = Export_mem (Some (Symbolic "memory")) }
        ; MExport
            { name = "table"; desc = Export_table (Some (Symbolic "table")) }
        ; MExport
            { name = "print"; desc = Export_func (Some (Symbolic "print")) }
        ; MExport
            { name = "print_i32"
            ; desc = Export_func (Some (Symbolic "print_i32"))
            }
        ; MExport
            { name = "print_f32"
            ; desc = Export_func (Some (Symbolic "print_f32"))
            }
        ; MExport
            { name = "print_i64"
            ; desc = Export_func (Some (Symbolic "print_i64"))
            }
        ; MExport
            { name = "print_f64"
            ; desc = Export_func (Some (Symbolic "print_f64"))
            }
        ; MExport
            { name = "print_i32_f32"
            ; desc = Export_func (Some (Symbolic "print_i32_f32"))
            }
        ; MExport
            { name = "print_f64_f64"
            ; desc = Export_func (Some (Symbolic "print_f64_f64"))
            }
        ; MExport
            { name = "global_i32"
            ; desc = Export_global (Some (Symbolic "global_i32"))
            }
        ; MExport
            { name = "global_i64"
            ; desc = Export_global (Some (Symbolic "global_i64"))
            }
        ; MExport
            { name = "global_f32"
            ; desc = Export_global (Some (Symbolic "global_f32"))
            }
        ; MExport
            { name = "global_f64"
            ; desc = Export_global (Some (Symbolic "global_f64"))
            }
        ]
    }
