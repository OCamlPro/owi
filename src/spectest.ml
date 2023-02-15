open Types.Symbolic

let extern_m : Link.extern_module =
  let fmt = Format.std_formatter in
  let print = () in
  let print_i32 i = Format.fprintf fmt "%li@\n%!" i in
  let print_i64 i = Format.fprintf fmt "%Li@\n%!" i in
  let print_f32 f = Format.fprintf fmt "%s@\n%!" (Float32.to_string f) in
  let print_f64 f = Format.fprintf fmt "%s@\n%!" (Float64.to_string f) in
  let print_i32_f32 i f =
    print_i32 i;
    print_f32 f
  in
  let print_f64_f64 f1 f2 =
    print_f64 f1;
    print_f64 f2
  in
  let func = () in
  let func_in_i32 (_i : int32) = () in
  let func_out_i32 = 1l in
  let func_in_i32_out_i32 (_i : int32) = 1l in

  let functions =
    [ ("print", Value.Func.Extern_func (Func (Res, R0), print))
    ; ( "print_i32"
      , Value.Func.Extern_func (Func (Arg (I32, Res), R0), print_i32) )
    ; ( "print_i64"
      , Value.Func.Extern_func (Func (Arg (I64, Res), R0), print_i64) )
    ; ( "print_f32"
      , Value.Func.Extern_func (Func (Arg (F32, Res), R0), print_f32) )
    ; ( "print_f64"
      , Value.Func.Extern_func (Func (Arg (F64, Res), R0), print_f64) )
    ; ( "print_i32_f32"
      , Value.Func.Extern_func
          (Func (Arg (I32, Arg (F32, Res)), R0), print_i32_f32) )
    ; ( "print_f64_f64"
      , Value.Func.Extern_func
          (Func (Arg (F64, Arg (F64, Res)), R0), print_f64_f64) )
    ; ("func", Value.Func.Extern_func (Func (Res, R0), func))
    ; ( "func-i32"
      , Value.Func.Extern_func (Func (Arg (I32, Res), R0), func_in_i32) )
    ; ("func->i32", Value.Func.Extern_func (Func (Res, R1 I32), func_out_i32))
    ; ( "func-i32->i32"
      , Value.Func.Extern_func
          (Func (Arg (I32, Res), R1 I32), func_in_i32_out_i32) )
    ]
  in

  { functions }

let m =
  Module
    { id = Some "spectest"
    ; fields =
        [ MImport
            { modul = "spectest_extern"
            ; name = "print"
            ; desc = Import_func (Some "print", Bt_raw (None, ([], [])))
            }
        ; MImport
            { modul = "spectest_extern"
            ; name = "print_i32"
            ; desc =
                Import_func
                  ( Some "print_i32"
                  , Bt_raw (None, ([ (None, Num_type I32) ], [])) )
            }
        ; MImport
            { modul = "spectest_extern"
            ; name = "print_i64"
            ; desc =
                Import_func
                  ( Some "print_i64"
                  , Bt_raw (None, ([ (None, Num_type I64) ], [])) )
            }
        ; MImport
            { modul = "spectest_extern"
            ; name = "print_f32"
            ; desc =
                Import_func
                  ( Some "print_f32"
                  , Bt_raw (None, ([ (None, Num_type F32) ], [])) )
            }
        ; MImport
            { modul = "spectest_extern"
            ; name = "print_f64"
            ; desc =
                Import_func
                  ( Some "print_f64"
                  , Bt_raw (None, ([ (None, Num_type F64) ], [])) )
            }
        ; MImport
            { modul = "spectest_extern"
            ; name = "print_i32_f32"
            ; desc =
                Import_func
                  ( Some "print_i32_f32"
                  , Bt_raw
                      ( None
                      , ([ (None, Num_type I32); (None, Num_type F32) ], []) )
                  )
            }
        ; MImport
            { modul = "spectest_extern"
            ; name = "print_f64_f64"
            ; desc =
                Import_func
                  ( Some "print_f64_f64"
                  , Bt_raw
                      ( None
                      , ([ (None, Num_type F64); (None, Num_type F64) ], []) )
                  )
            }
        ; MImport
            { modul = "spectest_extern"
            ; name = "func"
            ; desc = Import_func (Some "func", Bt_raw (None, ([], [])))
            }
        ; MImport
            { modul = "spectest_extern"
            ; name = "func-i32"
            ; desc =
                Import_func
                  ( Some "func-i32"
                  , Bt_raw (None, ([ (None, Num_type I32) ], [])) )
            }
        ; MImport
            { modul = "spectest_extern"
            ; name = "func->i32"
            ; desc =
                Import_func
                  (Some "func->i32", Bt_raw (None, ([], [ Num_type I32 ])))
            }
        ; MImport
            { modul = "spectest_extern"
            ; name = "func-i32->i32"
            ; desc =
                Import_func
                  ( Some "func-i32->i32"
                  , Bt_raw (None, ([ (None, Num_type I32) ], [ Num_type I32 ]))
                  )
            }
        ; MMem (Some "memory", { min = 1; max = Some 2 })
        ; MTable (Some "table", ({ min = 10; max = Some 20 }, (Null, Func_ht)))
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
