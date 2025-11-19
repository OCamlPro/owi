(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Fmt

type extern_module = Concrete_extern_func.extern_func Link.extern_module

let ( let* ) = Result.bind

let extern_m =
  let print () = Ok () in
  let print_i32 i =
    pr "%li@\n%!" i;
    Ok ()
  in
  let print_i64 i =
    pr "%Li@\n%!" i;
    Ok ()
  in
  let print_f32 f =
    pr "%a@\n%!" Float32.pp f;
    Ok ()
  in
  let print_f64 f =
    pr "%a@\n%!" Float64.pp f;
    Ok ()
  in
  let print_i32_f32 i f =
    let* () = print_i32 i in
    let* () = print_f32 f in
    Ok ()
  in
  let print_f64_f64 f1 f2 =
    let* () = print_f64 f1 in
    let* () = print_f64 f2 in
    Ok ()
  in
  let func () = Ok () in
  let func_in_i32 (_i : int32) = Ok () in
  let func_out_i32 () = Ok 1l in
  let func_in_i32_out_i32 (_i : int32) = Ok 1l in
  let open Concrete_extern_func in
  let open Concrete_extern_func.Syntax in
  let functions =
    [ ("print", Extern_func (unit ^->. unit, print))
    ; ("print_i32", Extern_func (i32 ^->. unit, print_i32))
    ; ("print_i64", Extern_func (i64 ^->. unit, print_i64))
    ; ("print_f32", Extern_func (f32 ^->. unit, print_f32))
    ; ("print_f64", Extern_func (f64 ^->. unit, print_f64))
    ; ("print_i32_f32", Extern_func (i32 ^-> f32 ^->. unit, print_i32_f32))
    ; ("print_f64_f64", Extern_func (f64 ^-> f64 ^->. unit, print_f64_f64))
    ; ("func", Extern_func (unit ^->. unit, func))
    ; ("func-i32", Extern_func (i32 ^->. unit, func_in_i32))
    ; ("func->i32", Extern_func (unit ^->. i32, func_out_i32))
    ; ("func-i32->i32", Extern_func (i32 ^->. i32, func_in_i32_out_i32))
    ]
  in

  { Link.functions; func_type = Concrete.Extern_func.extern_type }

let m =
  let open Wast in
  Text_module
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
            { typ = (Const, Num_type I32)
            ; init = [ Text.I32_const 666l ] |> Annotated.dummy_deep
            ; id = Some "global_i32"
            }
        ; MGlobal
            { typ = (Const, Num_type I64)
            ; init = [ Text.I64_const 666L ] |> Annotated.dummy_deep
            ; id = Some "global_i64"
            }
        ; MGlobal
            { typ = (Const, Num_type F32)
            ; init =
                [ Text.F32_const (Float32.of_float 666.6) ]
                |> Annotated.dummy_deep
            ; id = Some "global_f32"
            }
        ; MGlobal
            { typ = (Const, Num_type F64)
            ; init =
                [ Text.F64_const (Float64.of_float 666.6) ]
                |> Annotated.dummy_deep
            ; id = Some "global_f64"
            }
        ; MExport { name = "func"; desc = Export_func (Some (Text "func")) }
        ; MExport
            { name = "func-i32"; desc = Export_func (Some (Text "func-i32")) }
        ; MExport
            { name = "func->i32"; desc = Export_func (Some (Text "func->i32")) }
        ; MExport
            { name = "func-i32->i32"
            ; desc = Export_func (Some (Text "func-i32->i32"))
            }
        ; MExport { name = "memory"; desc = Export_mem (Some (Text "memory")) }
        ; MExport { name = "table"; desc = Export_table (Some (Text "table")) }
        ; MExport { name = "print"; desc = Export_func (Some (Text "print")) }
        ; MExport
            { name = "print_i32"; desc = Export_func (Some (Text "print_i32")) }
        ; MExport
            { name = "print_f32"; desc = Export_func (Some (Text "print_f32")) }
        ; MExport
            { name = "print_i64"; desc = Export_func (Some (Text "print_i64")) }
        ; MExport
            { name = "print_f64"; desc = Export_func (Some (Text "print_f64")) }
        ; MExport
            { name = "print_i32_f32"
            ; desc = Export_func (Some (Text "print_i32_f32"))
            }
        ; MExport
            { name = "print_f64_f64"
            ; desc = Export_func (Some (Text "print_f64_f64"))
            }
        ; MExport
            { name = "global_i32"
            ; desc = Export_global (Some (Text "global_i32"))
            }
        ; MExport
            { name = "global_i64"
            ; desc = Export_global (Some (Text "global_i64"))
            }
        ; MExport
            { name = "global_f32"
            ; desc = Export_global (Some (Text "global_f32"))
            }
        ; MExport
            { name = "global_f64"
            ; desc = Export_global (Some (Text "global_f64"))
            }
        ]
    }
