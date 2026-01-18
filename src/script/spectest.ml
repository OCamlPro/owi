(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Fmt
open Syntax

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

  { Extern.Module.functions; func_type = Concrete_extern_func.extern_type }

let m =
  let open Wast in
  Text_module
    { id = Some "spectest"
    ; fields =
        (let modul_name = "spectest_extern" in
         [ Import
             { modul_name
             ; name = "print"
             ; typ = Func (Some "print", Bt_raw (None, ([], [])))
             }
         ; Import
             { modul_name
             ; name = "print_i32"
             ; typ =
                 Func
                   ( Some "print_i32"
                   , Bt_raw (None, ([ (None, Num_type I32) ], [])) )
             }
         ; Import
             { modul_name
             ; name = "print_i64"
             ; typ =
                 Func
                   ( Some "print_i64"
                   , Bt_raw (None, ([ (None, Num_type I64) ], [])) )
             }
         ; Import
             { modul_name
             ; name = "print_f32"
             ; typ =
                 Func
                   ( Some "print_f32"
                   , Bt_raw (None, ([ (None, Num_type F32) ], [])) )
             }
         ; Import
             { modul_name
             ; name = "print_f64"
             ; typ =
                 Func
                   ( Some "print_f64"
                   , Bt_raw (None, ([ (None, Num_type F64) ], [])) )
             }
         ; Import
             { modul_name
             ; name = "print_i32_f32"
             ; typ =
                 Func
                   ( Some "print_i32_f32"
                   , Bt_raw
                       ( None
                       , ([ (None, Num_type I32); (None, Num_type F32) ], []) )
                   )
             }
         ; Import
             { modul_name
             ; name = "print_f64_f64"
             ; typ =
                 Func
                   ( Some "print_f64_f64"
                   , Bt_raw
                       ( None
                       , ([ (None, Num_type F64); (None, Num_type F64) ], []) )
                   )
             }
         ; Import
             { modul_name
             ; name = "func"
             ; typ = Func (Some "func", Bt_raw (None, ([], [])))
             }
         ; Import
             { modul_name
             ; name = "func-i32"
             ; typ =
                 Func
                   ( Some "func-i32"
                   , Bt_raw (None, ([ (None, Num_type I32) ], [])) )
             }
         ; Import
             { modul_name
             ; name = "func->i32"
             ; typ =
                 Func (Some "func->i32", Bt_raw (None, ([], [ Num_type I32 ])))
             }
         ; Import
             { modul_name
             ; name = "func-i32->i32"
             ; typ =
                 Func
                   ( Some "func-i32->i32"
                   , Bt_raw (None, ([ (None, Num_type I32) ], [ Num_type I32 ]))
                   )
             }
         ; Mem (Some "memory", { min = 1; max = Some 2 })
         ; Table (Some "table", ({ min = 10; max = Some 20 }, (Null, Func_ht)))
         ; Global
             { typ = (Const, Num_type I32)
             ; init = [ Text.I32_const 666l ] |> Annotated.dummy_deep
             ; id = Some "global_i32"
             }
         ; Global
             { typ = (Const, Num_type I64)
             ; init = [ Text.I64_const 666L ] |> Annotated.dummy_deep
             ; id = Some "global_i64"
             }
         ; Global
             { typ = (Const, Num_type F32)
             ; init =
                 [ Text.F32_const (Float32.of_float 666.6) ]
                 |> Annotated.dummy_deep
             ; id = Some "global_f32"
             }
         ; Global
             { typ = (Const, Num_type F64)
             ; init =
                 [ Text.F64_const (Float64.of_float 666.6) ]
                 |> Annotated.dummy_deep
             ; id = Some "global_f64"
             }
         ; Export { name = "func"; typ = Func (Some (Text "func")) }
         ; Export { name = "func-i32"; typ = Func (Some (Text "func-i32")) }
         ; Export { name = "func->i32"; typ = Func (Some (Text "func->i32")) }
         ; Export
             { name = "func-i32->i32"
             ; typ = Func (Some (Text "func-i32->i32"))
             }
         ; Export { name = "memory"; typ = Mem (Some (Text "memory")) }
         ; Export { name = "table"; typ = Table (Some (Text "table")) }
         ; Export { name = "print"; typ = Func (Some (Text "print")) }
         ; Export { name = "print_i32"; typ = Func (Some (Text "print_i32")) }
         ; Export { name = "print_f32"; typ = Func (Some (Text "print_f32")) }
         ; Export { name = "print_i64"; typ = Func (Some (Text "print_i64")) }
         ; Export { name = "print_f64"; typ = Func (Some (Text "print_f64")) }
         ; Export
             { name = "print_i32_f32"
             ; typ = Func (Some (Text "print_i32_f32"))
             }
         ; Export
             { name = "print_f64_f64"
             ; typ = Func (Some (Text "print_f64_f64"))
             }
         ; Export
             { name = "global_i32"; typ = Global (Some (Text "global_i32")) }
         ; Export
             { name = "global_i64"; typ = Global (Some (Text "global_i64")) }
         ; Export
             { name = "global_f32"; typ = Global (Some (Text "global_f32")) }
         ; Export
             { name = "global_f64"; typ = Global (Some (Text "global_f64")) }
         ] )
    }
