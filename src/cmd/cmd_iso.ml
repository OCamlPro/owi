(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let module_name1 = "owi_iso_module1"

let module_name2 = "owi_iso_module2"

let binaryen_fuzzing_support_module weird_log_i64 =
  let log_i32 x =
    Logs.app (fun m -> m "%a@?" Symbolic.Value.pp_int32 x);
    Symbolic_choice_with_memory.return ()
  in
  let log_i64 x =
    Logs.app (fun m -> m "%a@?" Symbolic.Value.pp_int64 x);
    Symbolic_choice_with_memory.return ()
  in
  let log_i64_weird x y =
    Logs.app (fun m ->
      m "%a%a@?" Symbolic.Value.pp_int32 x Symbolic.Value.pp_int32 y );
    Symbolic_choice_with_memory.return ()
  in
  let log_f32 x =
    Logs.app (fun m -> m "%a@?" Symbolic.Value.pp_float32 x);
    Symbolic_choice_with_memory.return ()
  in
  let log_f64 x =
    Logs.app (fun m -> m "%a@?" Symbolic.Value.pp_float64 x);
    Symbolic_choice_with_memory.return ()
  in
  let call_export _n1 _n2 = Symbolic_choice_with_memory.return () in
  let call_export_catch _n =
    Symbolic_choice_with_memory.return @@ Symbolic_value.const_i32 0l
  in
  let sleep _ms id = Symbolic_choice_with_memory.return id in
  let open Symbolic.Extern_func in
  let open Symbolic.Extern_func.Syntax in
  let functions =
    [ ("log-i32", Extern_func (i32 ^->. unit, log_i32))
    ; ( "log-i64"
      , if weird_log_i64 then Extern_func (i32 ^-> i32 ^->. unit, log_i64_weird)
        else Extern_func (i64 ^->. unit, log_i64) )
    ; ("log-f32", Extern_func (f32 ^->. unit, log_f32))
    ; ("log-f64", Extern_func (f64 ^->. unit, log_f64))
    ; ("call-export", Extern_func (i32 ^-> i32 ^->. unit, call_export))
    ; ("call-export-catch", Extern_func (i32 ^->. i32, call_export_catch))
    ; ("sleep", Extern_func (i32 ^-> i32 ^->. i32, sleep))
    ]
  in
  { Link.functions }

let emscripten_fuzzing_support_module () =
  let temp_ret_0 = ref (Smtml.Expr.value (Smtml.Value.Int 0)) in
  let set_temp_ret_0 x =
    temp_ret_0 := x;
    Symbolic_choice_with_memory.return ()
  in
  let get_temp_ret_0 () = Symbolic_choice_with_memory.return !temp_ret_0 in
  let open Symbolic.Extern_func in
  let open Symbolic.Extern_func.Syntax in
  let functions =
    [ ("setTempRet0", Extern_func (i32 ^->. unit, set_temp_ret_0))
    ; ("getTempRet0", Extern_func (unit ^->. i32, get_temp_ret_0))
    ]
  in
  { Link.functions }

let check_iso ~unsafe export_name export_type module1 module2 =
  let weird_log_i64 =
    match
      Binary.Module.find_imported_func_index ~modul_name:"fuzzing-support"
        ~func_name:"log-i64" module1
    with
    | None -> false
    | Some index -> (
      match Binary.Module.get_func_type index module1 with
      | None -> assert false
      | Some (Types.Bt_raw (_, (pt, _rt))) -> (
        match List.length pt with 1 -> false | _n -> true ) )
  in

  let link_state = Cmd_sym.link_symbolic_modules Link.empty_state in
  let link_state =
    Link.extern_module' link_state ~name:"fuzzing-support"
      ~func_typ:Symbolic.Extern_func.extern_type
      (binaryen_fuzzing_support_module weird_log_i64)
  in
  let link_state =
    Link.extern_module' link_state ~name:"env"
      ~func_typ:Symbolic.Extern_func.extern_type
      (emscripten_fuzzing_support_module ())
  in
  let* _module, link_state =
    Compile.Binary.until_link ~name:(Some module_name1) ~unsafe ~optimize:false
      link_state module1
  in
  let* _module, link_state =
    Compile.Binary.until_link ~name:(Some module_name2) ~unsafe ~optimize:false
      link_state module2
  in

  let typ = Types.Bt_raw (None, export_type) in

  let iso_modul = Binary.Module.empty in
  let func1 =
    Runtime.Imported
      { modul = module_name1
      ; name = export_name
      ; assigned_name = Some "iso_func1"
      ; desc = typ
      }
  in
  let iso_modul, idf1 = Binary.Module.add_func func1 iso_modul in
  let func2 =
    Runtime.Imported
      { modul = module_name2
      ; name = export_name
      ; assigned_name = Some "iso_func2"
      ; desc = typ
      }
  in
  let iso_modul, idf2 = Binary.Module.add_func func2 iso_modul in

  let iso_modul, id_owi_assert =
    Binary.Module.add_func
      (Runtime.Imported
         { modul = "owi"
         ; name = "assert"
         ; assigned_name = Some "assert"
         ; desc = Types.Bt_raw (None, ([ (None, Types.Num_type I32) ], []))
         } )
      iso_modul
  in
  let iso_func =
    let id = Some "check_iso_func" in
    let locals =
      [ (None, Types.Num_type F32)
      ; (None, Types.Num_type F32)
      ; (None, Num_type F64)
      ; (None, Num_type F64)
      ]
    in
    let body =
      let put_on_stack =
        List.mapi
          (fun i (_name, _symbol) ->
            let id = Types.Raw i in
            [ Types.Local_get id ] )
          (fst export_type)
        |> List.flatten
      in
      let local_offset = List.length (fst export_type) in

      put_on_stack @ [ Types.Call (Raw idf1) ] @ put_on_stack
      @ [ Types.Call (Raw idf2) ]
      @ ( match snd export_type with
        | [] -> [ Types.I32_const 1l ]
        | [ Types.Num_type I32 ] -> [ Types.I_relop (S32, Eq) ]
        | [ Types.Num_type I64 ] -> [ I_relop (S64, Eq) ]
        | [ Types.Num_type F32 ] ->
          (* Here we can not simply compare the two numbers, because they may both be nan and then the comparison on float will return false. *)
          [ (* We store the two floats *)
            Local_set (Raw (local_offset + 0))
          ; Local_set (Raw (local_offset + 1))
          ; (* We compare the first one with itself to see if it is nan. *)
            Local_get (Raw (local_offset + 0))
          ; Local_get (Raw (local_offset + 0))
          ; F_relop (S32, Eq)
          ; If_else
              ( None
              , Some (Bt_raw (None, ([], [ Num_type I32 ])))
              , [ (* Not nan case, we can directly compare the two numbers *)
                  Local_get (Raw (local_offset + 0))
                ; Local_get (Raw (local_offset + 1))
                ; F_relop (S32, Eq)
                ]
              , [ (* Nan case, we must check if the second one is nan *)
                  Local_get (Raw (local_offset + 1))
                ; Local_get (Raw (local_offset + 1))
                ; F_relop (S32, Eq)
                ; If_else
                    ( None
                    , Some (Bt_raw (None, ([], [ Num_type I32 ])))
                    , [ (* Not nan case, we can compare the two numbers *)
                        Local_get (Raw (local_offset + 0))
                      ; Local_get (Raw (local_offset + 1))
                      ; F_relop (S32, Eq)
                      ]
                    , [ (* Nan case, they are both nan, we return true *)
                        I32_const 1l
                      ] )
                ] )
          ]
        | [ Types.Num_type F64 ] ->
          (* Here we can not simply compare the two numbers, because they may both be nan and then the comparison on float will return false. *)
          [ (* We store the two floats *)
            Local_set (Raw (local_offset + 2))
          ; Local_set (Raw (local_offset + 3))
          ; (* We compare the first one with itself to see if it is nan. *)
            Local_get (Raw (local_offset + 2))
          ; Local_get (Raw (local_offset + 2))
          ; F_relop (S64, Eq)
          ; If_else
              ( None
              , Some (Bt_raw (None, ([], [ Num_type I32 ])))
              , [ (* Not nan case, we can directly compare the two numbers *)
                  Local_get (Raw (local_offset + 2))
                ; Local_get (Raw (local_offset + 3))
                ; F_relop (S64, Eq)
                ]
              , [ (* Nan case, we must check if the second one is nan *)
                  Local_get (Raw (local_offset + 3))
                ; Local_get (Raw (local_offset + 3))
                ; F_relop (S64, Eq)
                ; If_else
                    ( None
                    , Some (Bt_raw (None, ([], [ Num_type I32 ])))
                    , [ (* Not nan case, we can compare the two numbers *)
                        Local_get (Raw (local_offset + 2))
                      ; Local_get (Raw (local_offset + 3))
                      ; F_relop (S64, Eq)
                      ]
                    , [ (* Nan case, they are both nan, we return true *)
                        I32_const 1l
                      ] )
                ] )
          ]
        | rt ->
          Fmt.failwith
            "Equivalence check has not been implemented for result type %a, \
             please open a bug report."
            Types.pp_result_type rt )
      @ [ Call (Raw id_owi_assert) ]
    in
    let type_f =
      let (Bt_raw (_, typ)) = typ in
      Types.Bt_raw (None, (fst typ, []))
    in
    Runtime.Local { Types.type_f; locals; body; id }
  in

  let iso_modul, id_i32_symbol =
    Binary.Module.add_func
      (Runtime.Imported
         { modul = "owi"
         ; name = "i32_symbol"
         ; assigned_name = Some "i32_symbol"
         ; desc = Types.Bt_raw (None, ([], [ Types.Num_type I32 ]))
         } )
      iso_modul
  in

  let iso_modul, id_i64_symbol =
    Binary.Module.add_func
      (Runtime.Imported
         { modul = "owi"
         ; name = "i64_symbol"
         ; assigned_name = Some "i64_symbol"
         ; desc = Types.Bt_raw (None, ([], [ Types.Num_type I64 ]))
         } )
      iso_modul
  in

  let iso_modul, id_f32_symbol =
    Binary.Module.add_func
      (Runtime.Imported
         { modul = "owi"
         ; name = "f32_symbol"
         ; assigned_name = Some "f32_symbol"
         ; desc = Types.Bt_raw (None, ([], [ Types.Num_type F32 ]))
         } )
      iso_modul
  in

  let iso_modul, id_f64_symbol =
    Binary.Module.add_func
      (Runtime.Imported
         { modul = "owi"
         ; name = "f64_symbol"
         ; assigned_name = Some "f64_symbol"
         ; desc = Types.Bt_raw (None, ([], [ Types.Num_type F64 ]))
         } )
      iso_modul
  in

  (* TODO: all of this should be moved to a symbolic/harness.ml module and refactored with what's already in cmd_sym for entry point and invoke-with-symbols *)
  let iso_modul, iso_check_index = Binary.Module.add_func iso_func iso_modul in
  let start_function =
    let id = Some "start" in
    let locals = [] in
    let body =
      List.map
        (function
          | (None | Some _), Types.Num_type I32 -> Types.Call (Raw id_i32_symbol)
          | (None | Some _), Types.Num_type I64 -> Types.Call (Raw id_i64_symbol)
          | (None | Some _), Types.Num_type F32 -> Types.Call (Raw id_f32_symbol)
          | (None | Some _), Types.Num_type F64 -> Types.Call (Raw id_f64_symbol)
          | _ -> Fmt.failwith "TODO" )
        (fst export_type)
      @ [ Types.Call (Raw iso_check_index) ]
    in
    let type_f = Types.Bt_raw (None, ([], [])) in
    Runtime.Local { Types.type_f; locals; body; id }
  in
  let iso_modul, index = Binary.Module.add_func start_function iso_modul in
  let start = Some index in
  let modul = { iso_modul with start } in
  let text_modul = Binary_to_text.modul modul in
  Logs.debug (fun m ->
    m "generated module:@\n  @[<v>%a@]" Text.pp_modul text_modul );
  let+ m, link_state =
    Compile.Binary.until_link ~unsafe:false ~optimize:false ~name:None
      link_state modul
  in
  let m = Symbolic.convert_module_to_run m in

  Interpret.Symbolic.modul ~timeout:None ~timeout_instr:None link_state.envs m

module String_set = Set.Make (String)

let cmd ~deterministic_result_order ~fail_mode ~files ~model_format
  ~no_assert_failure_expression_printing ~no_stop_at_failure ~no_value ~solver
  ~unsafe ~workers ~workspace ~model_out_file ~with_breadcrumbs =
  let* workspace =
    match workspace with
    | Some path -> Ok path
    | None -> Bos.OS.Dir.tmp "owi_iso_%s"
  in
  let* _created_dir = Bos.OS.Dir.create ~path:true ~mode:0o755 workspace in
  let* file1, file2 =
    match files with
    | [ file1; file2 ] -> Ok (file1, file2)
    | [] | [ _ ] -> Fmt.error_msg "require at least two modules"
    | _ -> Fmt.error_msg "require at most two modules"
  in

  Logs.info (fun m -> m "comparing %a and %a" Fpath.pp file1 Fpath.pp file2);
  Logs.info (fun m -> m "module %s is %a" module_name1 Fpath.pp file1);
  Logs.info (fun m -> m "module %s is %a" module_name2 Fpath.pp file2);

  let compile ~unsafe file =
    Compile.File.until_binary_validate ~unsafe ~rac:false ~srac:false file
  in

  Logs.info (fun m -> m "Compiling %a" Fpath.pp file1);
  let* module1 = compile ~unsafe file1 in

  Logs.info (fun m -> m "Compiling %a" Fpath.pp file2);
  let* module2 = compile ~unsafe file2 in

  let funcexports1 =
    module1.exports.func
    |> List.map (fun { Binary.name; id } ->
         let typ = Binary.Module.get_func_type id module1 in
         (name, typ) )
  in
  let funcexports2 =
    module2.exports.func
    |> List.map (fun { Binary.name; id } ->
         let typ = Binary.Module.get_func_type id module2 in
         (name, typ) )
  in

  let exports_name_1 = List.map fst funcexports1 in
  let exports_name_2 = List.map fst funcexports2 in

  Logs.debug (fun m ->
    m "%a exports: %a" Fpath.pp file1
      (Fmt.list ~sep:(fun fmt () -> Fmt.pf fmt " ") Fmt.string)
      exports_name_1 );

  Logs.debug (fun m ->
    m "%a exports: %a" Fpath.pp file2
      (Fmt.list ~sep:(fun fmt () -> Fmt.pf fmt " ") Fmt.string)
      exports_name_2 );

  let exports_name_1 = String_set.of_list exports_name_1 in
  let exports_name_2 = String_set.of_list exports_name_2 in

  let common_exports =
    String_set.inter exports_name_1 exports_name_2 |> String_set.to_list
  in

  let common_exports =
    List.fold_left
      (fun common_exports name ->
        let typ1 =
          List.find_opt
            (fun (name', _typ) -> String.equal name name')
            funcexports1
        in
        let typ2 =
          List.find_opt
            (fun (name', _typ) -> String.equal name name')
            funcexports2
        in
        let typ1, typ2 =
          match (typ1, typ2) with
          | Some (_, Some (Bt_raw (_, typ1))), Some (_, Some (Bt_raw (_, typ2)))
            ->
            (typ1, typ2)
          | _, _ -> assert false
        in
        if Types.func_type_eq typ1 typ2 then (name, typ1) :: common_exports
        else begin
          Logs.warn (fun m ->
            m
              "Removing %s from common exports because they have a different \
               type in each module."
              name );
          common_exports
        end )
      [] common_exports
  in

  Logs.info (fun m ->
    m "common exports: %a"
      (Fmt.list
         ~sep:(fun fmt () -> Fmt.pf fmt " ")
         (fun fmt (elt, _ft) -> Fmt.string fmt elt) )
      common_exports );

  list_fold_left
    (fun () (export_name, export_type) ->
      Logs.info (fun m -> m "checking export %s" export_name);
      let* result = check_iso ~unsafe export_name export_type module1 module2 in

      Cmd_sym.handle_result ~fail_mode ~workers ~solver
        ~deterministic_result_order ~model_format ~no_value
        ~no_assert_failure_expression_printing ~workspace ~no_stop_at_failure
        ~model_out_file ~with_breadcrumbs result )
    () common_exports
