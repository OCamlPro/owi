(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let module_name1 = "owi_iso_module1"

let module_name2 = "owi_iso_module2"

let binaryen_fuzzing_support_module weird_log_i64 =
  let log_i32 x =
    Log.app (fun m -> m "%a@?" Symbolic_i32.pp x);
    Symbolic_choice.return ()
  in
  let log_i64 x =
    Log.app (fun m -> m "%a@?" Symbolic_i64.pp x);
    Symbolic_choice.return ()
  in
  let log_i64_weird x y =
    Log.app (fun m -> m "%a%a@?" Symbolic_i32.pp x Symbolic_i32.pp y);
    Symbolic_choice.return ()
  in
  let log_f32 x =
    Log.app (fun m -> m "%a@?" Symbolic_f32.pp x);
    Symbolic_choice.return ()
  in
  let log_f64 x =
    Log.app (fun m -> m "%a@?" Symbolic_f64.pp x);
    Symbolic_choice.return ()
  in
  let call_export _n1 _n2 = Symbolic_choice.return () in
  let call_export_catch _n =
    Symbolic_choice.return @@ Symbolic_i32.of_concrete 0l
  in
  let sleep _ms id = Symbolic_choice.return id in
  let open Symbolic_extern_func in
  let open Symbolic_extern_func.Syntax in
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
  { Extern.Module.functions; func_type = Symbolic_extern_func.extern_type }

let emscripten_fuzzing_support_module () =
  let temp_ret_0 = ref (Smtml.Expr.value (Smtml.Value.Int 0)) in
  let set_temp_ret_0 x =
    temp_ret_0 := x;
    Symbolic_choice.return ()
  in
  let get_temp_ret_0 () = Symbolic_choice.return !temp_ret_0 in
  let open Symbolic_extern_func in
  let open Symbolic_extern_func.Syntax in
  let functions =
    [ ("setTempRet0", Extern_func (i32 ^->. unit, set_temp_ret_0))
    ; ("getTempRet0", Extern_func (unit ^->. i32, get_temp_ret_0))
    ]
  in
  { Extern.Module.functions; func_type = Symbolic_extern_func.extern_type }

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
      | Some (Binary.Bt_raw (_, (pt, _rt))) -> (
        match List.length pt with 1 -> false | _n -> true ) )
  in

  let link_state =
    Link.State.empty ()
    |> Link.Extern.modul ~name:"owi" Symbolic_wasm_ffi.symbolic_extern_module
    |> Link.Extern.modul ~name:"fuzzing-support"
         (binaryen_fuzzing_support_module weird_log_i64)
    |> Link.Extern.modul ~name:"env" (emscripten_fuzzing_support_module ())
  in
  let* _module, link_state =
    Compile.Binary.until_link ~name:(Some module_name1) ~unsafe link_state
      module1
  in
  let* _module, link_state =
    Compile.Binary.until_link ~name:(Some module_name2) ~unsafe link_state
      module2
  in

  let typ = Binary.Bt_raw (None, export_type) in

  let iso_modul = Binary.Module.empty in
  let func1 =
    Origin.imported ~modul_name:module_name1 ~name:export_name
      ~assigned_name:(Some "iso_func1") ~typ
  in
  let iso_modul, idf1 = Binary.Module.add_func func1 iso_modul in
  let func2 =
    Origin.imported ~modul_name:module_name2 ~name:export_name
      ~assigned_name:(Some "iso_func2") ~typ
  in
  let iso_modul, idf2 = Binary.Module.add_func func2 iso_modul in

  let iso_modul, id_owi_assert =
    Binary.Module.add_func
      (Origin.imported ~modul_name:"owi" ~name:"assert"
         ~assigned_name:(Some "assert")
         ~typ:(Binary.Bt_raw (None, ([ (None, Text.Num_type I32) ], []))) )
      iso_modul
  in
  let iso_func =
    let id = Some "check_iso_func" in
    let locals =
      [ (None, Text.Num_type F32)
      ; (None, Num_type F32)
      ; (None, Num_type F64)
      ; (None, Num_type F64)
      ]
    in
    let body =
      let put_on_stack =
        List.mapi
          (fun id (_name, _symbol) -> [ Binary.Local_get id ])
          (fst export_type)
        |> List.flatten
      in
      let local_offset = List.length (fst export_type) in

      put_on_stack @ [ Binary.Call idf1 ] @ put_on_stack @ [ Binary.Call idf2 ]
      @ ( match snd export_type with
        | [] -> [ Binary.I32_const 1l ]
        | [ Text.Num_type I32 ] -> [ I_relop (S32, Eq) ]
        | [ Text.Num_type I64 ] -> [ I_relop (S64, Eq) ]
        | [ Text.Num_type F32 ] ->
          (* Here we can not simply compare the two numbers, because they may both be nan and then the comparison on float will return false. *)
          [ (* We store the two floats *)
            Local_set (local_offset + 0)
          ; Local_set (local_offset + 1)
          ; (* We compare the first one with itself to see if it is nan. *)
            Local_get (local_offset + 0)
          ; Local_get (local_offset + 0)
          ; F_relop (S32, Eq)
          ; If_else
              ( None
              , Some (Bt_raw (None, ([], [ Num_type I32 ])))
              , [ (* Not nan case, we can directly compare the two numbers *)
                  Binary.Local_get (local_offset + 0)
                ; Local_get (local_offset + 1)
                ; F_relop (S32, Eq)
                ]
                |> Annotated.dummy_deep
              , [ (* Nan case, we must check if the second one is nan *)
                  Binary.Local_get (local_offset + 1)
                ; Local_get (local_offset + 1)
                ; F_relop (S32, Eq)
                ; If_else
                    ( None
                    , Some (Bt_raw (None, ([], [ Num_type I32 ])))
                    , [ (* Not nan case, we can compare the two numbers *)
                        Binary.Local_get (local_offset + 0)
                      ; Local_get (local_offset + 1)
                      ; F_relop (S32, Eq)
                      ]
                      |> Annotated.dummy_deep
                    , [ (* Nan case, they are both nan, we return true *)
                        Binary.I32_const 1l
                      ]
                      |> Annotated.dummy_deep )
                ]
                |> Annotated.dummy_deep )
          ]
        | [ Text.Num_type F64 ] ->
          (* Here we can not simply compare the two numbers, because they may both be nan and then the comparison on float will return false. *)
          [ (* We store the two floats *)
            Local_set (local_offset + 2)
          ; Local_set (local_offset + 3)
          ; (* We compare the first one with itself to see if it is nan. *)
            Local_get (local_offset + 2)
          ; Local_get (local_offset + 2)
          ; F_relop (S64, Eq)
          ; If_else
              ( None
              , Some (Bt_raw (None, ([], [ Num_type I32 ])))
              , [ (* Not nan case, we can directly compare the two numbers *)
                  Binary.Local_get (local_offset + 2)
                ; Local_get (local_offset + 3)
                ; F_relop (S64, Eq)
                ]
                |> Annotated.dummy_deep
              , [ (* Nan case, we must check if the second one is nan *)
                  Binary.Local_get (local_offset + 3)
                ; Local_get (local_offset + 3)
                ; F_relop (S64, Eq)
                ; If_else
                    ( None
                    , Some (Bt_raw (None, ([], [ Num_type I32 ])))
                    , [ (* Not nan case, we can compare the two numbers *)
                        Binary.Local_get (local_offset + 2)
                      ; Local_get (local_offset + 3)
                      ; F_relop (S64, Eq)
                      ]
                      |> Annotated.dummy_deep
                    , [ (* Nan case, they are both nan, we return true *)
                        Binary.I32_const 1l
                      ]
                      |> Annotated.dummy_deep )
                ]
                |> Annotated.dummy_deep )
          ]
        | rt ->
          Fmt.failwith
            "Equivalence check has not been implemented for result type %a, \
             please open a bug report."
            Text.pp_result_type rt )
      @ [ Call id_owi_assert ]
    in
    let body = Annotated.dummies body |> Annotated.dummy in
    let type_f =
      let (Bt_raw (_, typ) : Binary.block_type) = typ in
      Binary.Bt_raw (None, (fst typ, []))
    in
    Origin.Local { Binary.Func.type_f; locals; body; id }
  in

  let iso_modul, id_i32_symbol =
    Binary.Module.add_func
      (Origin.imported ~modul_name:"owi" ~name:"i32_symbol"
         ~assigned_name:(Some "i32_symbol")
         ~typ:(Binary.Bt_raw (None, ([], [ Text.Num_type I32 ]))) )
      iso_modul
  in

  let iso_modul, id_i64_symbol =
    Binary.Module.add_func
      (Origin.imported ~modul_name:"owi" ~name:"i64_symbol"
         ~assigned_name:(Some "i64_symbol")
         ~typ:(Binary.Bt_raw (None, ([], [ Text.Num_type I64 ]))) )
      iso_modul
  in

  let iso_modul, id_f32_symbol =
    Binary.Module.add_func
      (Origin.imported ~modul_name:"owi" ~name:"f32_symbol"
         ~assigned_name:(Some "f32_symbol")
         ~typ:(Binary.Bt_raw (None, ([], [ Text.Num_type F32 ]))) )
      iso_modul
  in

  let iso_modul, id_f64_symbol =
    Binary.Module.add_func
      (Origin.imported ~modul_name:"owi" ~name:"f64_symbol"
         ~assigned_name:(Some "f64_symbol")
         ~typ:(Binary.Bt_raw (None, ([], [ Text.Num_type F64 ]))) )
      iso_modul
  in

  (* TODO: all of this should be moved to a symbolic/harness.ml module and refactored with what's already in cmd_sym for entry point and invoke-with-symbols *)
  let iso_modul, iso_check_index = Binary.Module.add_func iso_func iso_modul in
  let start_function =
    let id = Some "start" in
    let locals = [] in
    let body =
      Annotated.dummy_deep
      @@ List.map
           (function
             | (None | Some _), Text.Num_type I32 -> Binary.Call id_i32_symbol
             | (None | Some _), Num_type I64 -> Call id_i64_symbol
             | (None | Some _), Num_type F32 -> Call id_f32_symbol
             | (None | Some _), Num_type F64 -> Call id_f64_symbol
             | _ -> Fmt.failwith "TODO" )
           (fst export_type)
      @ [ Binary.Call iso_check_index ]
    in
    let type_f = Binary.Bt_raw (None, ([], [])) in
    Origin.Local { Binary.Func.type_f; locals; body; id }
  in
  let iso_modul, index = Binary.Module.add_func start_function iso_modul in
  let start = Some index in
  let modul = { iso_modul with start } in
  let text_modul = Binary_to_text.modul modul in
  Log.debug (fun m ->
    m "generated module:@\n  @[<v>%a@]" Text.Module.pp text_modul );
  let+ m, link_state =
    Compile.Binary.until_link ~unsafe:false ~name:None link_state modul
  in
  let module I = Interpret.Symbolic (Interpret.Default_parameters) in
  I.modul link_state m

module String_set = Set.Make (String)

let cmd ~deterministic_result_order ~fail_mode ~exploration_strategy ~files
  ~model_format ~no_assert_failure_expression_printing ~no_stop_at_failure
  ~no_value ~solver ~unsafe ~workers ~workspace ~model_out_file
  ~with_breadcrumbs =
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

  Log.info (fun m -> m "comparing %a and %a" Fpath.pp file1 Fpath.pp file2);
  Log.info (fun m -> m "module %s is %a" module_name1 Fpath.pp file1);
  Log.info (fun m -> m "module %s is %a" module_name2 Fpath.pp file2);

  let compile ~unsafe file = Compile.File.until_validate ~unsafe file in

  Log.info (fun m -> m "Compiling %a" Fpath.pp file1);
  let* module1 = compile ~unsafe file1 in

  Log.info (fun m -> m "Compiling %a" Fpath.pp file2);
  let* module2 = compile ~unsafe file2 in

  let funcexports1 =
    module1.exports.func
    |> Array.map (fun { Binary.Export.name; id } ->
      let typ = Binary.Module.get_func_type id module1 in
      (name, typ) )
  in
  let funcexports2 =
    module2.exports.func
    |> Array.map (fun { Binary.Export.name; id } ->
      let typ = Binary.Module.get_func_type id module2 in
      (name, typ) )
  in

  let exports_name_1 = Array.map fst funcexports1 in
  let exports_name_2 = Array.map fst funcexports2 in

  Log.debug (fun m ->
    m "%a exports: %a" Fpath.pp file1
      (Fmt.array ~sep:(fun fmt () -> Fmt.pf fmt " ") Fmt.string)
      exports_name_1 );

  Log.debug (fun m ->
    m "%a exports: %a" Fpath.pp file2
      (Fmt.array ~sep:(fun fmt () -> Fmt.pf fmt " ") Fmt.string)
      exports_name_2 );

  let array_to_string_set a =
    Array.fold_left (fun s v -> String_set.add v s) String_set.empty a
  in

  let exports_name_1 = array_to_string_set exports_name_1 in
  let exports_name_2 = array_to_string_set exports_name_2 in

  let common_exports =
    String_set.inter exports_name_1 exports_name_2 |> String_set.to_list
  in

  let common_exports =
    List.fold_left
      (fun common_exports name ->
        let typ1 =
          Array.find_opt
            (fun (name', _typ) -> String.equal name name')
            funcexports1
        in
        let typ2 =
          Array.find_opt
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
        if Text.func_type_eq typ1 typ2 then (name, typ1) :: common_exports
        else begin
          Log.warn (fun m ->
            m
              "Removing %s from common exports because they have a different \
               type in each module."
              name );
          common_exports
        end )
      [] common_exports
  in

  Log.info (fun m ->
    m "common exports: %a"
      (Fmt.list
         ~sep:(fun fmt () -> Fmt.pf fmt " ")
         (fun fmt (elt, _ft) -> Fmt.string fmt elt) )
      common_exports );

  list_fold_left
    (fun () (export_name, export_type) ->
      Log.info (fun m -> m "checking export %s" export_name);
      let* result = check_iso ~unsafe export_name export_type module1 module2 in
      let run_time = if Log.is_bench_enabled () then Some 0. else None in

      Symbolic_driver.handle_result ~exploration_strategy ~fail_mode ~workers
        ~solver ~deterministic_result_order ~model_format ~no_value
        ~no_assert_failure_expression_printing ~workspace ~no_stop_at_failure
        ~model_out_file ~with_breadcrumbs ~run_time result )
    () common_exports
