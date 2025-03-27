(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let module_name1 = "owi_iso_module1"

let module_name2 = "owi_iso_module2"

let link_state =
  lazy
    (let func_typ = Symbolic.Extern_func.extern_type in
     let link_state =
       Link.extern_module' Link.empty_state ~name:"symbolic" ~func_typ
         Symbolic_wasm_ffi.symbolic_extern_module
     in
     Link.extern_module' link_state ~name:"summaries" ~func_typ
       Symbolic_wasm_ffi.summaries_extern_module )

let ( let*/ ) (t : 'a Result.t)
  (f : 'a -> 'b Result.t Symbolic_choice_with_memory.t) :
  'b Result.t Symbolic_choice_with_memory.t =
  match t with
  | Error e -> Symbolic_choice_with_memory.return (Error e)
  | Ok x -> f x

let check_iso ~unsafe export_name export_type module1 module2 =
  let link_state = Lazy.force link_state in
  let*/ _module, link_state =
    Compile.Binary.until_link ~name:(Some module_name1) ~unsafe ~optimize:false
      link_state module1
  in
  let*/ _module, link_state =
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
         { modul = "symbolic"
         ; name = "assert"
         ; assigned_name = Some "assert"
         ; desc = Types.Bt_raw (None, ([ (None, Types.Num_type I32) ], []))
         } )
      iso_modul
  in
  let iso_func =
    let id = Some "check_iso_func" in
    let locals = [] in
    let body =
      let put_on_stack =
        List.mapi
          (fun i (_name, _symbol) ->
            let id = Types.Raw i in
            [ Types.Local_get id ] )
          (fst export_type)
        |> List.flatten
      in

      put_on_stack @ [ Types.Call (Raw idf1) ] @ put_on_stack
      @ [ Types.Call (Raw idf2) ]
      @ ( match snd export_type with
        | [ Types.Num_type I32 ] -> [ Types.I_relop (S32, Eq) ]
        | [ Types.Num_type I64 ] -> [ I_relop (S64, Eq) ]
        | [ Types.Num_type F32 ] -> [ F_relop (S32, Eq) ]
        | [ Types.Num_type F64 ] -> [ F_relop (S64, Eq) ]
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
         { modul = "symbolic"
         ; name = "i32_symbol"
         ; assigned_name = Some "i32_symbol"
         ; desc = Types.Bt_raw (None, ([], [ Types.Num_type I32 ]))
         } )
      iso_modul
  in

  let iso_modul, id_i64_symbol =
    Binary.Module.add_func
      (Runtime.Imported
         { modul = "symbolic"
         ; name = "i64_symbol"
         ; assigned_name = Some "i64_symbol"
         ; desc = Types.Bt_raw (None, ([], [ Types.Num_type I64 ]))
         } )
      iso_modul
  in

  let iso_modul, id_f32_symbol =
    Binary.Module.add_func
      (Runtime.Imported
         { modul = "symbolic"
         ; name = "f32_symbol"
         ; assigned_name = Some "f32_symbol"
         ; desc = Types.Bt_raw (None, ([], [ Types.Num_type F32 ]))
         } )
      iso_modul
  in

  let iso_modul, id_f64_symbol =
    Binary.Module.add_func
      (Runtime.Imported
         { modul = "symbolic"
         ; name = "f64_symbol"
         ; assigned_name = Some "f64_symbol"
         ; desc = Types.Bt_raw (None, ([], [ Types.Num_type F64 ]))
         } )
      iso_modul
  in

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
  Log.debug2 "generated module:@\n  @[<v>%a@]@\n" Text.pp_modul text_modul;
  let*/ m, link_state =
    Compile.Binary.until_link ~unsafe:false ~optimize:false ~name:None
      link_state modul
  in
  let m = Symbolic.convert_module_to_run m in

  let c = Interpret.Symbolic.modul link_state.envs m in

  Symbolic_choice_with_memory.bind (Symbolic_choice_with_memory.return (Ok ()))
    (function
    | Error _ as r -> Symbolic_choice_with_memory.return r
    | Ok () -> c )

module String_set = Set.Make (String)

let cmd ~concolic:_ ~debug ~files ~solver:_ ~unsafe =
  if debug then Log.debug_on := true;
  let* file1, file2 =
    match files with
    | [ file1; file2 ] -> Ok (file1, file2)
    | [] | [ _ ] -> Fmt.error_msg "require at least two modules"
    | _ -> Fmt.error_msg "require at most two modules"
  in

  Log.debug3 "Module %s is %a@\n" module_name1 Fpath.pp file1;
  Log.debug3 "Module %s is %a@\n" module_name2 Fpath.pp file2;

  let compile ~unsafe file =
    Compile.File.until_binary_validate ~unsafe ~rac:false ~srac:false file
  in

  Log.debug2 "Compiling %a@\n" Fpath.pp file1;
  let* module1 = compile ~unsafe file1 in

  Log.debug2 "Compiling %a@\n" Fpath.pp file2;
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

  Log.debug4 "%a exports: %a@\n" Fpath.pp file1
    (Fmt.list ~sep:(fun fmt () -> Fmt.pf fmt " ") Fmt.string)
    exports_name_1;

  Log.debug4 "%a exports: %a@\n" Fpath.pp file2
    (Fmt.list ~sep:(fun fmt () -> Fmt.pf fmt " ") Fmt.string)
    exports_name_2;

  let exports_name_1 = String_set.of_list exports_name_1 in
  let exports_name_2 = String_set.of_list exports_name_2 in

  let common_exports =
    String_set.inter exports_name_1 exports_name_2 |> String_set.to_list
  in

  Log.debug2 "common exports: %a@\n"
    (Fmt.list ~sep:(fun fmt () -> Fmt.pf fmt " ") Fmt.string)
    common_exports;

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
          Log.debug1
            "Removing %s from common exports because they have a different \
             type in each module.@\n"
            name;
          common_exports
        end )
      [] common_exports
  in

  let export_name, export_type =
    match common_exports with hd :: _ -> hd | _ -> Fmt.failwith "TODO"
  in
  let result = check_iso ~unsafe export_name export_type module1 module2 in

  let thread = Thread_with_memory.init () in
  let res_queue = Wq.make () in
  let path_count = ref 0 in
  let to_string = Smtml.Model.to_scfg_string ~no_value:false in
  let callback v =
    let open Symbolic_choice_intf in
    incr path_count;
    match v with
    | ETrap (t, m), thread -> Wq.push (`ETrap (t, m), thread) res_queue
    | EAssert (e, m), thread -> Wq.push (`EAssert (e, m), thread) res_queue
    | _ -> ()
  in
  let join_handles =
    Symbolic_choice_with_memory.run ~workers:2 Smtml.Solver_type.Z3_solver
      result thread ~callback
      ~callback_init:(fun () -> Wq.make_pledge res_queue)
      ~callback_end:(fun () -> Wq.end_pledge res_queue)
  in
  let results =
    Wq.read_as_seq res_queue ~finalizer:(fun () ->
      Array.iter Domain.join join_handles )
  in
  let print_bug = function
    | `ETrap (tr, model) ->
      Fmt.pr "Trap: %s@\n" (Trap.to_string tr);
      Fmt.pr "%s@\n" (to_string model)
    | `EAssert (assertion, model) ->
      Fmt.pr "Assert failure: %a@\n" Smtml.Expr.pp assertion;
      Fmt.pr "%s@\n" (to_string model)
  in
  let print_and_count_failures count_acc results =
    match results () with
    | Seq.Nil -> Ok count_acc
    | Seq.Cons ((result, _thread), _tl) ->
      let* _model =
        match result with
        | (`EAssert (_, model) | `ETrap (_, model)) as bug ->
          print_bug bug;
          Ok model
        | `Error e -> Error e
      in
      let count_acc = succ count_acc in
      Ok count_acc
  in
  let* count = print_and_count_failures 0 results in
  if true then Fmt.pr "Completed paths: %d@." !path_count;
  if count > 0 then Error (`Found_bug count)
  else begin
    Fmt.pr "All OK@.";
    Ok ()
  end
