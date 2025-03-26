(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let module_name1 = "owi_iso_module1"

let module_name2 = "owi_iso_module2"

let check_iso ~unsafe export_name export_type module1 module2 =
  let link_state = Link.empty_state in
  let* _module, link_state =
    Compile.Binary.until_link ~name:(Some module_name1) ~unsafe ~optimize:false
      link_state module1
  in
  let* _module, _link_state =
    Compile.Binary.until_link ~name:(Some module_name2) ~unsafe ~optimize:false
      link_state module2
  in

  let iso_modul = Binary.Module.empty in
  let func1 =
    Runtime.Imported
      { modul = module_name1
      ; name = export_name
      ; assigned_name = Some "iso_func1"
      ; desc = Types.Bt_raw (None, export_type)
      }
  in
  let iso_modul, _idf1 = Binary.Module.add_func func1 iso_modul in
  let func2 =
    Runtime.Imported
      { modul = module_name2
      ; name = export_name
      ; assigned_name = Some "iso_func2"
      ; desc = Types.Bt_raw (None, export_type)
      }
  in
  let iso_modul, _idf2 = Binary.Module.add_func func2 iso_modul in
  let iso_func =
    let id = Some "iso_func" in
    let body = [] in
    let locals = [] in
    let type_f = Types.Bt_raw (None, ([], [])) in
    Runtime.Local { Types.type_f; locals; body; id }
  in
  let iso_modul, index = Binary.Module.add_func iso_func iso_modul in
  let start = Some index in
  let modul = { iso_modul with start } in
  let modul = Binary_to_text.modul modul in
  Log.debug2 "generated module:@\n  @[<v>%a@]@\n" Text.pp_modul modul;
  Ok ()

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

  let* () =
    list_iter
      (fun (export_name, export_type) ->
        check_iso ~unsafe export_name export_type module1 module2 )
      common_exports
  in

  Ok ()
