(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

(* Models *)

type model_format =
  | Scfg
  | Json

(* Test-case generation *)

let out_testcase ~dst testcase =
  let o = Xmlm.make_output ~nl:true ~indent:(Some 2) dst in
  let tag atts name = (("", name), atts) in
  let atts = [ (("", "coversError"), "true") ] in
  let to_string v = Fmt.str "%a" Smtml.Value.pp v in
  let input v = `El (tag [] "input", [ `Data (to_string v) ]) in
  let testcase = `El (tag atts "testcase", List.map input testcase) in
  let dtd =
    {|<!DOCTYPE testcase PUBLIC "+//IDN sosy-lab.org//DTD test-format testcase 1.1//EN" "https://sosy-lab.org/test-format/testcase-1.1.dtd">|}
  in
  Xmlm.output o (`Dtd (Some dtd));
  Xmlm.output_tree Fun.id o testcase

let write_testcase =
  let cnt = ref 0 in
  fun ~dir testcase ->
    incr cnt;
    let name = Fmt.kstr Fpath.v "testcase-%d.xml" !cnt in
    let path = Fpath.append dir name in
    let* res =
      Bos.OS.File.with_oc path
        (fun chan () -> Ok (out_testcase ~dst:(`Channel chan) testcase))
        ()
    in
    res

(* Entry-point *)

let dummy_value_of_t = function
  | Types.Num_type I32 -> Ok (Types.I32_const 0l)
  | Num_type I64 -> Ok (Types.I64_const 0L)
  | Num_type F32 -> Ok (Types.F32_const (Float32.of_float 0.))
  | Num_type F64 -> Ok (Types.F64_const (Float64.of_float 0.))
  | Num_type V128 -> Ok (Types.V128_const (V128.of_i64x2 0L 0L))
  | Ref_type (Types.Null, t) -> Ok (Types.Ref_null t)
  | Ref_type (Types.No_null, t) ->
    Fmt.error_msg "can not create default value of type %a" Types.pp_heap_type t

let default_symbol_of_t m =
  (* TODO: make this lazy to avoid adding unused imports to the module *)
  let modul_name = "symbolic" in
  let i32_symbol, m =
    let func_name = "i32_symbol" in
    let desc = Types.Bt_raw (None, ([], [ Types.Num_type Types.I32 ])) in
    Binary.Module.add_import_if_not_present ~modul_name ~func_name ~desc m
  in
  let i64_symbol, m =
    let func_name = "i64_symbol" in
    let desc = Types.Bt_raw (None, ([], [ Types.Num_type Types.I64 ])) in
    Binary.Module.add_import_if_not_present ~modul_name ~func_name ~desc m
  in
  let f32_symbol, m =
    let func_name = "f32_symbol" in
    let desc = Types.Bt_raw (None, ([], [ Types.Num_type Types.F32 ])) in
    Binary.Module.add_import_if_not_present ~modul_name ~func_name ~desc m
  in
  let f64_symbol, m =
    let func_name = "f64_symbol" in
    let desc = Types.Bt_raw (None, ([], [ Types.Num_type Types.F64 ])) in
    Binary.Module.add_import_if_not_present ~modul_name ~func_name ~desc m
  in
  let v128_symbol, m =
    let func_name = "v128_symbol" in
    let desc = Types.Bt_raw (None, ([], [ Types.Num_type Types.V128 ])) in
    Binary.Module.add_import_if_not_present ~modul_name ~func_name ~desc m
  in
  ( m
  , function
    | Types.Num_type I32 -> Ok (Types.Call i32_symbol)
    | Num_type I64 -> Ok (Types.Call i64_symbol)
    | Num_type F32 -> Ok (Types.Call f32_symbol)
    | Num_type F64 -> Ok (Types.Call f64_symbol)
    | Num_type V128 -> Ok (Types.Call v128_symbol)
    | Ref_type t ->
      Fmt.error_msg "can not create default symbol of type %a" Types.pp_ref_type
        t )

let set_entry_point entry_point invoke_with_symbols (m : Binary.Module.t) =
  (* We are checking if there's a start function *)
  if Option.is_some m.start then
    if Option.is_some entry_point then
      Fmt.error_msg
        "We don't know how to handle a custom entry point when there is a \
         start function for now. Please open a bug report."
    else Ok m
  else
    (* If there is none and we have an entry point passed in argument we search for it *)
    let* export =
      match entry_point with
      | Some entry_point -> begin
        match Binary.Module.find_exported_func_from_name entry_point m with
        | None -> Fmt.error_msg "Entry point %s not found" entry_point
        | Some ep -> Ok ep
      end
      (* If we have no entry point argument then we search for common entry function names *)
      | None ->
        Fmt.error_msg
          "No start function found, no default entry point found and no custom \
           entry point was provided"
    in
    (* We found an entry point, so we check its type and build a start function that put the right values on the stack (be it symbols or dummy values), call the entry function and drop the result. *)
    match Binary.Module.get_func_type export.id m with
    | None -> Fmt.error_msg "can't find a main function"
    | Some (Bt_raw main_type) ->
      let m, default_symbol_of_t = default_symbol_of_t m in
      let+ body =
        let pt, rt = snd main_type in
        let+ args =
          list_map
            (fun (_, t) ->
              let default =
                if invoke_with_symbols then default_symbol_of_t
                else dummy_value_of_t
              in
              default t )
            pt
        in
        let after_call =
          List.map (fun (_ : _ Types.val_type) -> Types.Drop) rt
        in
        args @ [ Types.Call (Raw export.id) ] @ after_call
      in
      let type_f : Types.binary Types.block_type =
        Types.Bt_raw (None, ([], []))
      in
      let start_code : Types.binary Types.func =
        { Types.type_f; locals = []; body; id = None }
      in
      let start_func = Runtime.Local start_code in

      let m, start_index = Binary.Module.add_func start_func m in
      let start = Some start_index in
      { m with start }

(* Installed files  *)

let c_files_location = List.map Fpath.v Share.Sites.c_files

let rust_files_location = List.map Fpath.v Share.Sites.rust_files

let zig_files_location = List.map Fpath.v Share.Sites.zig_files

let find location file : Fpath.t Result.t =
  let* l =
    list_map
      (fun dir ->
        let filename = Fpath.append dir file in
        match Bos.OS.File.exists filename with
        | Ok true -> Ok (Some filename)
        | Ok false -> Ok None
        | Error _ as e -> e )
      location
  in
  let rec loop = function
    | [] -> Fmt.error_msg "can't find file %a" Fpath.pp file
    | None :: tl -> loop tl
    | Some file :: _tl -> Ok file
  in
  loop l

let find_installed_c_file filename = find c_files_location filename

let find_installed_rust_file filename = find rust_files_location filename

let find_installed_zig_file filename = find zig_files_location filename
