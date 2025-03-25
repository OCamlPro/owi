(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

(* Models *)

type model_output_format =
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

let find_exported_name exported_names (m : Binary.modul) =
  List.find_opt
    (function
      | { Binary.name; _ } when List.mem name exported_names -> true
      | _ -> false )
    m.exports.func

let find_imported_func modul_name func_name (m : Binary.modul) =
  Array.find_index
    (function
      | Runtime.Imported { Imported.modul; name; assigned_name = _; desc = _ }
        when String.equal modul_name modul && String.equal func_name name ->
        true
      | Local _ | Imported _ -> false )
    m.func

let set_entry_point entry_point invoke_with_symbols (m : Binary.modul) =
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
        match find_exported_name [ entry_point ] m with
        | None -> Fmt.error_msg "Entry point %s not found\n" entry_point
        | Some ep -> Ok ep
      end
      (* If we have no entry point argument then we search for common entry function names *)
      | None ->
        let possible_names = [ "main"; "_start" ] in
        begin
          match find_exported_name possible_names m with
          | Some entry_point -> Ok entry_point
          | None ->
            Fmt.error_msg "No entry point found, tried: %a\n"
              (Fmt.list ~sep:(fun fmt () -> Fmt.pf fmt ", ") Fmt.string)
              possible_names
        end
    in
    (* We found an entry point, so we check its type and build a start function that put the right values on the stack,
       call the entry function and drop the results *)
    let main_id = export.id in
    if main_id >= Array.length m.func then
      Fmt.error_msg "can't find a main function"
    else
      let main_function = m.func.(main_id) in
      let (Bt_raw main_type) =
        match main_function with Local f -> f.type_f | Imported i -> i.desc
      in
      let default_value_of_t = function
        | Types.Num_type I32 -> Ok (Types.I32_const 0l)
        | Num_type I64 -> Ok (Types.I64_const 0L)
        | Num_type F32 -> Ok (Types.F32_const (Float32.of_float 0.))
        | Num_type F64 -> Ok (Types.F64_const (Float64.of_float 0.))
        | Ref_type (Types.Null, t) -> Ok (Types.Ref_null t)
        | Ref_type (Types.No_null, t) ->
          Fmt.error_msg "can not create default value of type %a"
            Types.pp_heap_type t
      in
      let i32_symbol, m =
        match find_imported_func "symbolic" "i32_symbol" m with
        | None ->
          let func = m.func in
          let f =
            Runtime.Imported
              { Imported.modul = "symbolic"
              ; name = "i32_symbol"
              ; assigned_name = None
              ; desc = Types.Bt_raw (None, ([], [ Types.Num_type Types.I32 ]))
              }
          in
          let func = Array.append func [| f |] in
          let m = { m with func } in
          let i = Array.length func - 1 in
          (Types.Raw i, m)
        | Some i -> (Types.Raw i, m)
      in
      let i64_symbol, m =
        match find_imported_func "symbolic" "i64_symbol" m with
        | None ->
          let func = m.func in
          let f =
            Runtime.Imported
              { Imported.modul = "symbolic"
              ; name = "i64_symbol"
              ; assigned_name = None
              ; desc = Types.Bt_raw (None, ([], [ Types.Num_type Types.I64 ]))
              }
          in
          let func = Array.append func [| f |] in
          let m = { m with func } in
          let i = Array.length func - 1 in
          (Types.Raw i, m)
        | Some i -> (Types.Raw i, m)
      in
      let f32_symbol, m =
        match find_imported_func "symbolic" "f32_symbol" m with
        | None ->
          let func = m.func in
          let f =
            Runtime.Imported
              { Imported.modul = "symbolic"
              ; name = "f32_symbol"
              ; assigned_name = None
              ; desc = Types.Bt_raw (None, ([], [ Types.Num_type Types.F32 ]))
              }
          in
          let func = Array.append func [| f |] in
          let m = { m with func } in
          let i = Array.length func - 1 in
          (Types.Raw i, m)
        | Some i -> (Types.Raw i, m)
      in
      let f64_symbol, m =
        match find_imported_func "symbolic" "f64_symbol" m with
        | None ->
          let func = m.func in
          let f =
            Runtime.Imported
              { Imported.modul = "symbolic"
              ; name = "f64_symbol"
              ; assigned_name = None
              ; desc = Types.Bt_raw (None, ([], [ Types.Num_type Types.F64 ]))
              }
          in
          let func = Array.append func [| f |] in
          let m = { m with func } in
          let i = Array.length func - 1 in
          (Types.Raw i, m)
        | Some i -> (Types.Raw i, m)
      in
      let default_symbol_of_t = function
        | Types.Num_type I32 -> Ok (Types.Call i32_symbol)
        | Num_type I64 -> Ok (Types.Call i64_symbol)
        | Num_type F32 -> Ok (Types.Call f32_symbol)
        | Num_type F64 -> Ok (Types.Call f64_symbol)
        | Ref_type t ->
          Fmt.error_msg "can not create default symbol of type %a"
            Types.pp_ref_type t
      in
      let+ body =
        let pt, rt = snd main_type in
        let+ args =
          list_map
            (fun (_, t) ->
              let default =
                if invoke_with_symbols then default_symbol_of_t
                else default_value_of_t
              in
              default t )
            pt
        in
        let after_call =
          List.map (fun (_ : _ Types.val_type) -> Types.Drop) rt
        in
        args @ [ Types.Call (Raw main_id) ] @ after_call
      in
      let type_f : Types.binary Types.block_type =
        Types.Bt_raw (None, ([], []))
      in
      let start_code : Types.binary Types.func =
        { Types.type_f; locals = []; body; id = None }
      in
      let start_func = Runtime.Local start_code in

      (* We need to add the new start function to the funcs of the module at the next free index *)
      let func =
        Array.init
          (Array.length m.func + 1)
          (fun i -> if i = Array.length m.func then start_func else m.func.(i))
      in

      let start = Some (Array.length m.func) in
      { m with func; start }

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
