(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let out_testcase ~dst testcase =
  let o = Xmlm.make_output ~nl:true ~indent:(Some 2) dst in
  let tag atts name = (("", name), atts) in
  let atts = [ (("", "coversError"), "true") ] in
  let to_string v = Fmt.str "%a" Smtml.Value.pp_num v in
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

let add_main_as_start (m : Binary.modul) =
  (* We are checking if there's a start function *)
  if Option.is_some m.start then Ok m
  else
    (* If there is none, we look for a function exported with the name `main` *)
    match
      List.find_opt
        (function { Binary.name = "main"; _ } -> true | _ -> false)
        m.exports.func
    with
    | None ->
      (* TODO: fail/display a warning saying nothing will be done ? *)
      Ok m
    | Some export -> (
      (* We found a main function, so we check its type and build a start function that put the right values on the stack, call the main function and drop the results *)
      let main_id = export.id in
      match Indexed.get_at main_id m.func.values with
      | None -> Error (`Msg "can't find a main function")
      | Some main_function ->
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
            Error
              (`Msg
                (Fmt.str "can not create default value of type %a"
                   Types.pp_heap_type t ) )
        in
        let+ body =
          let pt, rt = snd main_type in
          let+ args = list_map (fun (_, t) -> default_value_of_t t) pt in
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
        let named = m.func.named in
        (* We need to add the new start function to the funcs of the module at the next free index *)
        let next_free_index =
          List.fold_left
            (fun next_free_index v ->
              let index = Indexed.get_index v in
              if next_free_index > index then next_free_index else index + 1 )
            0 m.func.values
        in
        let values =
          Indexed.return next_free_index start_func :: m.func.values
        in
        let func = { Named.named; values } in
        let start = Some next_free_index in
        { m with func; start } )
