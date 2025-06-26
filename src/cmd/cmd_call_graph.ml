(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax
open Types
open Runtime
module M = Map.Make (Int)

let rec find_functions acc e =
  match e with
  | Call i ->
    if List.exists (fun x -> compare_indice i x = 0) acc then acc else i :: acc
  | Block (_, _, exp) -> List.fold_left find_functions acc exp
  | Loop (_, _, exp) -> List.fold_left find_functions acc exp
  | If_else (_, _, e1, e2) ->
    let l1 = List.fold_left find_functions acc e1 in
    List.fold_left find_functions l1 e2
  | _ -> acc

let build_map (m, i) f =
  match f with
  | Local x ->
    Logs.app (fun log -> log "%a%a : " Fmt.int i pp_id_opt x.id);
    let l = List.fold_left find_functions [] x.body in
    List.iter (fun i -> Logs.app (fun log -> log "- %a" pp_indice i)) l;
    (M.add i l m, i + 1)
  | _ -> (m, i + 1)

let rec build_graph (call_graph, acc) k =
  match M.find_opt k call_graph with
  | None -> (call_graph, acc)
  | Some l ->
    let call_graph = M.remove k call_graph in

    List.fold_left
      (fun (cg, acc) i ->
        let cg', acc', s' =
          match i with
          | Raw i ->
            let cg', acc' = build_graph (cg, acc) i in
            (cg', acc', string_of_int i)
          | _ -> (cg, acc, "")
        in
        (cg', String.concat "" [ acc'; string_of_int k; "->"; s'; ";\n" ]) )
      (call_graph, acc) l

let pp_call_graph fmt (call_graph, entries) =
  let _, s = List.fold_left build_graph (call_graph, "") entries in
  Fmt.pf fmt "digraph call_graph {\n%s}" s

let find_entry_points (m : Binary.Module.t) =
  let l = Option.to_list m.start in
  List.fold_left (fun acc (x : Binary.export) -> x.id :: acc) l m.exports.func

let cmd_one entry_point file =
  let* m =
    Compile.File.until_validate ~unsafe:false ~rac:false ~srac:false file
  in
  let funcs = m.func in

  let call_graph, _ = Array.fold_left build_map (M.empty, 0) funcs in
  let entries =
    Option.value
      (Option.bind
         (Option.bind entry_point (fun x ->
            Array.find_index
              (fun f ->
                match f with
                | Local y -> String.equal x (Option.value y.id ~default:"")
                | _ -> false )
              funcs ) )
         (fun x -> Some [ x ]) )
      ~default:(find_entry_points m)
  in

  let* () =
    Bos.OS.File.writef (Fpath.v "call_graph.dot") "%a" pp_call_graph
      (call_graph, entries)
  in

  Ok ()

let cmd ~files ~entry_point = list_iter (cmd_one entry_point) files
