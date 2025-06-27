(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax
open Types

let rec find_children acc (e : binary instr) : int list =
  match e with
  | Call (Raw i) | Return_call (Raw i) -> i :: acc
  | Block (_, _, exp) -> List.fold_left find_children acc exp
  | Loop (_, _, exp) -> List.fold_left find_children acc exp
  | If_else (_, _, e1, e2) ->
    let l1 = List.fold_left find_children acc e1 in
    List.fold_left find_children l1 e2
  | _ -> acc

let build_graph (g, i) f =
  match f with
  | Runtime.Local x ->
    Logs.app (fun log -> log "%a%a : " Fmt.int i pp_id_opt x.id);
    let l = List.sort_uniq compare (List.fold_left find_children [] x.body) in
    List.iter (fun i -> Logs.app (fun log -> log "- %a" Fmt.int i)) l;
    (Graph.add_edges g i x.id l, i + 1)
  | _ -> (g, i + 1)

let find_entry_points (m : Binary.Module.t) =
  let l = Option.to_list m.start in
  List.fold_left (fun acc (x : Binary.export) -> x.id :: acc) l m.exports.func

let cmd ~source_file ~entry_point =
  let* m =
    Compile.File.until_validate ~unsafe:false ~rac:false ~srac:false source_file
  in
  let funcs = m.func in

  let call_graph, _ = Array.fold_left build_graph (Graph.empty, 0) funcs in
  let entries =
    Option.value
      (Option.bind
         (Option.bind entry_point (fun e -> Graph.find_indice call_graph e))
         (fun x -> Some [ x ]) )
      ~default:(find_entry_points m)
  in
  let call_graph = Graph.set_entry_points call_graph entries in

  let* () =
    Bos.OS.File.writef (Fpath.v "call_graph.dot") "%a" Graph.pp_dot call_graph
  in

  Ok ()
