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

let print_func (m, i) f =
  match f with
  | Local x ->
    Logs.app (fun log -> log "%a%a : " Fmt.int i pp_id_opt x.id);
    let l = List.fold_left find_functions [] x.body in
    List.iter (fun i -> Logs.app (fun log -> log "- %a" pp_indice i)) l;
    (M.add i l m, i + 1)
  | _ -> (m, i + 1)

let print_graph k l acc =
  List.fold_left
    (fun acc' i ->
      let s' = match i with Raw i -> string_of_int i | _ -> "" in
      String.concat "" [ acc'; string_of_int k; "->"; s'; ";" ] )
    acc l

let pp_call_graph fmt call_graph =
  let s = M.fold print_graph call_graph "" in
  Fmt.pf fmt "digraph call_graph {%s}" s

let cmd_one file =
  let* m =
    Compile.File.until_validate ~unsafe:false ~rac:false ~srac:false file
  in
  ( match m.start with
  | Some i -> Logs.app (fun log -> log "start : %a" Fmt.int i)
  | None -> () );
  let funcs = m.func in

  let call_graph, _ = Array.fold_left print_func (M.empty, 0) funcs in

  let* () =
    Bos.OS.File.writef (Fpath.v "call_graph.dot") "%a" pp_call_graph call_graph
  in

  Ok ()

let cmd ~files = list_iter cmd_one files
