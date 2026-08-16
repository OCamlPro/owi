(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

open Smtml

type t = {
  table : (Hash_footprint.t, Expr.t list list) Hashtbl.t;
  mutable lookups : int;
  mutable hits : int;
}

let create () =
  { table = Hashtbl.create 1024; lookups = 0; hits = 0 }

(* let core_to_expr core =
  match core with
  | [] -> assert false
  | [c] -> c
  | hd :: tl -> List.fold_left (fun acc c -> Expr.Bool.and_ acc c) hd tl *)

let add cache fp core =
  match Hashtbl.find_opt cache.table fp with
  | None -> Hashtbl.add cache.table fp [core]
  | Some cores -> Hashtbl.replace cache.table fp (core :: cores)

let lookup cache formula =
  cache.lookups <- cache.lookups + 1;
  let fp = Hash_footprint.of_expr formula in
  let fp_hash = Hash_footprint.hash fp in
  Logs.debug (fun m -> m "lookup: fp_hash=%d" fp_hash);
  match Hashtbl.find_opt cache.table fp with
  | None ->
      None
  | Some cores ->
      match cores with
      | core :: _ ->
          cache.hits <- cache.hits + 1;
          Some core
      | [] -> None

let clear cache =
  Hashtbl.clear cache.table;
  cache.lookups <- 0;
  cache.hits <- 0

let stats cache = (Hashtbl.length cache.table, cache.lookups, cache.hits)