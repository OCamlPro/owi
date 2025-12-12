(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(** Single table *)

type t =
  { mutable data : Symbolic_ref.t array
  ; limits : Text.limits
  ; typ : Text.ref_type
  }

(* WARNING: because we are doing an optimization in `Symbolic_choice`, the cloned state should not refer to a mutable value of the previous state. Assuming that the original state is not mutated is wrong. *)
let clone_table { limits; data; typ } = { typ; limits; data = Array.copy data }

let get t i = t.data.(i)

let set t i v = t.data.(i) <- v

let size t = Array.length t.data

let typ t = t.typ

let max_size t = t.limits.max

let grow t new_size x =
  let new_size = Int32.to_int new_size in
  let new_table = Array.make new_size x in
  Array.blit t.data 0 new_table 0 (Array.length t.data);
  t.data <- new_table

let fill t pos len x =
  let pos = Int32.to_int pos in
  let len = Int32.to_int len in
  Array.fill t.data pos len x

let copy ~t_src ~t_dst ~src ~dst ~len =
  let src = Int32.to_int src in
  let dst = Int32.to_int dst in
  let len = Int32.to_int len in
  Array.blit t_src.data src t_dst.data dst len

let convert_ref_values (v : Concrete_ref.t) : Symbolic_ref.t =
  match v with Func f -> Func f | _ -> assert false

let convert (orig_table : Concrete_table.t) =
  { data = Array.map convert_ref_values orig_table.data
  ; limits = orig_table.limits
  ; typ = orig_table.typ
  }

(** Collection of tables *)

type collection = (int * int, t) Hashtbl.t

let init () = Hashtbl.create 16

let clone collection =
  let collection' = init () in
  Hashtbl.iter
    (fun loc table ->
      let table' = clone_table table in
      Hashtbl.add collection' loc table' )
    collection;
  collection'

let get_table env_id (orig_table : Concrete_table.t) collection g_id =
  let loc = (env_id, g_id) in
  match Hashtbl.find_opt collection loc with
  | None ->
    let g = convert orig_table in
    Hashtbl.add collection loc g;
    g
  | Some t -> t
