(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(** Single table *)

type t =
  { mutable data : Symbolic_ref.t array
  ; limits : Text.limits
  ; typ : Text.ref_type
  }

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

(** Collection of tables *)

let convert_ref_values (v : Concrete_ref.t) : Symbolic_ref.t =
  match v with Func f -> Func f | _ -> assert false

let of_concrete (original : Concrete_table.t) =
  { data = Array.map convert_ref_values original.data
  ; limits = original.limits
  ; typ = original.typ
  }

module M = struct
  type concrete = Concrete_table.t

  type symbolic = t

  let convert_one (original : Concrete_table.t) = of_concrete original

  (* WARNING: because we are doing an optimization in `Symbolic_choice`, the cloned state should not refer to a mutable value of the previous state. Assuming that the original state is not mutated is wrong. *)
  let clone_one { limits; data; typ } = { typ; limits; data = Array.copy data }
end

module Collection = Collection.Make (M)
