(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type t = { mutable value : Symbolic_value.t }

let value v = v.value

let set_value v x = v.value <- x

(* WARNING: because we are doing an optimization in `Symbolic_choice`, the cloned state should not refer to a mutable value of the previous state. Assuming that the original state is not mutated is wrong. *)
let clone_global r = { value = r.value }

let convert_values (v : Concrete_value.t) : Symbolic_value.t =
  (* TODO share various versions *)
  match v with
  | I32 v -> I32 (Symbolic_value.const_i32 v)
  | I64 v -> I64 (Symbolic_value.const_i64 v)
  | F32 v -> F32 (Symbolic_value.const_f32 v)
  | F64 v -> F64 (Symbolic_value.const_f64 v)
  | V128 v -> V128 (Symbolic_value.const_v128 v)
  | Ref (Funcref f) -> Ref (Funcref f)
  | Ref _ -> assert false

let convert (orig_global : Concrete_global.t) : t =
  let value = convert_values orig_global.value in
  { value }

(** Collections of globals *)

type collection = (int * int, t) Hashtbl.t

let init () = Hashtbl.create 16

let clone collection =
  let collection' = init () in
  Hashtbl.iter
    (fun loc global ->
      let global' = clone_global global in
      Hashtbl.add collection' loc global' )
    collection;
  collection'

let get_global env_id (orig_global : Concrete_global.t) collection g_id =
  let loc = (env_id, g_id) in
  match Hashtbl.find_opt collection loc with
  | None ->
    let g = convert orig_global in
    Hashtbl.add collection loc g;
    g
  | Some t -> t
