(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module IMap = Map.Make (Int)

(** Single table *)

type table =
  { data : Symbolic_ref.t IMap.t
  ; limits : Text.limits
  ; typ : Text.ref_type
  ; env_id : int
  ; id : int
  }

let convert_ref_values (v : Concrete_ref.t) : Symbolic_ref.t =
  match v with Func f -> Func f | _ -> assert false

let table_of_concrete ~env_id ~id (original : Concrete_table.t) =
  let _i, data =
    Array.fold_left
      (fun (i, map) v ->
        let v = convert_ref_values v in
        let map = IMap.add i v map in
        (succ i, map) )
      (0, IMap.empty) original.data
  in
  { data; limits = original.limits; typ = original.typ; env_id; id }

(** Collection of tables *)

module M = struct
  type symbolic = table

  let clone_one = Fun.id
end

include Collection.Make (M)
