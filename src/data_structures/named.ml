(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(** named values (fields) *)
type 'a t =
  { values : 'a Dynarray.t
  ; named : (string, int) Hashtbl.t
  }

let get_at { values; _ } i =
  if i >= Dynarray.length values then None else Some (Dynarray.get values i)

let get_by_name { named; _ } name = Hashtbl.find_opt named name

let create values named = { values; named }

let fold f v acc =
  snd
  @@ Dynarray.fold_left
       (fun (i, acc) v -> (succ i, f i v acc))
       (0, acc) v.values

let map f v =
  let values = Dynarray.map f v.values in
  { v with values }

let monadic_map f v =
  let open Syntax in
  let+ values = dynarray_map f v.values in
  { v with values }

let to_array v = Dynarray.to_array v.values

let pp_values pp_v fmt values =
  Fmt.pf fmt "[%a]"
    (Fmt.iter ~sep:(fun fmt () -> Fmt.pf fmt " ; ") Dynarray.iter pp_v)
    values

let pp_named fmt named =
  Fmt.iter_bindings
    ~sep:(fun fmt () -> Fmt.pf fmt " ; ")
    Hashtbl.iter
    (fun fmt (name, n) -> Fmt.pf fmt "(%S, %d)" name n)
    fmt named

let pp pp_v fmt { values; named } =
  Fmt.pf fmt "{@\n  @[<v>values: %a@\nnamed: %a@]}" (pp_values pp_v) values
    pp_named named
