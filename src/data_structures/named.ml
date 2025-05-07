(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(** named values (fields) *)
type 'a t =
  { values : 'a Indexed.t list
  ; named : int String_map.t
  }

let empty = { values = []; named = String_map.empty }

let create values named = { values; named }

let fold f v acc =
  List.fold_left
    (fun acc v -> f (Indexed.get_index v) (Indexed.get v) acc)
    acc v.values

let map f v =
  let values = List.map f v.values in
  { v with values }

let to_array v = Indexed.list_to_array v.values

let pp_values pp_v fmt values = Indexed.pp_list pp_v fmt values

let pp_named fmt named =
  Fmt.iter_bindings
    ~sep:(fun fmt () -> Fmt.pf fmt " ; ")
    String_map.iter
    (fun fmt (name, n) -> Fmt.pf fmt "(%S, %d)" name n)
    fmt named

let pp pp_v fmt { values; named } =
  Fmt.pf fmt "{@\n  @[<v>values: %a@\nnamed: %a@]}" (pp_values pp_v) values
    pp_named named
