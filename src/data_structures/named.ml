(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021 Léo Andrès *)
(* Copyright © 2021 Pierre Chambart *)

(** named values (fields) *)
type 'a t =
  { values : 'a Indexed.t list
  ; named : int String_map.t
  }

let fold f v acc =
  List.fold_left
    (fun acc v -> f (Indexed.get_index v) (Indexed.get v) acc)
    acc v.values

let map f v =
  let values = List.map f v.values in
  { v with values }
