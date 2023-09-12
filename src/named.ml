(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021 Léo Andrès *)
(* Copyright © 2021 Pierre Chambart *)

open Indexed

(** named values (fields) *)
type 'a t =
  { values : 'a Indexed.t list
  ; named : int String_map.t
  }

let fold f v acc =
  List.fold_left (fun acc v -> f v.index v.value acc) acc v.values

let iter f v = List.iter (fun v -> f v.index v.value) v.values

let map f v =
  let values = List.map f v.values in
  { v with values }
