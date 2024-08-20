(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(** named values (fields) *)
type 'a t =
  { values : 'a Indexed.t list
  ; named : int String_map.t
  }

let empty = { values = []; named = String_map.empty }

let fold f v acc =
  List.fold_left
    (fun acc v -> f (Indexed.get_index v) (Indexed.get v) acc)
    acc v.values

let map f v =
  let values = List.map f v.values in
  { v with values }

let to_array v =
  let tbl = Hashtbl.create 512 in
  List.iter
    (fun v ->
      let i = Indexed.get_index v in
      let v = Indexed.get v in
      if Hashtbl.mem tbl i then assert false else Hashtbl.add tbl i v )
    v.values;
  Array.init (List.length v.values) (fun i ->
      match Hashtbl.find_opt tbl i with None -> assert false | Some v -> v )
