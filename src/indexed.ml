(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021 Léo Andrès *)
(* Copyright © 2021 Pierre Chambart *)

type 'a t =
  { index : int
  ; value : 'a
  }

let get v = v.value

let get_index v = v.index

let map f v = { index = v.index; value = f v.value }

let return index value = { index; value }

let has_index idx { index; _ } = idx = index

let get_at_exn i values =
  let { value; _ } = List.find (has_index i) values in
  value

let get_at i values =
  match List.find_opt (has_index i) values with
  | None -> None
  | Some { value; _ } -> Some value

let pp f fmt v = f fmt v.value
