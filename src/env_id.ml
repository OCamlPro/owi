(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021 Léo Andrès *)
(* Copyright © 2021 Pierre Chambart *)

type t = int

module Map = Map.Make (Int)

type 'a collection =
  { c : 'a Map.t
  ; last : int
  }

let empty = { c = Map.empty; last = 0 }

let with_fresh_id { c; last } f : _ Result.t =
  let open Syntax in
  let+ e, r = f last in
  let c = Map.add last e c in
  let last = succ last in
  ({ c; last }, r)

let get i c = Map.find i c.c

module Tbl = Hashtbl.Make (struct
  include Int

  let hash x = x
end)
