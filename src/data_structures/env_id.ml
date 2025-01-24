(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type t = int

module Map = Map.Make (Int)

type 'a collection =
  { c : 'a Map.t
  ; last : int
  }

let empty = { c = Map.empty; last = 0 }

let with_fresh_id f { c; last } =
  let open Syntax in
  let+ e, r = f last in
  let c = Map.add last e c in
  let last = succ last in
  ({ c; last }, r)

let get i c = match Map.find_opt i c.c with None -> assert false | Some v -> v

let map f c = { c with c = Map.map f c.c }

module Tbl = Hashtbl.Make (struct
  include Int

  let hash x = x
end)
