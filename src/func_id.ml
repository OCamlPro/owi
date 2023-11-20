(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021 Léo Andrès *)
(* Copyright © 2021 Pierre Chambart *)

open Types

type t = int

module IMap = Map.Make (Int)

type 'a collection =
  { c : ('a * simplified func_type) IMap.t
  ; last : int
  }

let empty = { c = IMap.empty; last = 0 }

let add f t c = (c.last, { c = IMap.add c.last (f, t) c.c; last = c.last + 1 })

let get i c =
  let v, _ = IMap.find i c.c in
  v

let get_typ i c =
  let _, t = IMap.find i c.c in
  t

let pp ppf i = Format.fprintf ppf "f_%i" i
