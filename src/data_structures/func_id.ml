(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Types

type t = int

module IMap = Map.Make (Int)

type 'a collection =
  { c : ('a * binary func_type) IMap.t
  ; last : int
  }

let empty = { c = IMap.empty; last = 0 }

let add f t { last; c } =
  let c = IMap.add last (f, t) c in
  (last, { c; last = succ last })

let get i c =
  match IMap.find_opt i c.c with None -> None | Some (v, _) -> Some v

let get_typ i c =
  match IMap.find_opt i c.c with None -> assert false | Some (_, t) -> t
