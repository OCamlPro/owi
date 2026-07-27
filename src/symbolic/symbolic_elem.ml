(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module IntMap = Map.Make (Int)

type t = Symbolic_ref.t IntMap.t

let get (elem : t) i : Symbolic_ref.t =
  match IntMap.find_opt i elem with Some v -> v | None -> assert false

let size (elem : t) = IntMap.cardinal elem

let drop (_elem : t) = IntMap.empty

let init l =
  let l = List.mapi (fun i x -> (i, x)) l in
  List.fold_left (fun elem (i, x) -> IntMap.add i x elem) IntMap.empty l
