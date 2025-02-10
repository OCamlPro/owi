(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type t = Smtml.Expr.Set.t

let empty = Smtml.Expr.Set.empty

(* TODO: should we make some normalization here? *)
(* TODO: is it the good place for simplification? *)
let add pc c =
  let c = Smtml.Expr.simplify c in
  Smtml.Expr.Set.add c pc

let to_list pc = Smtml.Expr.Set.to_list pc
