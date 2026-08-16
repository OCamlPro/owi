(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

open Smtml
open Fmt

type t = int

let of_expr (e : Expr.t) : t =
  Hashtbl.hash (Expr.to_string e)
let equal a b = a = b
let hash a = a
let pp fmt h = pf fmt "Hash(%d)" h