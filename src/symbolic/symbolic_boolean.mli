(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

include Boolean_intf.T with type t = Smtml.Typed.Bool.t

val ite : t -> 'a Smtml.Typed.expr -> 'a Smtml.Typed.expr -> 'a Smtml.Typed.expr
