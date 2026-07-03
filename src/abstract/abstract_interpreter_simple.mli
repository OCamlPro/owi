(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type t =
  | State of Abstract_state.t
  | Unreachable

val eval_instr : Abstract_state.t -> Binary.instr Annotated.t -> t
