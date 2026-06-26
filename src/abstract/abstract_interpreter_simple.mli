(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type t = Abstract_state.t option * Binary.instr Annotated.t option

val eval_instr : Abstract_state.t -> Binary.instr Annotated.t -> t
