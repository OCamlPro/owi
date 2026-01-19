(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type t =
  { value : Symbolic_value.t
  ; env_id : int
  ; id : int
  }
