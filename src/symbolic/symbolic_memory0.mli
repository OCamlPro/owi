(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type t =
  { data : Symbolic_i32.t Map.Make(Int32).t
  ; chunks : Symbolic_i32.t Map.Make(Int32).t
  ; size : Symbolic_i32.t
  ; env_id : int
  ; id : int
  }
