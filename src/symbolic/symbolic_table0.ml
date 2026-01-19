(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module IMap = Map.Make (Int)

type t =
  { data : Symbolic_ref.t IMap.t
  ; limits : Text.limits
  ; typ : Text.ref_type
  ; env_id : int
  ; id : int
  }
