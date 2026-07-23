(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type t =
  { data : Symbolic_ref.t Map.Make(Int).t
  ; limits : Binary.Table.Type.limits
  ; typ : Binary.ref_type
  ; module_id : int
  ; id : int
  }
