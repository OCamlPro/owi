(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type ('a, 'b) t =
  | Local of 'a
  | Imported of 'b Imported.t

let pp ~pp_local ~pp_imported fmt = function
  | Local local -> Fmt.pf fmt "Local (%a)" pp_local local
  | Imported imported ->
    Fmt.pf fmt "Imported (%a)" (Imported.pp pp_imported) imported
