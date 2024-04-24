(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type ('a, 'b) t =
  | Local of 'a
  | Imported of 'b Imported.t
