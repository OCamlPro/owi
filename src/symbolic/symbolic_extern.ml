(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module Func =
  Extern.Func.Make (Symbolic_value) (Symbolic_choice) (Symbolic_memory)

module Module = struct
  type t = (string * Func.t) list
end
