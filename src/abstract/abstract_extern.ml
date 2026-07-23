(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module Func =
  Extern.Func.Make (Abstract_value) (Abstract_monad) (Abstract_memory)

module Module = struct
  type t = (string * Func.t) list
end
