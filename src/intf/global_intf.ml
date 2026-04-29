(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module type T = sig
  type t

  type value

  type 'a choice

  val value : t -> value

  val set_value : t -> value -> unit choice
end
