(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(* TODO: mut and typ are only used at link time but not at runtime, it could be possible to remove them with some effort! *)
type t =
  { mutable value : Concrete_value.t
  ; mut : Binary.mut
  ; typ : Binary.val_type
  }

let value g = g.value

let set_value g v = g.value <- v

let backup t = { t with value = t.value }

let recover ~from_ ~to_ = to_.value <- from_.value
