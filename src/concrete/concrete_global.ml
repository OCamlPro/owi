(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Types

type t =
  { mutable value : Concrete_value.t
  ; label : string option
  ; mut : mut
  ; typ : binary val_type
  }

let value g = g.value

let set_value g v = g.value <- v

let mut g = g.mut

let typ g = g.typ

let backup t = { t with value = t.value }

let recover ~from_ ~to_ = to_.value <- from_.value
