(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021 Léo Andrès *)
(* Copyright © 2021 Pierre Chambart *)

type t =
  { mutable value : Value.t
  ; label : string option
  ; mut : Types.mut
  ; typ : Simplified.val_type
  }

let value g = g.value

let set_value g v = g.value <- v

let mut g = g.mut

let typ g = g.typ
