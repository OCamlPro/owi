(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(** runtime table *)

(* TODO: make private and even opaque! *)
type table = Concrete_ref.t array

(* TODO: make private and even opaque! *)
type t =
  { id : int
  ; label : string option
  ; limits : Text.limits
  ; typ : Text.ref_type
  ; mutable data : table
  }

include
  Table_intf.T
    with type reference := Concrete_ref.t
     and type t := t
     and type 'a choice := 'a Concrete_choice.t

val init : ?label:string -> Text.Table.Type.t -> t
