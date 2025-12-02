(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(** runtime global *)

(* TODO: make it private and even opaque later! *)
type t =
  { mutable value : Concrete_value.t
  ; mut : Text.mut
  ; typ : Text.val_type
  }

include Global_intf.T with module Value := Concrete_value and type t := t
