(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(** the types of imported values *)
type 'a t =
  { modul : string
  ; name : string
  ; assigned_name : string option
  ; typ : 'a
  }

val pp : 'a Fmt.t -> Format.formatter -> 'a t -> unit
