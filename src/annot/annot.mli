(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Fmt

type t =
  { annotid : string
  ; items : Sexp.t
  }

type 'a annot =
  | Contract of 'a Contract.t
  | Annot of t

val pp_annot : formatter -> 'a annot -> unit

val record_annot : string -> Sexp.t -> unit

val get_annots : unit -> t list
