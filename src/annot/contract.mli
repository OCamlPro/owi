(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Types
open Spec

type 'a t =
  { funcid : 'a indice
  ; preconditions : 'a prop list
  ; postconditions : 'a prop list
  }

val compare_funcid : 'a t -> 'a t -> int

val join_contract : 'a t -> 'a t -> 'a t

val pp_contract : Format.formatter -> 'a t -> unit

val parse_contract : Sexp.t -> text t Result.t
