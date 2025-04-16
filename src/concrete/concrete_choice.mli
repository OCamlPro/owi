(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(* TODO: type 'a t should be abstract, run will be needed for this *)
include
  Choice_intf.Base with type 'a t = 'a Result.t and module V := Concrete_value

val run : 'a t -> 'a t
