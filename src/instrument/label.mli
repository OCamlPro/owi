(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module Coverage_criteria : sig
  type t =
    | Function_coverage
    | Statement_coverage
    | Decision_coverage

  val of_string : string -> (t, [ `Msg of string ]) Prelude.Result.t

  val pp : t Fmt.t
end

val annotate : Coverage_criteria.t -> Binary.Module.t -> Binary.Module.t
