(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module type T = sig
  type context

  type value

  type reference

  val expr :
       context
    -> get_const_func:(Binary.indice -> int Result.t)
    -> get_const_global:(Binary.indice -> value Result.t)
    -> Binary.expr
    -> value Result.t

  val ref_expr :
       context
    -> get_const_func:(Binary.indice -> int Result.t)
    -> get_const_global:(Binary.indice -> value Result.t)
    -> Binary.expr
    -> reference Result.t
end
