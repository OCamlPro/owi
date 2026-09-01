(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module type T = sig
  type context

  type value

  type reference

  val default_gc_val : Binary.storage_type -> value

  val expr :
       context
    -> get_const_type:(Binary.indice -> Binary.sub_type)
    -> get_const_func:(Binary.indice -> int)
    -> get_const_global:(Binary.indice -> value)
    -> Binary.expr
    -> value

  val ref_expr :
       context
    -> get_const_type:(Binary.indice -> Binary.sub_type)
    -> get_const_func:(Binary.indice -> int)
    -> get_const_global:(Binary.indice -> value)
    -> Binary.expr
    -> reference
end
