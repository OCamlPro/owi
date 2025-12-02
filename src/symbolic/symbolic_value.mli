(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

include
  Value_intf.T
    with type boolean = Smtml.Expr.t
     and type i32 = Smtml.Expr.t
     and type i64 = Smtml.Expr.t
     and type f32 = Smtml.Expr.t
     and type f64 = Smtml.Expr.t
     and type v128 = Smtml.Expr.t

module Boolean : sig
  include module type of Boolean

  val select_expr :
       Smtml.Expr.t
    -> if_true:Smtml.Expr.t
    -> if_false:Smtml.Expr.t
    -> Smtml.Expr.t
end
