(*****************************************************************************)
(*                                                                           *)
(*  Owi                                                                      *)
(*                                                                           *)
(*  Copyright (C) 2021-2024 OCamlPro                                         *)
(*  Written by Léo Andrès and Pierre Chambart                                *)
(*                                                                           *)
(*  SPDX-License-Identifier: AGPL-3.0-or-later                               *)
(*                                                                           *)
(*  This program is free software: you can redistribute it and/or modify     *)
(*  it under the terms of the GNU Affero General Public License as published *)
(*  by the Free Software Foundation, either version 3 of the License, or     *)
(*  (at your option) any later version.                                      *)
(*                                                                           *)
(*  This program is distributed in the hope that it will be useful,          *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(*  GNU Affero General Public License for more details.                      *)
(*                                                                           *)
(*  You should have received a copy of the GNU Affero General Public License *)
(*  along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
(*                                                                           *)
(*****************************************************************************)

type externref

type ref_value =
  | Funcref of Func_intf.t option
  | Externref of externref option

include
  Value_intf.T
    with type ref_value := ref_value
    with type vbool = Encoding.Expr.t
     and type int32 = Encoding.Expr.t
     and type int64 = Encoding.Expr.t
     and type float32 = Encoding.Expr.t
     and type float64 = Encoding.Expr.t

module Bool : sig
  include module type of Bool

  val select_expr :
       Encoding.Expr.t
    -> if_true:Encoding.Expr.t
    -> if_false:Encoding.Expr.t
    -> Encoding.Expr.t
end
