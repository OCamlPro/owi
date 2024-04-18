(*****************************************************************************)
(*                                                                           *)
(*  Owi                                                                      *)
(*                                                                           *)
(*  Copyright (C) 2021-2024 OCamlPro                                         *)
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

(** Module to define externref values in OCaml. You should look in the `example`
    directory to understand how to use this before reading the code... *)

open Types

type externref = E : 'a Type.Id.t * 'a -> externref

module Make_extern_func (V : Func_intf.Value_types) (M : Func_intf.Monad_type) :
  Func_intf.T_Extern_func
    with type int32 := V.int32
     and type int64 := V.int64
     and type float32 := V.float32
     and type float64 := V.float64
     and type 'a m := 'a M.t

module Func :
  Func_intf.T
    with type int32 := Int32.t
     and type int64 := Int64.t
     and type float32 := Float32.t
     and type float64 := Float64.t
     and type 'a m := 'a

type ref_value =
  | Externref of externref option
  | Funcref of Func_intf.t option
  | Arrayref of unit array option

type t =
  | I32 of Int32.t
  | I64 of Int64.t
  | F32 of Float32.t
  | F64 of Float64.t
  | Ref of ref_value

val cast_ref : externref -> 'a Type.Id.t -> 'a option

val of_instr : simplified instr -> t

val to_instr : t -> simplified instr

val ref_null' : simplified heap_type -> ref_value

val ref_null : simplified heap_type -> t

val ref_func : Func.t -> t

val ref_externref : 'a Type.Id.t -> 'a -> t

val ref_is_null : ref_value -> bool

val pp : Format.formatter -> t -> unit
