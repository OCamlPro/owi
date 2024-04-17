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

type t

type collection

val init : unit -> collection

val clone : collection -> collection

val get_memory : Env_id.t -> Concrete_memory.t -> collection -> int -> t

val check_within_bounds :
     t
  -> Encoding.Expr.t
  -> (Encoding.Expr.t * Symbolic_value.int32, Trap.t) result

val replace_size : t -> Int32.t -> Encoding.Expr.t -> unit

val free : t -> Int32.t -> unit

val load_8_s : t -> Encoding.Expr.t -> Symbolic_value.int32

val load_8_u : t -> Encoding.Expr.t -> Symbolic_value.int32

val load_16_s : t -> Encoding.Expr.t -> Symbolic_value.int32

val load_16_u : t -> Encoding.Expr.t -> Symbolic_value.int32

val load_32 : t -> Encoding.Expr.t -> Symbolic_value.int32

val load_64 : t -> Encoding.Expr.t -> Symbolic_value.int32

val store_8 : t -> addr:Encoding.Expr.t -> Encoding.Expr.t -> unit

val store_16 : t -> addr:Encoding.Expr.t -> Encoding.Expr.t -> unit

val store_32 : t -> addr:Encoding.Expr.t -> Encoding.Expr.t -> unit

val store_64 : t -> addr:Encoding.Expr.t -> Encoding.Expr.t -> unit

val grow : t -> Encoding.Expr.t -> unit

val fill :
  t -> pos:Encoding.Expr.t -> len:Encoding.Expr.t -> char -> Encoding.Expr.t

val blit :
     t
  -> src:Encoding.Expr.t
  -> dst:Encoding.Expr.t
  -> len:Encoding.Expr.t
  -> Encoding.Expr.t

val blit_string :
     t
  -> string
  -> src:Encoding.Expr.t
  -> dst:Encoding.Expr.t
  -> len:Encoding.Expr.t
  -> Encoding.Expr.t

val size : t -> Encoding.Expr.t

val size_in_pages : t -> Encoding.Expr.t

val get_limit_max : t -> Encoding.Expr.t option

module ITbl : sig
  type 'a t

  type key

  val iter : (key -> 'a -> unit) -> 'a t -> unit
end

val iter : (t ITbl.t -> unit) -> collection -> unit
