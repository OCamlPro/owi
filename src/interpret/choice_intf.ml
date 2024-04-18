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

module type Base = sig
  module V : Func_intf.Value_types

  type 'a t

  val return : 'a -> 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val map : 'a t -> ('a -> 'b) -> 'b t

  val select : V.vbool -> bool t

  val select_i32 : V.int32 -> Int32.t t

  val trap : Trap.t -> 'a t

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
end

module type Complete = sig
  include Base

  type thread

  type 'a run_result

  val assertion : V.vbool -> unit t

  val with_thread : (thread -> 'b) -> 'b t

  val solver : Solver.solver t

  val thread : thread t

  val add_pc : V.vbool -> unit t

  val run : workers:int -> 'a t -> thread -> 'a run_result
end
