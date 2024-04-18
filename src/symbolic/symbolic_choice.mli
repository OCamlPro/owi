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

exception Assertion of Smtml.Expr.t * Thread.t

module Minimalist : sig
  type err = private
    | Assert_fail
    | Trap of Trap.t

  include
    Choice_intf.Complete
      with type thread := Thread.t
       and type 'a run_result = ('a, err) Stdlib.Result.t * Thread.t
       and module V := Symbolic_value
end

module Multicore : sig
  type 'a eval =
    | EVal of 'a
    | ETrap of Trap.t
    | EAssert of Smtml.Expr.t

  include
    Choice_intf.Complete
      with type thread := Thread.t
       and type 'a run_result = ('a eval * Thread.t) Seq.t
       and module V := Symbolic_value
end
