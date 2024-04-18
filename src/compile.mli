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

(** Utility functions to compile a module until a given step. *)

val until_check : unsafe:bool -> Text.modul -> Text.modul Result.t

val until_simplify : unsafe:bool -> Text.modul -> Simplified.modul Result.t

val until_typecheck : unsafe:bool -> Text.modul -> Simplified.modul Result.t

val until_optimize :
  unsafe:bool -> optimize:bool -> Text.modul -> Simplified.modul Result.t

(** compile a module with a given link state and produce a new link state and a
    runnable module *)
val until_link :
     unsafe:bool
  -> 'f Link.state
  -> optimize:bool
  -> name:string option
  -> Text.modul
  -> ('f Link.module_to_run * 'f Link.state) Result.t

(** compile and interpret a module with a given link state and produce a new
    link state *)
val until_interpret :
     Concrete_value.Func.extern_func Link.state
  -> unsafe:bool
  -> optimize:bool
  -> name:string option
  -> Text.modul
  -> Concrete_value.Func.extern_func Link.state Result.t
