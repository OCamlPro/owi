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

(** Module providing functions to parse a wasm script from various kind of
    inputs. *)

module Script : sig
  (** Parse a script from a string. *)
  val from_string : string -> Text.script Result.t

  (** Parse a script from a channel. *)
  val from_channel : in_channel -> Text.script Result.t

  (** Parse a script from a file. *)
  val from_file : Fpath.t -> Text.script Result.t
end

module Module : sig
  (** Parse a module from a string. *)
  val from_string : string -> Text.modul Result.t

  (** Parse a module from a channel. *)
  val from_channel : in_channel -> Text.modul Result.t

  (** Parse a module from a file. *)
  val from_file : Fpath.t -> Text.modul Result.t
end

module Inline_module : sig
  (** Parse an inline module from a string. *)
  val from_string : string -> Text.modul Result.t

  (** Parse an inline module from a channel. *)
  val from_channel : in_channel -> Text.modul Result.t

  (** Parse an inline module from a file. *)
  val from_file : Fpath.t -> Text.modul Result.t
end
