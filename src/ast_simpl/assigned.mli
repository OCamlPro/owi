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

open Types

type t =
  { id : string option
  ; typ : simplified str_type Named.t
  ; global : (Text.global, simplified global_type) Runtime.t Named.t
  ; table : (simplified table, simplified table_type) Runtime.t Named.t
  ; mem : (Types.mem, Types.limits) Runtime.t Named.t
  ; func : (text func, text block_type) Runtime.t Named.t
  ; elem : Text.elem Named.t
  ; data : Text.data Named.t
  ; exports : Grouped.opt_exports
  ; start : text indice option
  }

val of_grouped : Grouped.t -> t Result.t
