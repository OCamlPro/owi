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

open Format
open Types

(** named export *)
type export =
  { name : string
  ; id : int
  }

(** named exports of a module *)
type exports =
  { global : export list
  ; mem : export list
  ; table : export list
  ; func : export list
  }

type global =
  { typ : simplified global_type (* TODO: init : simplified+const expr*)
  ; init : simplified expr
  ; id : string option
  }

type data_mode =
  | Data_passive
  (* TODO: Data_active simplified+const expr*)
  | Data_active of int option * simplified expr

type data =
  { id : string option
  ; init : string
  ; mode : data_mode
  }

type elem_mode =
  | Elem_passive
  (* TODO: Elem_active simplified+const expr*)
  | Elem_active of int option * simplified expr
  | Elem_declarative

type elem =
  { id : string option
  ; typ : simplified ref_type (* TODO: init : simplified+const expr*)
  ; init : simplified expr list
  ; mode : elem_mode
  }

type modul =
  { id : string option
  ; global : (global, simplified global_type) Runtime.t Named.t
  ; table : (simplified table, simplified table_type) Runtime.t Named.t
  ; mem : (mem, limits) Runtime.t Named.t
  ; func : (simplified func, simplified block_type) Runtime.t Named.t
      (* TODO: switch to func_type *)
  ; elem : elem Named.t
  ; data : data Named.t
  ; exports : exports
  ; start : int option
  }

module Pp = struct
  let id fmt = Option.iter (fun id -> pp fmt " $%s" id)

  let func fmt = function
    | Runtime.Local f -> pp_func fmt f
    | Runtime.Imported { Imported.modul; name; _ } -> pp fmt "%s.%s" modul name

  let lst f fmt l = (pp_list ~pp_sep:pp_newline f) fmt (List.rev l)

  let funcs fmt funcs = lst (Indexed.pp func) fmt funcs.Named.values

  let export fmt (export : export) = pp fmt "%s: %d" export.name export.id

  let start fmt = function None -> () | Some ind -> pp fmt "(start %d)" ind

  let modul fmt (m : modul) : unit =
    pp fmt "(module%a@\n  @[%a@\n%a@]@\n)" id m.id funcs m.func start m.start
end
