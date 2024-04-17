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

let py_module = lazy (Py.Import.import_module "instrumentor")

let import_module () = Lazy.force py_module

let instrument file includes =
  let callable = Py.Module.get (import_module ()) "instrument" in
  let kwargs =
    [ ("file", Py.String.of_string @@ Fpath.to_string file)
    ; ( "includes"
      , Py.List.of_list
        @@ List.map
             (fun path -> Py.String.of_string (Fpath.to_string path))
             includes )
    ]
  in
  let _ : Py.Object.t =
    Py.Callable.to_function_with_keywords callable [||] kwargs
  in
  ()
