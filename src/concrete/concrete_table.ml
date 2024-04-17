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

(* TODO: Concrete_value.ref_value array, gadt to constraint to the right ref_type ? *)
type table = Concrete_value.ref_value array

type t =
  { id : int
  ; label : string option
  ; limits : limits
  ; typ : simplified ref_type
  ; mutable data : table
  }

let fresh =
  let r = ref (-1) in
  fun () ->
    incr r;
    !r

let init ?label (typ : simplified table_type) : t =
  let limits, ((_null, heap_type) as ref_type) = typ in
  let null = Concrete_value.ref_null' heap_type in
  let table = Array.make limits.min null in
  { id = fresh (); label; limits; typ = ref_type; data = table }

let update table data = table.data <- data

let get t i = t.data.(i)

let set t i v = t.data.(i) <- v

let size t = Array.length t.data

let typ t = t.typ

let max_size t = t.limits.max

let grow t new_size x =
  let new_size = Int32.to_int new_size in
  let new_table = Array.make new_size x in
  Array.blit t.data 0 new_table 0 (Array.length t.data);
  update t new_table

let fill t pos len x =
  let pos = Int32.to_int pos in
  let len = Int32.to_int len in
  Array.fill t.data pos len x

let copy ~t_src ~t_dst ~src ~dst ~len =
  let src = Int32.to_int src in
  let dst = Int32.to_int dst in
  let len = Int32.to_int len in
  Array.blit t_src.data src t_dst.data dst len
