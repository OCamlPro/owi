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

val ( let* ) :
     ('a, 'err) Stdlib.Result.t
  -> ('a -> ('b, 'err) Stdlib.Result.t)
  -> ('b, 'err) Stdlib.Result.t

val ( let+ ) :
  ('a, 'err) Stdlib.Result.t -> ('a -> 'b) -> ('b, 'err) Stdlib.Result.t

val error : string -> ('a, string) Stdlib.Result.t

val error_s :
  ('a, Format.formatter, unit, ('b, string) Stdlib.Result.t) format4 -> 'a

val ok : 'a -> ('a, 'err) Stdlib.Result.t

val list_iter :
     ('a -> (unit, 'err) Stdlib.Result.t)
  -> 'a list
  -> (unit, 'err) Stdlib.Result.t

val list_map :
     ('a -> ('b, 'err) Stdlib.Result.t)
  -> 'a list
  -> ('b list, 'err) Stdlib.Result.t

val list_fold_left :
     ('a -> 'b -> ('a, 'err) Stdlib.Result.t)
  -> 'a
  -> 'b list
  -> ('a, 'err) Stdlib.Result.t

val array_iter :
     ('a -> (unit, 'err) Stdlib.Result.t)
  -> 'a array
  -> (unit, 'err) Stdlib.Result.t

val array_map :
     ('a -> ('b, 'err) Stdlib.Result.t)
  -> 'a array
  -> ('b array, 'err) Stdlib.Result.t

val array_fold_left :
     ('a -> 'b -> ('a, 'err) Stdlib.Result.t)
  -> 'a
  -> 'b array
  -> ('a, 'err) Stdlib.Result.t
