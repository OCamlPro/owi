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

open Stdlib.Result

let ( let* ) o f = match o with Ok v -> f v | Error _ as e -> e

let ( let+ ) o f = match o with Ok v -> Ok (f v) | Error _ as e -> e

let error v = Error v

let error_s format = Format.kasprintf error format

let ok v = Ok v

let list_iter f l =
  let err = ref None in
  try
    List.iter
      (fun v ->
        match f v with
        | Error _e as e ->
          err := Some e;
          raise Exit
        | Ok () -> () )
      l;
    Ok ()
  with Exit -> Option.get !err

let list_map f l =
  let err = ref None in
  try
    ok
    @@ List.map
         (fun v ->
           match f v with
           | Error _e as e ->
             err := Some e;
             raise Exit
           | Ok v -> v )
         l
  with Exit -> Option.get !err

let list_fold_left f acc l =
  List.fold_left
    (fun acc v ->
      let* acc in
      f acc v )
    (Ok acc) l

let array_iter f a =
  let err = ref None in
  try
    for i = 0 to Array.length a - 1 do
      match f (Array.unsafe_get a i) with
      | Error _e as e ->
        err := Some e;
        raise Exit
      | Ok () -> ()
    done;
    Ok ()
  with Exit -> Option.get !err

let array_map f a =
  let err = ref None in
  try
    ok
    @@ Array.init (Array.length a) (fun i ->
           let v = Array.get a i in
           match f v with
           | Error _e as e ->
             err := Some e;
             raise Exit
           | Ok v -> v )
  with Exit -> Option.get !err

let array_fold_left f acc a =
  let err = ref None in
  let acc = ref acc in
  try
    for i = 0 to Array.length a - 1 do
      match f !acc (Array.unsafe_get a i) with
      | Error _e as e ->
        err := Some e;
        raise Exit
      | Ok v -> acc := v
    done;
    Ok !acc
  with Exit -> Option.get !err
