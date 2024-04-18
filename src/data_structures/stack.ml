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

module type S = sig
  type vbool

  type int32

  type int64

  type float32

  type float64

  type ref_value

  type value

  type t = value list

  val empty : t

  val pp : Format.formatter -> t -> unit

  (** pop operations *)

  val drop : t -> t

  val drop_n : 'a list -> int -> 'a list

  val pop : t -> value * t

  val pop_n : t -> int -> t * t

  val keep : t -> int -> t

  val pop_bool : t -> vbool * t

  val pop_i32 : t -> int32 * t

  val pop2_i32 : t -> (int32 * int32) * t

  val pop_i64 : t -> int64 * t

  val pop2_i64 : t -> (int64 * int64) * t

  val pop_f32 : t -> float32 * t

  val pop2_f32 : t -> (float32 * float32) * t

  val pop_f64 : t -> float64 * t

  val pop2_f64 : t -> (float64 * float64) * t

  val pop_ref : t -> value * t

  val pop_as_ref : t -> ref_value * t

  (** push operations *)

  val push : t -> value -> t

  val push_bool : t -> vbool -> t

  val push_i32 : t -> int32 -> t

  val push_const_i32 : t -> Int32.t -> t

  val push_i32_of_int : t -> int -> t

  val push_i64 : t -> int64 -> t

  val push_const_i64 : t -> Int64.t -> t

  val push_f32 : t -> float32 -> t

  val push_const_f32 : t -> Float32.t -> t

  val push_f64 : t -> float64 -> t

  val push_const_f64 : t -> Float64.t -> t

  val push_as_externref : t -> 'b Type.Id.t -> 'b -> t

  val push_array : t -> unit Array.t -> t
end

module Make (V : Value_intf.T) :
  S
    with type value := V.t
     and type vbool := V.vbool
     and type int32 := V.int32
     and type int64 := V.int64
     and type float32 := V.float32
     and type float64 := V.float64
     and type ref_value := V.ref_value = struct
  open V

  type t = V.t list

  exception Empty

  let empty = []

  let push s v = v :: s

  let push_bool s b = push s (I32 (V.Bool.int32 b))

  let push_const_i32 s i = push s (I32 (V.const_i32 i))

  let push_i32 s i = push s (I32 i)

  let push_i32_of_int s i = push_const_i32 s (Int32.of_int i)

  let push_const_i64 s i = push s (I64 (V.const_i64 i))

  let push_i64 s i = push s (I64 i)

  let push_const_f32 s f = push s (F32 (V.const_f32 f))

  let push_f32 s f = push s (F32 f)

  let push_const_f64 s f = push s (F64 (V.const_f64 f))

  let push_f64 s f = push s (F64 f)

  let push_as_externref s ty v = push s (V.ref_externref ty v)

  let push_array _ _ = assert false

  let pp fmt (s : t) =
    Format.pp_list ~pp_sep:(fun fmt () -> Format.pp_string fmt " ; ") V.pp fmt s

  let pop = function [] -> raise Empty | hd :: tl -> (hd, tl)

  let drop = function [] -> raise Empty | _hd :: tl -> tl

  let pop_i32 s =
    let hd, tl = pop s in
    match hd with
    | I32 n -> (n, tl)
    | _ -> Log.err "invalid type (expected i32)"

  let pop2_i32 s =
    let n2, s = pop s in
    let n1, tl = pop s in
    match (n1, n2) with
    | I32 n1, I32 n2 -> ((n1, n2), tl)
    | _ -> Log.err "invalid type (expected i32)"

  let pop_i64 s =
    let hd, tl = pop s in
    match hd with
    | I64 n -> (n, tl)
    | _ -> Log.err "invalid type (expected i64)"

  let pop2_i64 s =
    let n2, s = pop s in
    let n1, tl = pop s in
    match (n1, n2) with
    | I64 n1, I64 n2 -> ((n1, n2), tl)
    | _ -> Log.err "invalid type (expected i64)"

  let pop_f32 s =
    let hd, tl = pop s in
    match hd with
    | F32 f -> (f, tl)
    | _ -> Log.err "invalid type (expected f32)"

  let pop2_f32 s =
    let n2, s = pop s in
    let n1, tl = pop s in
    match (n1, n2) with
    | F32 n1, F32 n2 -> ((n1, n2), tl)
    | _ -> Log.err "invalid type (expected f32)"

  let pop_f64 s =
    let hd, tl = pop s in
    match hd with
    | F64 f -> (f, tl)
    | _ -> Log.err "invalid type (expected f64)"

  let pop2_f64 s =
    let n2, s = pop s in
    let n1, tl = pop s in
    match (n1, n2) with
    | F64 n1, F64 n2 -> ((n1, n2), tl)
    | _ -> Log.err "invalid type (expected f64)"

  let pop_ref s =
    let hd, tl = pop s in
    match hd with
    | Ref _ -> (hd, tl)
    | _ -> Log.err "invalid type (expected ref)"

  let pop_as_ref s =
    let hd, tl = pop s in
    match hd with
    | Ref hd -> (hd, tl)
    | _ -> Log.err "invalid type (expected ref)"

  let pop_bool s =
    let hd, tl = pop s in
    match hd with
    | I32 n -> (V.I32.to_bool n, tl)
    | _ -> Log.err "invalid type (expected i32 (bool))"

  let pop_n s n =
    (List.filteri (fun i _hd -> i < n) s, List.filteri (fun i _hd -> i >= n) s)

  let keep s n = List.filteri (fun i _hd -> i < n) s

  let rec drop_n s n =
    if n = 0 then s
    else match s with [] -> invalid_arg "drop_n" | _ :: tl -> drop_n tl (n - 1)
end
