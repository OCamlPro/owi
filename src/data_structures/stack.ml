(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module type S = sig
  type boolean

  type i32

  type i64

  type f32

  type f64

  type v128

  type ref_value

  type value

  type t = value list

  val empty : t

  val pp : t Fmt.t

  (** pop operations *)

  val drop : t -> t

  val drop_n : 'a list -> int -> 'a list

  val pop : t -> value * t

  val pop_n : t -> int -> t * t

  val keep : t -> int -> t

  val pop_bool : t -> boolean * t

  val pop_i32 : t -> i32 * t

  val pop2_i32 : t -> (i32 * i32) * t

  val pop_i64 : t -> i64 * t

  val pop2_i64 : t -> (i64 * i64) * t

  val pop_f32 : t -> f32 * t

  val pop2_f32 : t -> (f32 * f32) * t

  val pop_f64 : t -> f64 * t

  val pop2_f64 : t -> (f64 * f64) * t

  val pop_v128 : t -> v128 * t

  val pop2_v128 : t -> (v128 * v128) * t

  val pop_ref : t -> value * t

  val pop_as_ref : t -> ref_value * t

  (** push operations *)

  val push : t -> value -> t

  val push_bool : t -> boolean -> t

  val push_i32 : t -> i32 -> t

  val push_concrete_i32 : t -> Int32.t -> t

  val push_i32_of_int : t -> int -> t

  val push_i64 : t -> i64 -> t

  val push_concrete_i64 : t -> Int64.t -> t

  val push_f32 : t -> f32 -> t

  val push_concrete_f32 : t -> Float32.t -> t

  val push_f64 : t -> f64 -> t

  val push_concrete_f64 : t -> Float64.t -> t

  val push_v128 : t -> v128 -> t

  val push_concrete_v128 : t -> Concrete_v128.t -> t

  val push_ref : t -> ref_value -> t

  val push_array : t -> unit Array.t -> t

  (** apply operations *)

  val apply_i32_i32 : t -> (i32 -> i32) -> t

  val apply_i32_i64 : t -> (i32 -> i64) -> t

  val apply_i32_f32 : t -> (i32 -> f32) -> t

  val apply_i32_f64 : t -> (i32 -> f64) -> t

  val apply_i32_boolean : t -> (i32 -> boolean) -> t

  val apply_i32_i32_i32 : t -> (i32 -> i32 -> i32) -> t

  val apply_i32_i32_boolean : t -> (i32 -> i32 -> boolean) -> t

  val apply_i64_i64 : t -> (i64 -> i64) -> t

  val apply_i64_i32 : t -> (i64 -> i32) -> t

  val apply_i64_f32 : t -> (i64 -> f32) -> t

  val apply_i64_f64 : t -> (i64 -> f64) -> t

  val apply_i64_boolean : t -> (i64 -> boolean) -> t

  val apply_i64_i64_i64 : t -> (i64 -> i64 -> i64) -> t

  val apply_i64_i64_boolean : t -> (i64 -> i64 -> boolean) -> t

  val apply_f32_f32 : t -> (f32 -> f32) -> t

  val apply_f32_f64 : t -> (f32 -> f64) -> t

  val apply_f32_i32 : t -> (f32 -> i32) -> t

  val apply_f32_i64 : t -> (f32 -> i64) -> t

  val apply_f32_f32_f32 : t -> (f32 -> f32 -> f32) -> t

  val apply_f32_f32_boolean : t -> (f32 -> f32 -> boolean) -> t

  val apply_f64_f64 : t -> (f64 -> f64) -> t

  val apply_f64_f32 : t -> (f64 -> f32) -> t

  val apply_f64_i32 : t -> (f64 -> i32) -> t

  val apply_f64_i64 : t -> (f64 -> i64) -> t

  val apply_f64_f64_f64 : t -> (f64 -> f64 -> f64) -> t

  val apply_f64_f64_boolean : t -> (f64 -> f64 -> boolean) -> t
end

module Make (Value : Value_intf.T) :
  S
    with type value := Value.t
     and type boolean := Value.boolean
     and type i32 := Value.i32
     and type i64 := Value.i64
     and type f32 := Value.f32
     and type f64 := Value.f64
     and type v128 := Value.v128
     and type ref_value := Value.Ref.t = struct
  open Value

  type t = Value.t list

  exception Empty

  let empty = []

  let push s v = v :: s

  let push_bool s b = push s (I32 (Value.I32.of_boolean b))

  let push_concrete_i32 s i = push s (I32 (Value.I32.of_int32 i))

  let push_i32 s i = push s (I32 i)

  let push_i32_of_int s i = push_concrete_i32 s (Int32.of_int i)

  let push_concrete_i64 s i = push s (I64 (Value.I64.of_int64 i))

  let push_i64 s i = push s (I64 i)

  let push_concrete_f32 s f = push s (F32 (Value.F32.of_float32 f))

  let push_f32 s f = push s (F32 f)

  let push_concrete_f64 s f =
    push s (F64 (Value.F64.of_float (Float64.to_float f)))

  let push_f64 s f = push s (F64 f)

  let push_concrete_v128 s f = push s (V128 (Value.V128.of_concrete f))

  let push_v128 s f = push s (V128 f)

  let push_ref s r = push s (Ref r)

  let push_array _ _ = assert false

  let pp fmt (s : t) =
    Fmt.list ~sep:(fun fmt () -> Fmt.string fmt " ; ") Value.pp fmt s

  let pop = function [] -> raise Empty | hd :: tl -> (hd, tl)

  let drop = function [] -> raise Empty | _hd :: tl -> tl

  let pop_i32 s =
    let hd, tl = pop s in
    match hd with I32 n -> (n, tl) | _ -> assert false

  let pop2_i32 s =
    let n2, s = pop s in
    let n1, tl = pop s in
    match (n1, n2) with I32 n1, I32 n2 -> ((n1, n2), tl) | _ -> assert false

  let pop_i64 s =
    let hd, tl = pop s in
    match hd with I64 n -> (n, tl) | _ -> assert false

  let pop2_i64 s =
    let n2, s = pop s in
    let n1, tl = pop s in
    match (n1, n2) with I64 n1, I64 n2 -> ((n1, n2), tl) | _ -> assert false

  let pop_f32 s =
    let hd, tl = pop s in
    match hd with F32 f -> (f, tl) | _ -> assert false

  let pop2_f32 s =
    let n2, s = pop s in
    let n1, tl = pop s in
    match (n1, n2) with F32 n1, F32 n2 -> ((n1, n2), tl) | _ -> assert false

  let pop_f64 s =
    let hd, tl = pop s in
    match hd with F64 f -> (f, tl) | _ -> assert false

  let pop2_f64 s =
    let n2, s = pop s in
    let n1, tl = pop s in
    match (n1, n2) with F64 n1, F64 n2 -> ((n1, n2), tl) | _ -> assert false

  let pop_v128 s =
    let hd, tl = pop s in
    match hd with V128 f -> (f, tl) | _ -> assert false

  let pop2_v128 s =
    let n2, s = pop s in
    let n1, tl = pop s in
    match (n1, n2) with V128 n1, V128 n2 -> ((n1, n2), tl) | _ -> assert false

  let pop_ref s =
    let hd, tl = pop s in
    match hd with Ref _ -> (hd, tl) | _ -> assert false

  let pop_as_ref s =
    let hd, tl = pop s in
    match hd with Ref hd -> (hd, tl) | _ -> assert false

  let pop_bool s =
    let hd, tl = pop s in
    match hd with I32 n -> (Value.I32.to_boolean n, tl) | _ -> assert false

  let pop_n s n =
    (List.filteri (fun i _hd -> i < n) s, List.filteri (fun i _hd -> i >= n) s)

  let keep s n = List.filteri (fun i _hd -> i < n) s

  let rec drop_n s n =
    if n = 0 then s
    else match s with [] -> assert false | _ :: tl -> drop_n tl (n - 1)

  let apply_i32_boolean s f =
    let hd, tl = pop_i32 s in
    push_bool tl (f hd)

  let apply_i32_i32 s f =
    let hd, tl = pop_i32 s in
    push_i32 tl (f hd)

  let apply_i32_f32 s f =
    let hd, tl = pop_i32 s in
    push_f32 tl (f hd)

  let apply_i32_f64 s f =
    let hd, tl = pop_i32 s in
    push_f64 tl (f hd)

  let apply_i32_i64 s f =
    let hd, tl = pop_i32 s in
    push_i64 tl (f hd)

  let apply_i32_i32_i32 s f =
    let (hd1, hd2), tl = pop2_i32 s in
    push_i32 tl (f hd1 hd2)

  let apply_i32_i32_boolean s f =
    let (hd1, hd2), tl = pop2_i32 s in
    push_bool tl (f hd1 hd2)

  let apply_i64_boolean s f =
    let hd, tl = pop_i64 s in
    push_bool tl (f hd)

  let apply_i64_i64 s f =
    let hd, tl = pop_i64 s in
    push_i64 tl (f hd)

  let apply_i64_i32 s f =
    let hd, tl = pop_i64 s in
    push_i32 tl (f hd)

  let apply_i64_f32 s f =
    let hd, tl = pop_i64 s in
    push_f32 tl (f hd)

  let apply_i64_f64 s f =
    let hd, tl = pop_i64 s in
    push_f64 tl (f hd)

  let apply_i64_i64_i64 s f =
    let (hd1, hd2), tl = pop2_i64 s in
    push_i64 tl (f hd1 hd2)

  let apply_i64_i64_boolean s f =
    let (hd1, hd2), tl = pop2_i64 s in
    push_bool tl (f hd1 hd2)

  let apply_f32_f32 s f =
    let hd, tl = pop_f32 s in
    push_f32 tl (f hd)

  let apply_f32_f64 s f =
    let hd, tl = pop_f32 s in
    push_f64 tl (f hd)

  let apply_f32_i32 s f =
    let hd, tl = pop_f32 s in
    push_i32 tl (f hd)

  let apply_f32_i64 s f =
    let hd, tl = pop_f32 s in
    push_i64 tl (f hd)

  let apply_f32_f32_f32 s f =
    let (hd1, hd2), tl = pop2_f32 s in
    push_f32 tl (f hd1 hd2)

  let apply_f32_f32_boolean s f =
    let (hd1, hd2), tl = pop2_f32 s in
    push_bool tl (f hd1 hd2)

  let apply_f64_f64 s f =
    let hd, tl = pop_f64 s in
    push_f64 tl (f hd)

  let apply_f64_f32 s f =
    let hd, tl = pop_f64 s in
    push_f32 tl (f hd)

  let apply_f64_i32 s f =
    let hd, tl = pop_f64 s in
    push_i32 tl (f hd)

  let apply_f64_i64 s f =
    let hd, tl = pop_f64 s in
    push_i64 tl (f hd)

  let apply_f64_f64_f64 s f =
    let (hd1, hd2), tl = pop2_f64 s in
    push_f64 tl (f hd1 hd2)

  let apply_f64_f64_boolean s f =
    let (hd1, hd2), tl = pop2_f64 s in
    push_bool tl (f hd1 hd2)
end
