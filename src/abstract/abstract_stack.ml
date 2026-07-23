(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module Value = Abstract_value

type t = Value.t list

exception Empty

let empty = []

let push s v = v :: s

let push_bool s ctx b = push s (Value.I32 (Value.I32.of_boolean ctx b))

let push_concrete_i32 s ctx i = push s (Value.I32 (Value.I32.of_int32 ctx i))

let push_i32 s i = push s (Value.I32 i)

let push_i32_of_int s ctx i = push_concrete_i32 s ctx (Int32.of_int i)

let push_concrete_i64 s ctx i = push s (Value.I64 (Value.I64.of_int64 ctx i))

let push_i64 s i = push s (Value.I64 i)

let push_concrete_f32 s ctx f = push s (Value.F32 (Value.F32.of_float32 ctx f))

let push_f32 s f = push s (Value.F32 f)

let push_concrete_f64 s ctx f =
  push s (Value.F64 (Value.F64.of_float ctx (Float64.to_float f)))

let push_f64 s f = push s (Value.F64 f)

let push_concrete_v128 s ctx f =
  push s (Value.V128 (Value.V128.of_concrete ctx f))

let push_v128 s f = push s (Value.V128 f)

let push_ref s r = push s (Value.Ref r)

let push_array _ _ = assert false

let pp ctx fmt (s : t) =
  Fmt.list
    ~sep:(fun fmt () -> Fmt.string fmt " ; ")
    (Value.pp_with_ctx ctx) fmt s

let pop = function [] -> raise Empty | hd :: tl -> (hd, tl)

let pop2 = function v1 :: v2 :: tl -> ((v1, v2), tl) | _ -> raise Empty

let drop = function [] -> raise Empty | _hd :: tl -> tl

let pop_i32 s =
  let hd, tl = pop s in
  match hd with Value.I32 n -> (n, tl) | _ -> assert false

let pop2_i32 s =
  let n2, s = pop s in
  let n1, tl = pop s in
  match (n1, n2) with
  | Value.I32 n1, I32 n2 -> ((n1, n2), tl)
  | _ -> assert false

let pop_i64 s =
  let hd, tl = pop s in
  match hd with Value.I64 n -> (n, tl) | _ -> assert false

let pop2_i64 s =
  let n2, s = pop s in
  let n1, tl = pop s in
  match (n1, n2) with
  | Value.I64 n1, I64 n2 -> ((n1, n2), tl)
  | _ -> assert false

let pop_f32 s =
  let hd, tl = pop s in
  match hd with Value.F32 f -> (f, tl) | _ -> assert false

let pop2_f32 s =
  let n2, s = pop s in
  let n1, tl = pop s in
  match (n1, n2) with
  | Value.F32 n1, F32 n2 -> ((n1, n2), tl)
  | _ -> assert false

let pop_f64 s =
  let hd, tl = pop s in
  match hd with Value.F64 f -> (f, tl) | _ -> assert false

let pop2_f64 s =
  let n2, s = pop s in
  let n1, tl = pop s in
  match (n1, n2) with
  | Value.F64 n1, F64 n2 -> ((n1, n2), tl)
  | _ -> assert false

let pop_v128 s =
  let hd, tl = pop s in
  match hd with Value.V128 f -> (f, tl) | _ -> assert false

let pop2_v128 s =
  let n2, s = pop s in
  let n1, tl = pop s in
  match (n1, n2) with
  | Value.V128 n1, V128 n2 -> ((n1, n2), tl)
  | _ -> assert false

let pop_ref s =
  let hd, tl = pop s in
  match hd with Value.Ref _ -> (hd, tl) | _ -> assert false

let pop_as_ref s =
  let hd, tl = pop s in
  match hd with Value.Ref hd -> (hd, tl) | _ -> assert false

let pop_bool s ctx =
  let hd, tl = pop s in
  match hd with
  | Value.I32 n -> (Value.I32.to_boolean ctx n, tl)
  | _ -> assert false

let pop_n s n =
  (List.filteri (fun i _hd -> i < n) s, List.filteri (fun i _hd -> i >= n) s)

let keep s n = List.filteri (fun i _hd -> i < n) s

let rec drop_n s n =
  if n = 0 then s
  else match s with [] -> assert false | _ :: tl -> drop_n tl (n - 1)

let apply_i32_boolean s ctx f =
  let hd, tl = pop_i32 s in
  push_bool tl ctx (f hd)

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

let apply_i32_i32_boolean s ctx f =
  let (hd1, hd2), tl = pop2_i32 s in
  push_bool tl ctx (f hd1 hd2)

let apply_i64_boolean s ctx f =
  let hd, tl = pop_i64 s in
  push_bool tl ctx (f hd)

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

let apply_i64_i64_boolean s ctx f =
  let (hd1, hd2), tl = pop2_i64 s in
  push_bool tl ctx (f hd1 hd2)

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

let apply_f32_f32_boolean s ctx f =
  let (hd1, hd2), tl = pop2_f32 s in
  push_bool tl ctx (f hd1 hd2)

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

let apply_f64_f64_boolean s ctx f =
  let (hd1, hd2), tl = pop2_f64 s in
  push_bool tl ctx (f hd1 hd2)
