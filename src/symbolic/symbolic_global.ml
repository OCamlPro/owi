(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type t = Symbolic_global0.t =
  { value : Symbolic_value.t
  ; env_id : int
  ; id : int
  }

let value global = global.value

let replace global =
  Symbolic_choice.modify_thread (Thread.replace_global global)

let set_value global value =
  let global = { global with value } in
  replace global

let convert_values (v : Concrete_value.t) : Symbolic_value.t =
  match v with
  | I32 v -> I32 (Symbolic_i32.of_concrete v)
  | I64 v -> I64 (Symbolic_i64.of_concrete v)
  | F32 v -> F32 (Symbolic_f32.of_concrete v)
  | F64 v -> F64 (Symbolic_f64.of_concrete v)
  | V128 v -> V128 (Symbolic_v128.of_concrete v)
  | Ref (Func f) -> Ref (Func f)
  | Ref _ -> assert false

let of_concrete ~env_id ~id (v : Concrete_global.t) : t =
  let value = convert_values v.Concrete_global.value in
  { value; env_id; id }
