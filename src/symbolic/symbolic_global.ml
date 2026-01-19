(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type t = { mutable value : Symbolic_value.t }

let value v = v.value

let set_value v x = v.value <- x

(** Collections of globals *)

let convert_values (v : Concrete_value.t) : Symbolic_value.t =
  match v with
  | I32 v -> I32 (Symbolic_i32.of_concrete v)
  | I64 v -> I64 (Symbolic_i64.of_concrete v)
  | F32 v -> F32 (Symbolic_f32.of_concrete v)
  | F64 v -> F64 (Symbolic_f64.of_concrete v)
  | V128 v -> V128 (Symbolic_v128.of_concrete v)
  | Ref (Func f) -> Ref (Func f)
  | Ref _ -> assert false

let of_concrete (v : Concrete_global.t) : t =
  let value = convert_values v.Concrete_global.value in
  { value }

module M = struct
  type concrete = Concrete_global.t

  type symbolic = t

  let convert_one original = of_concrete original

  (* WARNING: because we are doing an optimization in `Symbolic_choice`, the cloned state should not refer to a mutable value of the previous state. Assuming that the original state is not mutated is wrong. *)
  let clone_one r = { value = r.value }
end

module Collection = Collection.Make (M)
