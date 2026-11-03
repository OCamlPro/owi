(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type i32 = Symbolic_i32.t

type boolean = Symbolic_boolean.t

type 'value t =
  (Smtml.Typed.Bitv32.w, 'value) Smtml.Typed.Arrays.array Smtml.Typed.expr

let get_type _a = assert false
(* Smtml.Typed.Arrays.ty a *)

let new_fill _type_id _v _n = assert false

let new_fixed_with _type_id _fields = assert false

let get_elem a index =
  let v = Smtml.Typed.Arrays.select a index in
  match Smtml.Typed.view v with
  | Val v ->
    begin match v with _ -> assert false
    end
  | _ -> assert false

let set_elem a index v =
  let _, _, _ = (a, index, v) in
  assert false
(*
  Smtml.Typed.Arrays.store a index v
*)

let length _a = assert false

let phys_equal _a1 _a2 = assert false
