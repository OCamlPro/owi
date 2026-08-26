(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type i32 = Concrete_i32.t

type boolean = Concrete_boolean.t

type 'value t =
  { obj_id : int
  ; type_id : int
  ; fields : 'value array
  }

let get_type { type_id; _ } = Some type_id

let fresh_id =
  let obj_id_counter = ref ~-1 in
  fun () ->
    incr obj_id_counter;
    !obj_id_counter

let new_fill type_id v n =
  let obj_id = fresh_id () in
  let fields = Array.init (Concrete_i32.to_int n) (fun _i -> v) in
  { obj_id; type_id; fields }

let new_fixed_with type_id fields = { obj_id = fresh_id (); type_id; fields }

let get_elem { fields; _ } index =
  let index = Concrete_i32.to_int index in
  fields.(index)

let set_elem a index v =
  let index = Concrete_i32.to_int index in
  a.fields.(index) <- v

let length { fields; _ } =
  let length = Array.length fields in
  Concrete_i32.of_int length

let phys_equal a1 a2 = Int.equal a1.obj_id a2.obj_id
