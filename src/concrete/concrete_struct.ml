(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type boolean = Concrete_boolean.t

type 'value t =
  { obj_id : int
  ; type_id : int
  ; fields : 'value array
  }

let fresh_id =
  let obj_id_counter = ref ~-1 in
  fun () ->
    incr obj_id_counter;
    !obj_id_counter

let new_with type_id fields = { obj_id = fresh_id (); type_id; fields }

let get_type { type_id; _ } = Some type_id

let get_field { fields; _ } idx = fields.(idx)

let set_field s idx v = s.fields.(idx) <- v

let phys_equal a1 a2 = Int.equal a1.obj_id a2.obj_id
