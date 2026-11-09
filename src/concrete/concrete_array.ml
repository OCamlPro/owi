(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module Map = Map.Make (Concrete_i32)

type i32 = Concrete_i32.t

type boolean = Concrete_boolean.t

type 'value t =
  { obj_id : int
  ; type_id : int
  ; fields : 'value Map.t
  ; length : Concrete_i32.t
  ; default : 'value option
  }

let get_type { type_id; _ } = Some type_id

let fresh_id =
  let obj_id_counter = ref ~-1 in
  fun () ->
    incr obj_id_counter;
    !obj_id_counter

let new_fill type_id v length =
  let obj_id = fresh_id () in
  let fields = Map.empty in
  let default = Some v in
  { obj_id; type_id; fields; length; default }

let new_fixed_with type_id fields =
  let obj_id = fresh_id () in
  let default = None in
  let fields, length =
    Array.fold_left
      (fun (fields, index) v ->
        let fields = Map.add index v fields in
        let index = Concrete_i32.add index 1l in
        (fields, index) )
      (Map.empty, 0l) fields
  in
  { obj_id; type_id; fields; length; default }

let get_elem { fields; default; _ } index =
  match Map.find_opt index fields with
  | Some v -> v
  | None ->
    begin match default with
    | Some v -> v
    | None -> (* checked by the interpreter directly *) assert false
    end

let set_elem a index v =
  let fields = Map.add index v a.fields in
  { a with fields }

let length { length; _ } = length

let phys_equal a1 a2 = Int.equal a1.obj_id a2.obj_id
