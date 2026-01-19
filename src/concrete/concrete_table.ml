(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(* TODO: Concrete_value.Ref.t array, gadt to constraint to the right ref_type ? *)
type table = Concrete_ref.t array

type t =
  { id : int
  ; label : string option
  ; limits : Text.limits
  ; typ : Text.ref_type
  ; mutable data : table
  }

let fresh =
  let r = ref (-1) in
  fun () ->
    incr r;
    !r

let init ?label (typ : Text.Table.Type.t) : t =
  let limits, ((_null, heap_type) as ref_type) = typ in
  let null = Concrete_ref.null heap_type in
  let table = Array.make limits.min null in
  { id = fresh (); label; limits; typ = ref_type; data = table }

let update table data = table.data <- data

let get t i = t.data.(i)

let set t i v =
  t.data.(i) <- v;
  Ok ()

let size t = Array.length t.data

let typ t = t.typ

let max_size t = t.limits.max

let grow t new_size x =
  let new_size = Int32.to_int new_size in
  let new_table = Array.make new_size x in
  Array.blit t.data 0 new_table 0 (Array.length t.data);
  update t new_table;
  Ok ()

let fill t pos len x =
  let pos = Int32.to_int pos in
  let len = Int32.to_int len in
  Array.fill t.data pos len x;
  Ok ()

let copy ~t_src ~t_dst ~src ~dst ~len =
  let src = Int32.to_int src in
  let dst = Int32.to_int dst in
  let len = Int32.to_int len in
  Array.blit t_src.data src t_dst.data dst len;
  Ok ()
