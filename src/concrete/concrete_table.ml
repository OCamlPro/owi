(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

(* TODO: Concrete_value.Ref.t array, gadt to constraint to the right ref_type ? *)
type table = Concrete_ref.t array

type t =
  { limits : Binary.Table.Type.limits
  ; typ : Binary.ref_type
  ; mutable data : table
  }

let get_min : Binary.Table.Type.limits -> int = function
  | I32 { min; _ } -> Int32.to_int min
  | I64 { min; _ } -> Int64.to_int min

let max_size t =
  match t.limits with
  | I32 { max; _ } -> Option.map (fun maxv -> Int32.to_int maxv) max
  | I64 { max; _ } ->
    Option.map
      (fun maxv ->
        let max2int = Int64.to_int maxv in
        assert (Int64.(eq maxv (Int64.of_int max2int)));
        max2int )
      max

let get_type { limits; typ; _ } = (limits, typ)

let init (typ : Binary.Table.Type.t) : t =
  let limits, ((_null, heap_type) as ref_type) = typ in
  let null = Concrete_ref.null heap_type in
  let table = Array.make (get_min limits) null in
  { limits; typ = ref_type; data = table }

let update table data = table.data <- data

let get t i = t.data.(i)

let set t i v =
  t.data.(i) <- v;
  t

let size t = Array.length t.data

let typ t = t.typ

let grow t new_size x =
  let new_size = Int32.to_int new_size in
  let new_table = Array.make new_size x in
  Array.blit t.data 0 new_table 0 (Array.length t.data);
  update t new_table;
  t

let fill t pos len x =
  let pos = Int32.to_int pos in
  let len = Int32.to_int len in
  Array.fill t.data pos len x;
  t

let copy ~t_src ~t_dst ~src ~dst ~len =
  let src = Int32.to_int src in
  let dst = Int32.to_int dst in
  let len = Int32.to_int len in
  Array.blit t_src.data src t_dst.data dst len;
  t_dst
