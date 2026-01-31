(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

let page_size = 65_536

type t =
  { mutable limits : Binary.Mem.Type.limits
  ; mutable data : bytes
  }

let get_min : Binary.Mem.Type.limits -> int = function
  | I32 { min; _ } -> Int32.to_int min
  | I64 { min; _ } -> min

let set_min (limits : Binary.Mem.Type.limits) new_min : Binary.Mem.Type.limits =
  match limits with
  | I32 l -> I32 { l with min = Int32.of_int new_min }
  | I64 l -> I64 { l with min = new_min }

let init limits : t =
  (* TODO: overflow check? (page_size * limits.min) / page_size = limits.min? *)
  let data = Bytes.make (page_size * get_min limits) '\x00' in
  { limits; data }

let update_memory mem data =
  let limits =
    set_min mem.limits
      (max (get_min mem.limits) (Bytes.length data / page_size))
  in
  mem.limits <- limits;
  mem.data <- data

let grow mem delta =
  let delta = Int32.to_int delta in
  let old_size = Bytes.length mem.data in
  let new_mem = Bytes.extend mem.data 0 delta in
  Bytes.unsafe_fill new_mem old_size delta (Char.chr 0);
  update_memory mem new_mem;
  Ok ()

let fill mem ~pos ~len c =
  let pos = Int32.to_int pos in
  let len = Int32.to_int len in
  Bytes.unsafe_fill mem.data pos len c;
  Ok ()

let blit ~src ~src_idx ~dst ~dst_idx ~len =
  let src_idx = Int32.to_int src_idx in
  let dst_idx = Int32.to_int dst_idx in
  let len = Int32.to_int len in
  Bytes.unsafe_blit src.data src_idx dst.data dst_idx len;
  Ok ()

let blit_string mem str ~src ~dst ~len =
  let src = Int32.to_int src in
  let dst = Int32.to_int dst in
  let len = Int32.to_int len in
  Bytes.unsafe_blit_string str src mem.data dst len;
  Ok ()

let get_limit_max { limits; _ } =
  match limits with
  | I32 { max; _ } -> Option.map (fun maxv -> Int32.to_int maxv) max
  | I64 { max; _ } -> max

let get_limits { limits; _ } = limits

let store_8 mem ~addr n =
  let addr = Int32.to_int addr in
  let n = Int32.to_int n in
  Ok (Bytes.set_int8 mem.data addr n)

let store_16 mem ~addr n =
  let addr = Int32.to_int addr in
  let n = Int32.to_int n in
  Ok (Bytes.set_int16_le mem.data addr n)

let store_32 mem ~addr n =
  let addr = Int32.to_int addr in
  Ok (Bytes.set_int32_le mem.data addr n)

let store_64 mem ~addr n =
  let addr = Int32.to_int addr in
  Ok (Bytes.set_int64_le mem.data addr n)

let load_8_s mem addr =
  let addr = Int32.to_int addr in
  Ok (Int32.of_int @@ Bytes.get_int8 mem.data addr)

let load_8_u mem addr =
  let addr = Int32.to_int addr in
  Ok (Int32.of_int @@ Bytes.get_uint8 mem.data addr)

let load_16_s mem addr =
  let addr = Int32.to_int addr in
  Ok (Int32.of_int @@ Bytes.get_int16_le mem.data addr)

let load_16_u mem addr =
  let addr = Int32.to_int addr in
  Ok (Int32.of_int @@ Bytes.get_uint16_le mem.data addr)

let load_32 mem addr =
  let addr = Int32.to_int addr in
  Ok (Bytes.get_int32_le mem.data addr)

let load_64 mem addr =
  let addr = Int32.to_int addr in
  Ok (Bytes.get_int64_le mem.data addr)

let size_in_pages mem = Int32.of_int @@ (Bytes.length mem.data / page_size)

let size mem = Int32.of_int @@ Bytes.length mem.data
