(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module Map = Map.Make (Int32)

type t =
  { data : Smtml.Typed.Bitv8.t Map.t
  ; size : Symbolic_i32.t
  ; limits : Binary.Mem.Type.limits
  }

let address a =
  let open Symbolic_choice in
  match Smtml.Typed.view a with
  | Val (Bitv bv) -> return (Smtml.Bitvector.to_int32 bv)
  | _ -> select_i32 a

let empty_byte = Smtml.Typed.Bitv8.v (Smtml.Bitvector.of_int8 0)

let load_byte a { data; _ } =
  match Map.find_opt a data with None -> empty_byte | Some v -> v

let replace_byte a (v : Smtml.Typed.Bitv8.t) data = Map.add a v data

let page_size = 65_536

let page_size_i32 = Symbolic_i32.of_int page_size

(******************************************)

let i32 v =
  match Smtml.Expr.view v with
  | Val (Bitv i) when Smtml.Bitvector.numbits i = 32 ->
    Smtml.Bitvector.to_int32 i
  | _ -> assert false

let grow m delta =
  let old_size = Symbolic_i32.mul m.size page_size_i32 in
  let new_size = Symbolic_i32.(div (add old_size delta) page_size_i32) in
  let size =
    Symbolic_boolean.ite (Symbolic_i32.lt m.size new_size) new_size m.size
  in
  { m with size }

let size { size; _ } = Symbolic_i32.mul size page_size_i32

let size_in_pages { size; _ } = size

let load_8_s m a =
  let open Symbolic_choice in
  let+ a = address a in
  let v = load_byte a m in
  Smtml.Typed.Bitv32.of_int8_s v

let load_8_u m a =
  let open Symbolic_choice in
  let+ a = address a in
  let v = load_byte a m in
  Smtml.Typed.Bitv32.of_int8_u v

let load_16_unchecked m a : Smtml.Typed.Bitv16.t =
  let lsb = load_byte a m in
  let msb = load_byte (Int32.add a 1l) m in
  Smtml.Typed.Bitv8.concat msb lsb

let load_16_s m a =
  let open Symbolic_choice in
  let+ a = address a in
  let v = load_16_unchecked m a in
  Smtml.Typed.Bitv32.of_int16_s v

let load_16_u m a =
  let open Symbolic_choice in
  let+ a = address a in
  let v = load_16_unchecked m a in
  Smtml.Typed.Bitv32.of_int16_u v

let load_32_unchecked m a : Smtml.Typed.Bitv32.t =
  let low = load_16_unchecked m a in
  let high = load_16_unchecked m (Int32.add a 2l) in
  Smtml.Typed.Bitv16.concat high low

let load_32 m a =
  let open Symbolic_choice in
  let+ a = address a in
  let v = load_32_unchecked m a in
  Smtml.Typed.simplify v

let load_64_unchecked m a : Smtml.Typed.Bitv64.t =
  let low = load_32_unchecked m a in
  let high = load_32_unchecked m (Int32.add a 4l) in
  Smtml.Typed.Bitv32.concat high low

let load_64 m a =
  let open Symbolic_choice in
  let+ a = address a in
  load_64_unchecked m a

let load_128_unchecked m a : Smtml.Typed.Bitv128.t =
  let low = load_64_unchecked m a in
  let high = load_64_unchecked m (Int32.add a 8l) in
  Smtml.Typed.Bitv64.concat high low

let load_128 m a =
  let open Symbolic_choice in
  let+ a = address a in
  load_128_unchecked m a

let store_8 m ~addr v =
  let open Symbolic_choice in
  let+ addr = address addr in
  let data =
    replace_byte addr (Smtml.Typed.Bitv32.extract v ~high:7 ~low:0) m.data
  in
  { m with data }

let store_16 m ~addr v =
  let open Symbolic_choice in
  let+ addr = address addr in
  let data =
    replace_byte addr (Smtml.Typed.Bitv32.extract v ~high:7 ~low:0) m.data
    |> replace_byte (Int32.add addr 1l)
         (Smtml.Typed.Bitv32.extract v ~high:15 ~low:8)
  in
  { m with data }

let store_byte_list data start_addr bytes =
  let rec loop data offset = function
    | [] -> data
    | byte :: remaining ->
      let addr = Int32.add start_addr offset in
      let data = replace_byte addr byte data in
      loop data (Int32.add offset 1l) remaining
  in
  loop data 0l bytes

let store_32 m ~addr v =
  let open Symbolic_choice in
  let+ addr = address addr in
  let data = store_byte_list m.data addr (Smtml.Typed.Bitv32.to_bytes v) in
  { m with data }

let store_64 m ~(addr : Symbolic_i32.t) v =
  let open Symbolic_choice in
  let+ addr = address addr in
  let data = store_byte_list m.data addr (Smtml.Typed.Bitv64.to_bytes v) in
  { m with data }

let store_128 m ~(addr : Symbolic_i32.t) v =
  let open Symbolic_choice in
  let+ addr = address addr in
  let data = store_byte_list m.data addr (Smtml.Typed.Bitv128.to_bytes v) in
  { m with data }

let fill m ~(pos : Symbolic_i32.t) ~(len : Symbolic_i32.t) (c : char) =
  let open Symbolic_choice in
  let* len = select_i32 len in
  let len = Int32.to_int len in
  let* pos = select_i32 pos in
  let pos = Int32.to_int pos in
  let c = Symbolic_i32.of_int (int_of_char c) in

  let rec loop i m =
    if i = len then return m
    else
      let addr = Symbolic_i32.of_int (pos + i) in
      let* m = store_8 m ~addr c in
      loop (i + 1) m
  in
  loop 0 m

let blit ~src ~src_idx ~dst ~dst_idx ~len =
  let open Symbolic_choice in
  let* len = select_i32 len in
  let len = Int32.to_int len in
  let* src_idx = select_i32 src_idx in
  let src_idx = Int32.to_int src_idx in
  let* dst_idx = select_i32 dst_idx in
  let dst_idx = Int32.to_int dst_idx in

  let rec loop i dst =
    if i = len then return dst
    else
      let addr = Symbolic_i32.of_int (src_idx + i) in
      let* v = load_8_s src addr in
      let addr = Symbolic_i32.of_int (dst_idx + i) in
      let* dst = store_8 dst ~addr v in
      loop (i + 1) dst
  in
  loop 0 dst

let blit_string m str ~src ~dst ~len =
  (* This function is only used in memory init so everything will be concrete *)
  (* TODO: I am not sure this is true, this should be investigated and fixed at some point *)
  let open Symbolic_choice in
  let src = Smtml.Typed.Unsafe.unwrap src in
  let dst = Smtml.Typed.Unsafe.unwrap dst in
  let len = Smtml.Typed.Unsafe.unwrap len in
  let src = Int32.to_int @@ i32 src in
  let dst = Int32.to_int @@ i32 dst in
  let len = Int32.to_int @@ i32 len in
  let rec loop i m =
    if i = len then return m
    else
      let byte = Char.code @@ String.get str (src + i) in
      let addr = Symbolic_i32.of_int (dst + i) in
      let* m =
        store_8 m ~addr (Smtml.Typed.Bitv32.v (Smtml.Bitvector.of_int8 byte))
      in
      loop (i + 1) m
  in
  loop 0 m

let get_limit_max { limits; _ } =
  match limits with
  | I32 { max; _ } -> Option.map Int32.to_int max
  | I64 { max; _ } -> max

let get_min : Binary.Mem.Type.limits -> int = function
  | I32 { min; _ } -> Int32.to_int min
  | I64 { min; _ } -> min

let init limits =
  let size = get_min limits in
  { data = Map.empty; size = Symbolic_i32.of_int size; limits }

let get_limits { limits; _ } = limits
