(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module Expr = Smtml.Expr
module Value = Symbolic_value
open Expr

let page_size = Symbolic_value.const_i32 65_536l

module Make (Backend : Symbolic_memory_intf.M) = struct
  type t =
    { data : Backend.t
    ; mutable size : Value.int32
    }

  type address = Backend.address

  let create size = { data = Backend.create (); size = Value.const_i32 size }

  let ptr p = Backend.ptr p

  let address a = Backend.address a

  let address_i32 a = Backend.address_i32 a

  let i32 v =
    match view v with
    | Val (Num (I32 i)) -> i
    | _ -> Log.err {|Unsupported symbolic value reasoning over "%a"|} Expr.pp v

  let grow m delta =
    let old_size = Value.I32.mul m.size page_size in
    let new_size = Value.I32.(div (add old_size delta) page_size) in
    m.size <-
      Value.Bool.select_expr
        (Value.I32.gt new_size m.size)
        ~if_true:new_size ~if_false:m.size

  let size { size; _ } = Value.I32.mul size page_size

  let size_in_pages { size; _ } = size

  let fill _ = assert false

  let blit _ = assert false

  let blit_string m str ~src ~dst ~len =
    (* This function is only used in memory init so everything will be concrete *)
    let str_len = String.length str in
    let mem_len = Int32.(to_int (i32 m.size) * to_int (i32 page_size)) in
    let src = Int32.to_int @@ i32 src in
    let dst = Int32.to_int @@ i32 dst in
    let len = Int32.to_int @@ i32 len in
    if
      src < 0 || dst < 0 || len < 0
      || src + len > str_len
      || dst + len > mem_len
    then Value.Bool.const true
    else begin
      for i = 0 to len - 1 do
        let byte = Char.code @@ String.get str (src + i) in
        let dst = Backend.address_i32 (Int32.of_int (dst + i)) in
        Backend.storen m.data dst (value (Num (I8 byte))) 1
      done;
      Value.Bool.const false
    end

  let clone m = { data = Backend.clone m.data; size = m.size }

  let load_8_s m a =
    let v = Backend.loadn m.data a 1 in
    match view v with
    | Val (Num (I8 i8)) -> Value.const_i32 (Int32.extend_s 8 (Int32.of_int i8))
    | _ -> cvtop (Ty_bitv 32) (Sign_extend 24) v

  let load_8_u m a =
    let v = Backend.loadn m.data a 1 in
    match view v with
    | Val (Num (I8 i)) -> Value.const_i32 (Int32.of_int i)
    | _ -> cvtop (Ty_bitv 32) (Zero_extend 24) v

  let load_16_s m a =
    let v = Backend.loadn m.data a 2 in
    match view v with
    | Val (Num (I32 i16)) -> Value.const_i32 (Int32.extend_s 16 i16)
    | _ -> cvtop (Ty_bitv 32) (Sign_extend 16) v

  let load_16_u m a =
    let v = Backend.loadn m.data a 2 in
    match view v with
    | Val (Num (I32 _)) -> v
    | _ -> cvtop (Ty_bitv 32) (Zero_extend 16) v

  let load_32 m a = Backend.loadn m.data a 4

  let load_64 m a = Backend.loadn m.data a 8

  let store_8 m ~addr v = Backend.storen m.data addr v 1

  let store_16 m ~addr v = Backend.storen m.data addr v 2

  let store_32 m ~addr v = Backend.storen m.data addr v 4

  let store_64 m ~addr v = Backend.storen m.data addr v 8

  let get_limit_max _m = None (* TODO *)

  let is_within_bounds m a = Backend.is_within_bounds m.data a

  let free m ptr = Backend.free m.data ptr

  let realloc m ptr size = Backend.realloc m.data ptr size
end

module Backend = Symbolic_memory_backend.Make (Symbolic_memory_choice)
include Make (Backend)

module ITbl = Hashtbl.Make (struct
  include Int

  let hash x = x
end)

type collection = t ITbl.t Env_id.Tbl.t

let init () = Env_id.Tbl.create 0

let iter f collection = Env_id.Tbl.iter (fun _ tbl -> f tbl) collection

let clone collection =
  (* TODO: this is ugly and should be rewritten *)
  let s = Env_id.Tbl.to_seq collection in
  Env_id.Tbl.of_seq
  @@ Seq.map
       (fun (i, t) ->
         let s = ITbl.to_seq t in
         (i, ITbl.of_seq @@ Seq.map (fun (i, a) -> (i, clone a)) s) )
       s

let convert (orig_mem : Concrete_memory.t) : t =
  let s = Concrete_memory.size_in_pages orig_mem in
  create s

let get_env env_id memories =
  match Env_id.Tbl.find_opt memories env_id with
  | Some env -> env
  | None ->
    let t = ITbl.create 0 in
    Env_id.Tbl.add memories env_id t;
    t

let get_memory env_id (orig_memory : Concrete_memory.t) collection g_id =
  let env = get_env env_id collection in
  match ITbl.find_opt env g_id with
  | Some t -> t
  | None ->
    let t = convert orig_memory in
    ITbl.add env g_id t;
    t
