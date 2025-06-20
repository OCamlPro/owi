(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(** Single memories *)

let page_size = Symbolic_value.const_i32 65_536l

module Map = Map.Make (struct
  include Int32

  (* TODO: define this in Int32 directly? *)
  let compare i1 i2 = compare (Int32.to_int i1) (Int32.to_int i2)
end)

type t =
  { mutable data : Symbolic_value.int32 Map.t
  ; mutable size : Symbolic_value.int32
  }

let create size = { data = Map.empty; size = Symbolic_value.const_i32 size }

let i32 v =
  match Smtml.Expr.view v with
  | Val (Bitv i) when Smtml.Bitvector.numbits i = 32 ->
    Smtml.Bitvector.to_int32 i
  | _ -> assert false

let grow m delta =
  let old_size = Symbolic_value.I32.mul m.size page_size in
  let new_size = Symbolic_value.I32.(div (add old_size delta) page_size) in
  m.size <-
    Symbolic_value.Bool.select_expr
      (Symbolic_value.I32.gt new_size m.size)
      ~if_true:new_size ~if_false:m.size

let size { size; _ } = Symbolic_value.I32.mul size page_size

let size_in_pages { size; _ } = size

let replace m k v = m.data <- Map.add k v m.data

let blit_string m str ~src ~dst ~len =
  (* This function is only used in memory init so everything will be concrete *)
  (* TODO: I am not sure this is completely true... this should be investigated and fixed at some point *)
  let src = Int32.to_int @@ i32 src in
  let dst = Int32.to_int @@ i32 dst in
  let len = Int32.to_int @@ i32 len in
  begin
    for i = 0 to len - 1 do
      let byte = Char.code @@ String.get str (src + i) in
      let dst = Int32.of_int (dst + i) in
      replace m dst (Smtml.Expr.value (Bitv (Smtml.Bitvector.of_int8 byte)))
    done
  end

let load_byte a { data; _ } =
  match Map.find_opt a data with
  | None -> Smtml.Expr.value (Bitv (Smtml.Bitvector.of_int8 0))
  | Some v -> v

let loadn m a n =
  let rec loop addr size i acc =
    if i = size then acc
    else
      let addr' = Int32.(add addr (of_int i)) in
      let byte = load_byte addr' m in
      loop addr size (i + 1) (Smtml.Expr.concat byte acc)
  in
  let v0 = load_byte a m in
  loop a n 1 v0

let load_8_s m a =
  let v = loadn m (i32 a) 1 in
  match Smtml.Expr.view v with
  | Val (Bitv i8) when Smtml.Bitvector.numbits i8 = 8 ->
    let i8 = Smtml.Bitvector.to_int32 i8 in
    Symbolic_value.const_i32 (Int32.extend_s 8 i8)
  | _ -> Smtml.Expr.cvtop (Ty_bitv 32) (Sign_extend 24) v

let load_8_u m a =
  let v = loadn m (i32 a) 1 in
  match Smtml.Expr.view v with
  | Val (Bitv i) when Smtml.Bitvector.numbits i = 8 ->
    let i = Smtml.Bitvector.to_int32 i in
    Symbolic_value.const_i32 i
  | _ -> Smtml.Expr.cvtop (Ty_bitv 32) (Zero_extend 24) v

let load_16_s m a =
  let v = loadn m (i32 a) 2 in
  match Smtml.Expr.view v with
  | Val (Bitv i16) when Smtml.Bitvector.numbits i16 = 16 ->
    let i16 = Smtml.Bitvector.to_int32 i16 in
    Symbolic_value.const_i32 (Int32.extend_s 16 i16)
  | _ -> Smtml.Expr.cvtop (Ty_bitv 32) (Sign_extend 16) v

let load_16_u m a =
  let v = loadn m (i32 a) 2 in
  match Smtml.Expr.view v with
  | Val (Bitv i16) when Smtml.Bitvector.numbits i16 = 16 ->
    let i16 = Smtml.Bitvector.to_int32 i16 in
    Symbolic_value.const_i32 i16
  | _ -> Smtml.Expr.cvtop (Ty_bitv 32) (Zero_extend 16) v

let load_32 m a = loadn m (i32 a) 4

let load_64 m a = loadn m (i32 a) 8

let storen m ~addr v n =
  let a0 = i32 addr in
  for i = 0 to n - 1 do
    let addr' = Int32.add a0 (Int32.of_int i) in
    let v' = Smtml.Expr.extract v ~low:i ~high:(i + 1) in
    replace m addr' v'
  done

let store_8 m ~addr v = storen m ~addr v 1

let store_16 m ~addr v = storen m ~addr v 2

let store_32 m ~addr v = storen m ~addr v 4

let store_64 m ~addr v = storen m ~addr v 8

let get_limit_max _m = None (* TODO *)

(** Collection of memories *)

module ITbl = Hashtbl.Make (struct
  include Int

  let hash x = x
end)

type collection = t ITbl.t Env_id.Tbl.t

let init () = Env_id.Tbl.create 0

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
