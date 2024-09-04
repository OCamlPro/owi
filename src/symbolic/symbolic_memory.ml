(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module Value = Symbolic_value
module Expr = Smtml.Expr
module Ty = Smtml.Ty
open Expr

let page_size = Symbolic_value.const_i32 65_536l

type t =
  { data : (Int32.t, Value.int32) Hashtbl.t option ref
  ; parent : t option
  ; mutable size : Value.int32
  ; chunks : (Int32.t, Value.int32) Hashtbl.t
  }

let create size =
  { data = ref None
  ; parent = None
  ; size = Value.const_i32 size
  ; chunks = Hashtbl.create 16
  }

let i32 v = match view v with Val (Num (I32 i)) -> i | _ -> assert false

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

let replace m k v =
  match !(m.data) with
  | None ->
    let tbl = Hashtbl.create 16 in
    Hashtbl.add tbl k v;
    m.data := Some tbl
  | Some tbl -> Hashtbl.replace tbl k v

let blit_string m str ~src ~dst ~len =
  (* This function is only used in memory init so everything will be concrete *)
  let str_len = String.length str in
  let mem_len = Int32.(to_int (i32 m.size) * to_int (i32 page_size)) in
  let src = Int32.to_int @@ i32 src in
  let dst = Int32.to_int @@ i32 dst in
  let len = Int32.to_int @@ i32 len in
  if src < 0 || dst < 0 || len < 0 || src + len > str_len || dst + len > mem_len
  then Value.Bool.const true
  else begin
    for i = 0 to len - 1 do
      let byte = Char.code @@ String.get str (src + i) in
      let dst = Int32.of_int (dst + i) in
      replace m dst (make (Val (Num (I8 byte))))
    done;
    Value.Bool.const false
  end

let clone m =
  let parent = if Option.is_none !(m.data) then m.parent else Some m in
  { data = ref None
  ; parent
  ; size = m.size
  ; chunks = Hashtbl.copy m.chunks (* TODO: we can make this lazy as well *)
  }

let rec load_byte { parent; data; _ } a =
  let v =
    match !data with None -> None | Some data -> Hashtbl.find_opt data a
  in
  match v with
  | Some v -> v
  | None -> (
    match parent with
    | None -> make (Val (Num (I8 0)))
    | Some parent -> load_byte parent a )

(* TODO: don't rebuild so many values it generates unecessary hc lookups *)
let merge_extracts (e1, h, m1) (e2, m2, l) =
  let ty = Expr.ty e1 in
  if m1 = m2 && Expr.equal e1 e2 then
    if h - l = Ty.size ty then e1 else make (Extract (e1, h, l))
  else make (Concat (make (Extract (e1, h, m1)), make (Extract (e2, m2, l))))

let concat ~msb ~lsb offset =
  assert (offset > 0 && offset <= 8);
  match (view msb, view lsb) with
  | Val (Num (I8 i1)), Val (Num (I8 i2)) ->
    Value.const_i32 Int32.(logor (shl (of_int i1) 8l) (of_int i2))
  | Val (Num (I8 i1)), Val (Num (I32 i2)) ->
    let offset = offset * 8 in
    if offset < 32 then
      Value.const_i32 Int32.(logor (shl (of_int i1) (of_int offset)) i2)
    else
      let i1' = Int64.of_int i1 in
      let i2' = Int64.of_int32 i2 in
      Value.const_i64 Int64.(logor (shl i1' (of_int offset)) i2')
  | Val (Num (I8 i1)), Val (Num (I64 i2)) ->
    let offset = Int64.of_int (offset * 8) in
    Value.const_i64 Int64.(logor (shl (of_int i1) offset) i2)
  | Extract (e1, h, m1), Extract (e2, m2, l) ->
    merge_extracts (e1, h, m1) (e2, m2, l)
  | Extract (e1, h, m1), Concat ({ node = Extract (e2, m2, l); _ }, e3) ->
    make (Concat (merge_extracts (e1, h, m1) (e2, m2, l), e3))
  | _ -> make (Concat (msb, lsb))

let loadn m a n =
  let rec loop addr size i acc =
    if i = size then acc
    else
      let addr' = Int32.(add addr (of_int i)) in
      let byte = load_byte m addr' in
      loop addr size (i + 1) (concat i ~msb:byte ~lsb:acc)
  in
  let v0 = load_byte m a in
  loop a n 1 v0

let load_8_s m a =
  let v = loadn m (i32 a) 1 in
  match view v with
  | Val (Num (I8 i8)) -> Value.const_i32 (Int32.extend_s 8 (Int32.of_int i8))
  | _ -> cvtop (Ty_bitv 32) (Sign_extend 24) v

let load_8_u m a =
  let v = loadn m (i32 a) 1 in
  match view v with
  | Val (Num (I8 i)) -> Value.const_i32 (Int32.of_int i)
  | _ -> cvtop (Ty_bitv 32) (Zero_extend 24) v

let load_16_s m a =
  let v = loadn m (i32 a) 2 in
  match view v with
  | Val (Num (I32 i16)) -> Value.const_i32 (Int32.extend_s 16 i16)
  | _ -> cvtop (Ty_bitv 32) (Sign_extend 16) v

let load_16_u m a =
  let v = loadn m (i32 a) 2 in
  match view v with
  | Val (Num (I32 _)) -> v
  | _ -> cvtop (Ty_bitv 32) (Zero_extend 16) v

let load_32 m a = loadn m (i32 a) 4

let load_64 m a = loadn m (i32 a) 8

let extract v pos =
  match view v with
  | Val (Num (I32 i)) ->
    let i' = Int32.(to_int @@ logand 0xffl @@ shr_s i @@ of_int (pos * 8)) in
    value (Num (I8 i'))
  | Val (Num (I64 i)) ->
    let i' = Int64.(to_int @@ logand 0xffL @@ shr_s i @@ of_int (pos * 8)) in
    value (Num (I8 i'))
  | Cvtop
      ( _
      , (Zero_extend 24 | Sign_extend 24)
      , ({ node = Symbol { ty = Ty_bitv 8; _ }; _ } as sym) ) ->
    sym
  | _ -> make (Extract (v, pos + 1, pos))

let storen m ~addr v n =
  let a0 = i32 addr in
  for i = 0 to n - 1 do
    let addr' = Int32.add a0 (Int32.of_int i) in
    let v' = extract v i in
    replace m addr' v'
  done

let store_8 m ~addr v = storen m ~addr v 1

let store_16 m ~addr v = storen m ~addr v 2

let store_32 m ~addr v = storen m ~addr v 4

let store_64 m ~addr v = storen m ~addr v 8

let get_limit_max _m = None (* TODO *)

let check_within_bounds m a =
  match view a with
  | Val (Num (I32 _)) -> Ok (Value.Bool.const false, a)
  | Ptr { base; offset } -> (
    match Hashtbl.find_opt m.chunks base with
    | None -> Error Trap.Memory_leak_use_after_free
    | Some size ->
      let ptr = Int32.add base (i32 offset) in
      let upper_bound =
        Value.(I32.ge (const_i32 ptr) (I32.add (const_i32 base) size))
      in
      Ok
        ( Value.Bool.(or_ (const (Int32.lt ptr base)) upper_bound)
        , Value.const_i32 ptr ) )
  | _ -> assert false

let free m base =
  if not @@ Hashtbl.mem m.chunks base then
    Fmt.failwith "Memory leak double free";
  Hashtbl.remove m.chunks base

let realloc m base size = Hashtbl.replace m.chunks base size

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
