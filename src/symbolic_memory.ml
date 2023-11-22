(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021 Léo Andrès *)
(* Copyright © 2021 Pierre Chambart *)

module Intf = Interpret_functor_intf
module Value = Symbolic_value.S

module M = struct
  module Expr = Encoding.Expr
  module Ty = Encoding.Ty
  open Expr

  let page_size = 65_536

  type int32 = Expr.t

  type int64 = Expr.t

  type t =
    { data : (Int32.t, Expr.t) Hashtbl.t
    ; parent : t Option.t
    ; mutable size : int
    }

  let create size =
    { data = Hashtbl.create 128; parent = None; size = Int32.to_int size }

  let concretize_i32 a =
    match a with Val (Num (I32 i)) -> i | _ -> assert false

  let grow m delta =
    let delta = Int32.to_int @@ concretize_i32 delta.e in
    let old_size = m.size * page_size in
    m.size <- max m.size ((old_size + delta) / page_size)

  let size { size; _ } = Value.const_i32 @@ Int32.of_int (size * page_size)

  let size_in_pages { size; _ } = Value.const_i32 @@ Int32.of_int @@ size

  let fill _ = assert false

  let blit _ = assert false

  let blit_string m str ~src ~dst ~len =
    (* Always concrete? *)
    let src = Int32.to_int @@ concretize_i32 src.e in
    let dst = Int32.to_int @@ concretize_i32 dst.e in
    let len = Int32.to_int @@ concretize_i32 len.e in
    if
      src < 0 || dst < 0
      || src + len > String.length str
      || dst + len > m.size * page_size
    then Value.Bool.const true
    else begin
      for i = 0 to len - 1 do
        let byte = Int32.of_int @@ Char.code @@ String.get str (src + i) in
        let dst = Int32.of_int (dst + i) in
        Hashtbl.replace m.data dst (Value.const_i32 byte)
      done;
      Value.Bool.const false
    end

  let clone m = { data = Hashtbl.create 0; parent = Some m; size = m.size }

  let rec load_byte_opt a m =
    match Hashtbl.find_opt m.data a with
    | Some b -> Some b
    | None -> Option.bind m.parent (load_byte_opt a)

  let load_byte m a = Option.value (load_byte_opt a m) ~default:Value.I32.zero

  let concat ~msb ~lsb offset =
    assert (offset > 0 && offset <= 8);
    let merge_extracts (e1, h, m1) (e2, m2, l) =
      if not (m1 = m2 && Expr.equal e1 e2) then
        Expr.(
          Concat (Extract (e1, h, m1) @: e1.ty, Extract (e2, m2, l) @: e1.ty)
          @: e1.ty )
      else if h - l = Ty.size e1.ty then e1
      else Extract (e1, h, l) @: e1.ty
    in
    match (msb.e, lsb.e) with
    | Val (Num (I32 i1)), Val (Num (I32 i2)) ->
      let offset = offset * 8 in
      if offset < 32 then
        Value.const_i32 Int32.(logor (shl i1 (of_int offset)) i2)
      else
        let i1' = Int64.of_int32 i1 in
        let i2' = Int64.of_int32 i2 in
        Value.const_i64 Int64.(logor (shl i1' (of_int offset)) i2')
    | Val (Num (I32 i1)), Val (Num (I64 i2)) ->
      let offset = Int64.of_int (offset * 8) in
      Value.const_i64 Int64.(logor (shl (of_int32 i1) offset) i2)
    | Extract (e1, h, m1), Extract (e2, m2, l) ->
      merge_extracts (e1, h, m1) (e2, m2, l)
    | Extract (e1, h, m1), Concat ({ e = Extract (e2, m2, l); _ }, e3) ->
      let ty : Ty.t = if offset >= 4 then Ty_bitv S64 else Ty_bitv S32 in
      Concat (merge_extracts (e1, h, m1) (e2, m2, l), e3) @: ty
    | _ ->
      let ty : Ty.t = if offset >= 4 then Ty_bitv S64 else Ty_bitv S32 in
      Concat (msb, lsb) @: ty

  let loadn m a n : int32 =
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
    let v = loadn m (concretize_i32 a.e) 1 in
    match v.e with
    | Val (Num (I32 i8)) -> Value.const_i32 (Int32.extend_s 8 i8)
    | _ -> Cvtop (ExtS 24, v) @: Ty_bitv S32

  let load_8_u m a =
    let v = loadn m (concretize_i32 a.e) 1 in
    match v.e with
    | Val (Num (I32 _)) -> v
    | _ -> Cvtop (ExtU 24, v) @: Ty_bitv S32

  let load_16_s m a =
    let v = loadn m (concretize_i32 a.e) 2 in
    match v.e with
    | Val (Num (I32 i16)) -> Value.const_i32 (Int32.extend_s 16 i16)
    | _ -> Cvtop (ExtS 16, v) @: Ty_bitv S32

  let load_16_u m a =
    let v = loadn m (concretize_i32 a.e) 2 in
    match v.e with
    | Val (Num (I32 _)) -> v
    | _ -> Cvtop (ExtU 16, v) @: Ty_bitv S32

  let load_32 m a = loadn m (concretize_i32 a.e) 4

  let load_64 m a = loadn m (concretize_i32 a.e) 8

  let extract v pos =
    match v.e with
    | Val (Num (I32 i)) ->
      let i' = Int32.(logand 0xffl @@ shr_s i @@ of_int (pos * 8)) in
      Value.const_i32 i'
    | Val (Num (I64 i)) ->
      let i' = Int64.(logand 0xffL @@ shr_s i @@ of_int (pos * 8)) in
      Value.const_i32 (Int32.of_int64 i')
    | _ -> Extract (v, pos + 1, pos) @: Ty_bitv S8

  let storen m ~addr v n =
    let a0 = concretize_i32 addr.e in
    for i = 0 to n - 1 do
      let addr' = Int32.add a0 (Int32.of_int i) in
      let v' = extract v i in
      Hashtbl.replace m.data addr' v'
    done

  let store_8 m ~addr v = storen m ~addr v 1

  let store_16 m ~addr v = storen m ~addr v 2

  let store_32 m ~addr v = storen m ~addr v 4

  let store_64 m ~addr v = storen m ~addr v 8

  let get_limit_max _m = None (* TODO *)
end

module M' : Intf.Memory_data = M

module ITbl = Hashtbl.Make (struct
  include Int

  let hash x = x
end)

type memories = M.t ITbl.t Env_id.Tbl.t

let init () = Env_id.Tbl.create 0

let clone (memories : memories) : memories =
  let s = Env_id.Tbl.to_seq memories in
  Env_id.Tbl.of_seq
  @@ Seq.map
       (fun (i, t) ->
         let s = ITbl.to_seq t in
         (i, ITbl.of_seq @@ Seq.map (fun (i, a) -> (i, M.clone a)) s) )
       s

let convert (orig_mem : Concrete_memory.t) : M.t =
  let s = Concrete_memory.size_in_pages orig_mem in
  M.create s

let get_env env_id memories =
  match Env_id.Tbl.find_opt memories env_id with
  | Some env -> env
  | None ->
    let t = ITbl.create 0 in
    Env_id.Tbl.add memories env_id t;
    t

let get_memory env_id (orig_memory : Concrete_memory.t) (memories : memories)
  g_id =
  let env = get_env env_id memories in
  match ITbl.find_opt env g_id with
  | Some t -> t
  | None ->
    let t = convert orig_memory in
    ITbl.add env g_id t;
    t
