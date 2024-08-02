(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

include Symbolic_memory_intf

let page_size = Symbolic_value.const_i32 65_536l

module Make (Backend : M) = struct
  type t =
    { data : Backend.t
    ; mutable size : Symbolic_value.int32
    }

  let create size =
    { data = Backend.make (); size = Symbolic_value.const_i32 size }

  let i32 v =
    match Smtml.Expr.view v with Val (Num (I32 i)) -> i | _ -> assert false

  let grow m delta =
    let old_size = Symbolic_value.I32.mul m.size page_size in
    let new_size = Symbolic_value.I32.(div (add old_size delta) page_size) in
    m.size <-
      Symbolic_value.Bool.select_expr
        (Symbolic_value.I32.gt new_size m.size)
        ~if_true:new_size ~if_false:m.size

  let size { size; _ } = Symbolic_value.I32.mul size page_size

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
    then Symbolic_value.Bool.const true
    else begin
      for i = 0 to len - 1 do
        let byte = Char.code @@ String.get str (src + i) in
        let a = Backend.address_i32 (Int32.of_int (dst + i)) in
        Backend.storen m.data a (Smtml.Expr.value (Num (I8 byte))) 1
      done;
      Symbolic_value.Bool.const false
    end

  let clone m = { data = Backend.clone m.data; size = m.size }

  let must_be_valid_address m a =
    let open Symbolic_choice_without_memory in
    let* addr = Backend.validate_address m a in
    match addr with Error t -> trap t | Ok ptr -> Backend.address ptr

  let load_8_s m a =
    let open Symbolic_choice_without_memory in
    let+ a = must_be_valid_address m.data a in
    let v = Backend.loadn m.data a 1 in
    match Smtml.Expr.view v with
    | Val (Num (I8 i8)) ->
      Symbolic_value.const_i32 (Int32.extend_s 8 (Int32.of_int i8))
    | _ -> Smtml.Expr.cvtop (Ty_bitv 32) (Sign_extend 24) v

  let load_8_u m a =
    let open Symbolic_choice_without_memory in
    let+ a = must_be_valid_address m.data a in
    let v = Backend.loadn m.data a 1 in
    match Smtml.Expr.view v with
    | Val (Num (I8 i)) -> Symbolic_value.const_i32 (Int32.of_int i)
    | _ -> Smtml.Expr.cvtop (Ty_bitv 32) (Zero_extend 24) v

  let load_16_s m a =
    let open Symbolic_choice_without_memory in
    let+ a = must_be_valid_address m.data a in
    let v = Backend.loadn m.data a 2 in
    match Smtml.Expr.view v with
    | Val (Num (I32 i16)) -> Symbolic_value.const_i32 (Int32.extend_s 16 i16)
    | _ -> Smtml.Expr.cvtop (Ty_bitv 32) (Sign_extend 16) v

  let load_16_u m a =
    let open Symbolic_choice_without_memory in
    let+ a = must_be_valid_address m.data a in
    let v = Backend.loadn m.data a 2 in
    match Smtml.Expr.view v with
    | Val (Num (I32 _)) -> v
    | _ -> Smtml.Expr.cvtop (Ty_bitv 32) (Zero_extend 16) v

  let load_32 m a =
    let open Symbolic_choice_without_memory in
    let+ a = must_be_valid_address m.data a in
    Backend.loadn m.data a 4

  let load_64 m a =
    let open Symbolic_choice_without_memory in
    let+ a = must_be_valid_address m.data a in
    Backend.loadn m.data a 8

  let store_8 m ~addr v =
    let open Symbolic_choice_without_memory in
    let+ a = must_be_valid_address m.data addr in
    Backend.storen m.data a v 1

  let store_16 m ~addr v =
    let open Symbolic_choice_without_memory in
    let+ a = must_be_valid_address m.data addr in
    Backend.storen m.data a v 2

  let store_32 m ~addr v =
    let open Symbolic_choice_without_memory in
    let+ a = must_be_valid_address m.data addr in
    Backend.storen m.data a v 4

  let store_64 m ~addr v =
    let open Symbolic_choice_without_memory in
    let+ a = must_be_valid_address m.data addr in
    Backend.storen m.data a v 8

  let get_limit_max _m = None (* TODO *)

  let free m base = Backend.free m.data base

  let realloc m ~ptr ~size = Backend.realloc m.data ~ptr ~size

  (* TODO: Move this into a separate module? *)
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
end
