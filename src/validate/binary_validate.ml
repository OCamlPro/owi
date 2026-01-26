(* SPDX-License-Identifier: AGPL-3.0-or-later *)

(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Binary
open Module
open Syntax
open Fmt

type typ =
  | Num_type of Text.num_type
  | Ref_type of ref_type
  | Any
  | Something

let sp fmt () = Fmt.string fmt " "

let pp_typ fmt = function
  | Num_type t -> Text.pp_num_type fmt t
  | Ref_type t -> pp_ref_type fmt t
  | Any -> string fmt "any"
  | Something -> string fmt "something"

let pp_typ_list fmt l = list ~sep:sp pp_typ fmt l

let typ_of_val_type = function
  | Binary.Ref_type rt -> Ref_type rt
  | Num_type t -> Num_type t

let typ_of_pt pt = typ_of_val_type @@ snd pt

module Index = struct
  module M = Int
  module Map = Map.Make (Int)
  include M
end

let check_mem modul n =
  if n >= Array.length modul.mem then Error (`Unknown_memory (Text.Raw n))
  else Ok ()

let check_data modul n =
  if n >= Array.length modul.data then Error (`Unknown_data (Text.Raw n))
  else Ok ()

let check_memarg (memarg : Text.memarg) align =
  (* TODO: whether an offset is out of range or not should be determined by
  the memory, but memory64 is not yet supported. *)
  if not (Int64.fits_in_u32 memarg.offset) then Error `Offset_out_of_range
  else if Int64.ge memarg.align align then Error `Alignment_too_large
  else Ok ()

module Env = struct
  type local =
    { typ : typ
    ; param : bool
    ; init : bool
    }

  type t =
    { locals : local Index.Map.t
    ; result_type : result_type
    ; blocks : typ list list
    ; modul : Module.t
    ; refs : (int, unit) Hashtbl.t
    }

  let type_get i m =
    match Module.get_type i m with
    | None -> Error (`Unknown_type (Text.Raw i))
    | Some (_, ty) -> Ok ty

  let local_get i env =
    match Index.Map.find_opt i env.locals with
    | None -> Error (`Unknown_local (Text.Raw i))
    | Some v -> Ok v

  let local_set_init i env =
    match Index.Map.find_opt i env.locals with
    | None -> Error (`Unknown_local (Text.Raw i))
    | Some ({ param = false; init = false; _ } as t) ->
      Ok { env with locals = Index.Map.add i { t with init = true } env.locals }
    | _ -> Ok env

  let global_get i modul =
    if i >= Array.length modul.global then Error (`Unknown_global (Text.Raw i))
    else
      match modul.global.(i) with
      | Origin.Local { typ; _ } | Imported { typ; _ } -> Ok typ

  let func_get i modul =
    if i >= Array.length modul.func then Error (`Unknown_func (Text.Raw i))
    else
      match modul.func.(i) with
      | Origin.Local { Func.type_f = Bt_raw (_, t); _ }
      | Imported { typ = Bt_raw (_, t); _ } ->
        Ok t

  let block_type_get i env =
    match List.nth_opt env.blocks i with
    | None -> Error (`Unknown_label (Text.Raw i))
    | Some bt -> Ok bt

  let table_type_get i (modul : Module.t) =
    if i >= Array.length modul.table then Error (`Unknown_table (Text.Raw i))
    else
      match modul.table.(i) with
      | Origin.Local { typ = _, t; _ } | Imported { typ = _, t; _ } -> Ok t

  let elem_type_get i modul =
    if i >= Array.length modul.elem then Error (`Unknown_elem (Text.Raw i))
    else match modul.elem.(i) with value -> Ok value.typ

  let tag_get i modul =
    if i >= Array.length modul.tag then Error (`Unknown_tag (Text.Raw i))
    else
      match modul.tag.(i) with
      | Origin.Local { typ; _ } -> Ok typ
      | Imported { typ; _ } -> Ok typ

  let make ~params ~locals ~modul ~result_type ~refs =
    let cnt, l =
      List.fold_left
        (fun (cnt, l) (_, typ) ->
          let typ = typ_of_val_type typ in
          (cnt + 1, Index.Map.add cnt { typ; param = true; init = false } l) )
        (0, Index.Map.empty) params
    in
    let _, l =
      List.fold_left
        (fun (cnt, l) (_, typ) ->
          let typ = typ_of_val_type typ in
          (cnt + 1, Index.Map.add cnt { typ; param = false; init = false } l) )
        (cnt, l) locals
    in
    { locals = l; modul; result_type; blocks = []; refs }
end

type stack = typ list

let i32 = Num_type I32

let i64 = Num_type I64

let f32 = Num_type F32

let f64 = Num_type F64

let v128 = Num_type V128

let any = Any

let itype = function Text.S32 -> i32 | S64 -> i64

let ftype = function Text.S32 -> f32 | S64 -> f64

(* TODO: Maybe add the type id to Bt_raw to avoid having to look it this way? *)
let get_func_type_id (env : Env.t) i =
  let typ =
    match env.modul.func.(i) with
    | Origin.Local { type_f = Bt_raw (_, typ); _ } -> typ
    | Imported { typ = Bt_raw (_, typ); _ } -> typ
  in
  Array.find_index (fun (_, typ') -> func_type_eq typ typ') env.modul.types

(* TODO: move type matching functions outside of the stack module? *)
module Stack : sig
  type t = typ list

  val drop : t -> t Result.t

  val pop : Module.t -> t -> t -> t Result.t

  val push : t -> t -> t Result.t

  val pop_push : Module.t -> block_type -> t -> t Result.t

  val pop_ref : t -> (typ * t) Result.t

  val equal : Module.t -> t -> t -> bool Result.t

  val match_heap_type :
       ?subtype:bool
    -> Module.t
    -> expected:heap_type
    -> got:heap_type
    -> bool Result.t

  val match_ref_type :
       ?subtype:bool
    -> Module.t
    -> expected:ref_type
    -> got:ref_type
    -> bool Result.t

  val match_types :
    ?subtype:bool -> Module.t -> expected:typ -> got:typ -> bool Result.t

  val pp : t Fmt.t

  val match_prefix : Module.t -> prefix:t -> stack:t -> t option Result.t
end = struct
  type t = typ list

  let pp fmt (s : stack) = pf fmt "[%a]" pp_typ_list s

  let match_num_type (required : Text.num_type) (got : Text.num_type) =
    match (required, got) with
    | I32, I32 -> Ok true
    | I64, I64 -> Ok true
    | F32, F32 -> Ok true
    | F64, F64 -> Ok true
    | V128, V128 -> Ok true
    | _, _ -> Ok false

  (* TODO: replace this by a comparison function, that returns 0 if the types
     are equal, and -1 if typ1 is a subtype of typ2, 1 otherwise *)
  let match_heap_type ?(subtype = false) modul ~expected ~got =
    match (got, expected) with
    | Func_ht, Func_ht
    | Extern_ht, Extern_ht
    | Any_ht, Any_ht
    | None_ht, None_ht
    | NoFunc_ht, NoFunc_ht
    | Exn_ht, Exn_ht
    | NoExn_ht, NoExn_ht
    | NoExtern_ht, NoExtern_ht ->
      Ok true
    | TypeUse id1, TypeUse id2 ->
      (* TODO: add subtyping check *)
      let* pt1, rt1 = Env.type_get id1 modul in
      let* pt2, rt2 = Env.type_get id2 modul in
      let res =
        List.compare_lengths pt1 pt2 = 0
        && List.compare_lengths rt1 rt2 = 0
        && List.for_all2 (fun (_, t1) (_, t2) -> val_type_eq t1 t2) pt1 pt2
        && List.for_all2 val_type_eq rt1 rt2
      in
      Ok res
    (* TODO: proper subtype checking *)
    | TypeUse _, Func_ht
    | NoFunc_ht, (TypeUse _ | Func_ht)
    | NoExn_ht, Exn_ht
    | NoExtern_ht, Extern_ht
    | None_ht, Any_ht
      when subtype ->
      Ok true
    | TypeUse id, Func_ht ->
      (* TODO: not ideal *)
      let* pt1, rt1 = Env.type_get id modul in
      if List.is_empty pt1 && List.is_empty rt1 then Ok true else Ok false
    | _ -> Ok false

  let match_ref_type ?(subtype = false) modul ~expected ~got =
    match (expected, got) with
    | (Text.Null, expected), (Text.No_null, got)
    | (Text.No_null, expected), (Text.No_null, got)
    | (Text.Null, expected), (Text.Null, got) ->
      match_heap_type ~subtype modul ~expected ~got
    | (Text.No_null, _), (Text.Null, _) ->
      Error
        (`Type_mismatch
           (Fmt.str "match_ref_type Expected %a got %a" pp_ref_type expected
              pp_ref_type got ) )

  let match_types ?subtype env ~expected ~got =
    match (expected, got) with
    | Something, _ | _, Something -> Ok true
    | Any, _ | _, Any -> Ok true
    | Num_type expected, Num_type got -> match_num_type expected got
    | Ref_type expected, Ref_type got ->
      match_ref_type ?subtype env ~expected ~got
    | Num_type _, Ref_type _ | Ref_type _, Num_type _ -> Ok false

  let rec equal env s s' =
    match (s, s') with
    | [], s | s, [] -> Ok (List.for_all (function Any -> true | _ -> false) s)
    | Any :: tl, Any :: tl' ->
      let* b = equal env tl s' in
      if b then Ok true else equal env s tl'
    | Any :: tl, hd :: tl' | hd :: tl', Any :: tl ->
      let* b = equal env tl (hd :: tl') in
      if b then Ok true else equal env (Any :: tl) tl'
    | hd :: tl, hd' :: tl' ->
      let* b = match_types ~subtype:true env ~expected:hd ~got:hd' in
      if not b then Ok false else equal env tl tl'

  let rec match_prefix modul ~prefix ~stack : t option Result.t =
    match (prefix, stack) with
    | [], stack -> Ok (Some stack)
    | _hd :: _tl, [] -> Ok None
    | _hd :: tl, Any :: tl' ->
      let* m2 = match_prefix modul ~prefix:tl ~stack in
      begin match m2 with
      | None -> match_prefix modul ~prefix ~stack:tl'
      | _ -> Ok m2
      end
    | hd :: tl, hd' :: tl' ->
      let* b = match_types ~subtype:true modul ~expected:hd ~got:hd' in
      if b then match_prefix modul ~prefix:tl ~stack:tl' else Ok None

  let pop env required stack =
    match match_prefix env ~prefix:required ~stack with
    | Ok None ->
      let msg = Fmt.str "expected %a but stack is %a" pp required pp stack in
      Error (`Type_mismatch msg)
    | Ok (Some stack) -> Ok stack
    | Error e -> Error e

  let pop_ref = function
    | (Ref_type _ as typ) :: tl -> Ok (typ, tl)
    | (Something as typ) :: tl -> Ok (typ, tl)
    | (Any as typ) :: _ as stack -> Ok (typ, stack)
    | _ -> Error (`Type_mismatch "pop_ref")

  let drop stack =
    match stack with
    | [] -> Error (`Type_mismatch "drop")
    | Any :: _ -> Ok [ Any ]
    | _ :: tl -> Ok tl

  let push t stack = ok @@ t @ stack

  let pop_push modul (Bt_raw (_, (pt, rt)) : block_type) stack =
    let pt, rt = (List.rev_map typ_of_pt pt, List.rev_map typ_of_val_type rt) in
    let* stack = pop modul pt stack in
    push rt stack
end

let typ_equal modul ~expected ~got =
  match (expected, got) with
  | Num_type t1, Num_type t2 -> Ok (Text.num_type_eq t1 t2)
  | Ref_type expected, Ref_type got ->
    let+ b = Stack.match_ref_type ~subtype:true modul ~expected ~got in
    b
  | Any, _ | _, Any -> Ok true
  | Something, _ | _, Something -> Ok true
  | _, _ -> Ok false

let is_func_type (ref_type : ref_type) =
  match ref_type with _, Func_ht -> true | _ -> false

let ref_type_as_non_null t =
  match t with
  | Ref_type (_, rt) -> Ok (Ref_type (No_null, rt))
  | (Any | Something) as t ->
    (* TODO: what should be done in this case? *)
    Ok t
  | _ -> assert false

let rec typecheck_instr (env : Env.t) (stack : stack) (instr : instr Annotated.t)
  : (Env.t * stack) Result.t =
  match instr.raw with
  | Nop -> Ok (env, stack)
  | Drop ->
    let+ stack = Stack.drop stack in
    (env, stack)
  | Return ->
    let+ _stack =
      Stack.pop env.modul (List.rev_map typ_of_val_type env.result_type) stack
    in
    (env, [ any ])
  | Unreachable -> Ok (env, [ any ])
  | I32_const _ ->
    let+ stack = Stack.push [ i32 ] stack in
    (env, stack)
  | I64_const _ ->
    let+ stack = Stack.push [ i64 ] stack in
    (env, stack)
  | F32_const _ ->
    let+ stack = Stack.push [ f32 ] stack in
    (env, stack)
  | F64_const _ ->
    let+ stack = Stack.push [ f64 ] stack in
    (env, stack)
  | V128_const _ ->
    let+ stack = Stack.push [ v128 ] stack in
    (env, stack)
  | I_unop (s, _op) ->
    let t = itype s in
    let* stack = Stack.pop env.modul [ t ] stack in
    let+ stack = Stack.push [ t ] stack in
    (env, stack)
  | I_binop (s, _op) ->
    let t = itype s in
    let* stack = Stack.pop env.modul [ t; t ] stack in
    let+ stack = Stack.push [ t ] stack in
    (env, stack)
  | F_unop (s, _op) ->
    let t = ftype s in
    let* stack = Stack.pop env.modul [ t ] stack in
    let+ stack = Stack.push [ t ] stack in
    (env, stack)
  | F_binop (s, _op) ->
    let t = ftype s in
    let* stack = Stack.pop env.modul [ t; t ] stack in
    let+ stack = Stack.push [ t ] stack in
    (env, stack)
  | V_ibinop (_shape, _op) ->
    let* stack = Stack.pop env.modul [ v128; v128 ] stack in
    let+ stack = Stack.push [ v128 ] stack in
    (env, stack)
  | I_testop (nn, _) ->
    let* stack = Stack.pop env.modul [ itype nn ] stack in
    let+ stack = Stack.push [ i32 ] stack in
    (env, stack)
  | I_relop (nn, _) ->
    let t = itype nn in
    let* stack = Stack.pop env.modul [ t; t ] stack in
    let+ stack = Stack.push [ i32 ] stack in
    (env, stack)
  | F_relop (nn, _) ->
    let t = ftype nn in
    let* stack = Stack.pop env.modul [ t; t ] stack in
    let+ stack = Stack.push [ i32 ] stack in
    (env, stack)
  | Local_get i ->
    let* { typ = t; param; init } = Env.local_get i env in
    let* () =
      if (not param) && not init then
        let no_def_value =
          match t with Ref_type (No_null, _) -> true | _ -> false
        in
        if no_def_value then Error (`Uninitialized_local i) else Ok ()
      else Ok ()
    in
    let+ stack = Stack.push [ t ] stack in
    (env, stack)
  | Local_set i ->
    let* { typ = t; _ } = Env.local_get i env in
    let* env = Env.local_set_init i env in
    let+ stack = Stack.pop env.modul [ t ] stack in
    (env, stack)
  | Local_tee i ->
    let* { typ = t; _ } = Env.local_get i env in
    let* stack = Stack.pop env.modul [ t ] stack in
    let+ stack = Stack.push [ t ] stack in
    (env, stack)
  | Global_get i ->
    let* _mut, t = Env.global_get i env.modul in
    let t = typ_of_val_type t in
    let+ stack = Stack.push [ t ] stack in
    (env, stack)
  | Global_set i ->
    let* mut, t = Env.global_get i env.modul in
    let* () =
      match mut with Var -> Ok () | Const -> Error `Global_is_immutable
    in
    let t = typ_of_val_type t in
    let+ stack = Stack.pop env.modul [ t ] stack in
    (env, stack)
  | If_else (_id, block_type, e1, e2) ->
    let* stack = Stack.pop env.modul [ i32 ] stack in
    let* stack_e1 = typecheck_expr env e1 ~is_loop:false block_type ~stack in
    let+ _stack_e2 = typecheck_expr env e2 ~is_loop:false block_type ~stack in
    (env, stack_e1)
  | I_load8 (id, nn, _, memarg) ->
    let* () = check_mem env.modul id in
    let* () = check_memarg memarg 1L in
    let* stack = Stack.pop env.modul [ i32 ] stack in
    let+ stack = Stack.push [ itype nn ] stack in
    (env, stack)
  | I_load16 (id, nn, _, memarg) ->
    let* () = check_mem env.modul id in
    let* () = check_memarg memarg 2L in
    let* stack = Stack.pop env.modul [ i32 ] stack in
    let+ stack = Stack.push [ itype nn ] stack in
    (env, stack)
  | I_load (id, nn, memarg) ->
    let* () = check_mem env.modul id in
    let max_allowed = match nn with S32 -> 4L | S64 -> 8L in
    let* () = check_memarg memarg max_allowed in
    let* stack = Stack.pop env.modul [ i32 ] stack in
    let+ stack = Stack.push [ itype nn ] stack in
    (env, stack)
  | I64_load32 (id, _, memarg) ->
    let* () = check_mem env.modul id in
    let* () = check_memarg memarg 4L in
    let* stack = Stack.pop env.modul [ i32 ] stack in
    let+ stack = Stack.push [ i64 ] stack in
    (env, stack)
  | I_store8 (id, nn, memarg) ->
    let* () = check_mem env.modul id in
    let* () = check_memarg memarg 1L in
    let+ stack = Stack.pop env.modul [ itype nn; i32 ] stack in
    (env, stack)
  | I_store16 (id, nn, memarg) ->
    let* () = check_mem env.modul id in
    let* () = check_memarg memarg 2L in
    let+ stack = Stack.pop env.modul [ itype nn; i32 ] stack in
    (env, stack)
  | I_store (id, nn, memarg) ->
    let* () = check_mem env.modul id in
    let max_allowed = match nn with S32 -> 4L | S64 -> 8L in
    let* () = check_memarg memarg max_allowed in
    let+ stack = Stack.pop env.modul [ itype nn; i32 ] stack in
    (env, stack)
  | I64_store32 (id, memarg) ->
    let* () = check_mem env.modul id in
    let* () = check_memarg memarg 4L in
    let+ stack = Stack.pop env.modul [ i64; i32 ] stack in
    (env, stack)
  | F_load (id, nn, memarg) ->
    let* () = check_mem env.modul id in
    let max_allowed = match nn with S32 -> 4L | S64 -> 8L in
    let* () = check_memarg memarg max_allowed in
    let* stack = Stack.pop env.modul [ i32 ] stack in
    let+ stack = Stack.push [ ftype nn ] stack in
    (env, stack)
  | F_store (id, nn, memarg) ->
    let* () = check_mem env.modul id in
    let max_allowed = match nn with S32 -> 4L | S64 -> 8L in
    let* () = check_memarg memarg max_allowed in
    let+ stack = Stack.pop env.modul [ ftype nn; i32 ] stack in
    (env, stack)
  | I_reinterpret_f (inn, fnn) ->
    let* stack = Stack.pop env.modul [ ftype fnn ] stack in
    let+ stack = Stack.push [ itype inn ] stack in
    (env, stack)
  | F_reinterpret_i (fnn, inn) ->
    let* stack = Stack.pop env.modul [ itype inn ] stack in
    let+ stack = Stack.push [ ftype fnn ] stack in
    (env, stack)
  | F32_demote_f64 ->
    let* stack = Stack.pop env.modul [ f64 ] stack in
    let+ stack = Stack.push [ f32 ] stack in
    (env, stack)
  | F64_promote_f32 ->
    let* stack = Stack.pop env.modul [ f32 ] stack in
    let+ stack = Stack.push [ f64 ] stack in
    (env, stack)
  | F_convert_i (fnn, inn, _) ->
    let* stack = Stack.pop env.modul [ itype inn ] stack in
    let+ stack = Stack.push [ ftype fnn ] stack in
    (env, stack)
  | I_trunc_f (inn, fnn, _) | I_trunc_sat_f (inn, fnn, _) ->
    let* stack = Stack.pop env.modul [ ftype fnn ] stack in
    let+ stack = Stack.push [ itype inn ] stack in
    (env, stack)
  | I32_wrap_i64 ->
    let* stack = Stack.pop env.modul [ i64 ] stack in
    let+ stack = Stack.push [ i32 ] stack in
    (env, stack)
  | I_extend8_s nn | I_extend16_s nn ->
    let t = itype nn in
    let* stack = Stack.pop env.modul [ t ] stack in
    let+ stack = Stack.push [ t ] stack in
    (env, stack)
  | I64_extend32_s ->
    let* stack = Stack.pop env.modul [ i64 ] stack in
    let+ stack = Stack.push [ i64 ] stack in
    (env, stack)
  | I64_extend_i32 _ ->
    let* stack = Stack.pop env.modul [ i32 ] stack in
    let+ stack = Stack.push [ i64 ] stack in
    (env, stack)
  | Memory_grow id ->
    let* () = check_mem env.modul id in
    let* stack = Stack.pop env.modul [ i32 ] stack in
    let+ stack = Stack.push [ i32 ] stack in
    (env, stack)
  | Memory_size id ->
    let* () = check_mem env.modul id in
    let+ stack = Stack.push [ i32 ] stack in
    (env, stack)
  | Memory_copy (id1, id2) ->
    let* () = check_mem env.modul id1 in
    let* () = check_mem env.modul id2 in
    let+ stack = Stack.pop env.modul [ i32; i32; i32 ] stack in
    (env, stack)
  | Memory_fill id ->
    let* () = check_mem env.modul id in
    let+ stack = Stack.pop env.modul [ i32; i32; i32 ] stack in
    (env, stack)
  | Memory_init (memidx, dataidx) ->
    let* () = check_mem env.modul memidx in
    let* () = check_data env.modul dataidx in
    let+ stack = Stack.pop env.modul [ i32; i32; i32 ] stack in
    (env, stack)
  | Block (_, bt, expr) ->
    let+ stack = typecheck_expr env expr ~is_loop:false bt ~stack in
    (env, stack)
  | Loop (_, bt, expr) ->
    let+ stack = typecheck_expr env expr ~is_loop:true bt ~stack in
    (env, stack)
  | Call_indirect (tbl_id, bt) ->
    let* _tbl_type = Env.table_type_get tbl_id env.modul in
    let* () =
      if is_func_type _tbl_type then Ok ()
      else Error (`Type_mismatch "call_indirect")
    in
    let* stack = Stack.pop env.modul [ i32 ] stack in
    let+ stack = Stack.pop_push env.modul bt stack in
    (env, stack)
  | Call i ->
    let* pt, rt = Env.func_get i env.modul in
    let* stack = Stack.pop env.modul (List.rev_map typ_of_pt pt) stack in
    let+ stack = Stack.push (List.rev_map typ_of_val_type rt) stack in
    (env, stack)
  | Call_ref t ->
    let* _, stack = Stack.pop_ref stack in
    let* pt, rt = Env.type_get t env.modul in
    let* stack = Stack.pop env.modul (List.rev_map typ_of_pt pt) stack in
    let+ stack = Stack.push (List.rev_map typ_of_val_type rt) stack in
    (env, stack)
  | Return_call i ->
    let* pt, rt = Env.func_get i env.modul in
    let* b =
      Stack.equal env.modul
        (List.rev_map typ_of_val_type rt)
        (List.rev_map typ_of_val_type env.result_type)
    in
    if not b then Error (`Type_mismatch "return_call")
    else
      let+ _stack = Stack.pop env.modul (List.rev_map typ_of_pt pt) stack in
      (env, [ any ])
  | Return_call_indirect (tbl_id, Bt_raw (_, (pt, rt))) ->
    let* _tbl_type = Env.table_type_get tbl_id env.modul in
    let* () =
      if is_func_type _tbl_type then Ok ()
      else Error (`Type_mismatch "call_indirect")
    in
    let* b =
      Stack.equal env.modul
        (List.rev_map typ_of_val_type rt)
        (List.rev_map typ_of_val_type env.result_type)
    in
    if not b then Error (`Type_mismatch "return_call_indirect")
    else
      let* stack = Stack.pop env.modul [ i32 ] stack in
      let+ _stack = Stack.pop env.modul (List.rev_map typ_of_pt pt) stack in
      (env, [ any ])
  | Return_call_ref (Bt_raw (_, (pt, rt))) ->
    let* b =
      Stack.equal env.modul
        (List.rev_map typ_of_val_type rt)
        (List.rev_map typ_of_val_type env.result_type)
    in
    if not b then Error (`Type_mismatch "return_call_ref")
    else
      let* _, stack = Stack.pop_ref stack in
      let+ _stack = Stack.pop env.modul (List.rev_map typ_of_pt pt) stack in
      (env, [ any ])
  | Data_drop id ->
    let+ () = check_data env.modul id in
    (env, stack)
  | Table_init (ti, ei) ->
    let* _, table_typ = Env.table_type_get ti env.modul in
    let* _, elem_typ = Env.elem_type_get ei env.modul in
    let* b =
      Stack.match_heap_type env.modul ~expected:table_typ ~got:elem_typ
    in
    if not b then Error (`Type_mismatch "table_init")
    else
      let+ stack = Stack.pop env.modul [ i32; i32; i32 ] stack in
      (env, stack)
  | Table_copy (id1, id2) ->
    let* _null1, typ1 = Env.table_type_get id1 env.modul in
    let* _null2, typ2 = Env.table_type_get id2 env.modul in
    let* b =
      Stack.match_heap_type ~subtype:true env.modul ~expected:typ1 ~got:typ2
    in
    if not b then Error (`Type_mismatch "table_copy")
    else
      let+ stack = Stack.pop env.modul [ i32; i32; i32 ] stack in
      (env, stack)
  | Table_fill i ->
    let* t = Env.table_type_get i env.modul in
    let+ stack = Stack.pop env.modul [ i32; Ref_type t; i32 ] stack in
    (env, stack)
  | Table_grow i ->
    let* t = Env.table_type_get i env.modul in
    let* stack = Stack.pop env.modul [ i32; Ref_type t ] stack in
    let+ stack = Stack.push [ i32 ] stack in
    (env, stack)
  | Table_size i ->
    let* _null, _t = Env.table_type_get i env.modul in
    let+ stack = Stack.push [ i32 ] stack in
    (env, stack)
  | Ref_is_null ->
    let* _, stack = Stack.pop_ref stack in
    let+ stack = Stack.push [ i32 ] stack in
    (env, stack)
  | Ref_as_non_null ->
    let* t, stack = Stack.pop_ref stack in
    let* t = ref_type_as_non_null t in
    let+ stack = Stack.push [ t ] stack in
    (env, stack)
    (* TODO: The type can be Something/Any, and if its a Ref_type the heap_type
      can be a TypeUse or Extern_ht. The pushed type should account for that
      and restrict whatever the type is to non_null. *)
  | Ref_null rt ->
    let+ stack = Stack.push [ Ref_type (Null, rt) ] stack in
    (env, stack)
  | Elem_drop id ->
    let+ _elem_typ = Env.elem_type_get id env.modul in
    (env, stack)
  | Select t ->
    let* stack = Stack.pop env.modul [ i32 ] stack in
    begin match t with
    | None -> begin
      match stack with
      | Ref_type _ :: _tl -> Error (`Type_mismatch "select implicit")
      | Any :: _ -> Ok (env, [ Something; Any ])
      | hd :: Any :: _ -> Ok (env, hd :: [ Any ])
      | hd :: hd' :: tl ->
        let* b = Stack.match_types env.modul ~expected:hd ~got:hd' in
        if b then Ok (env, hd :: tl) else Error (`Type_mismatch "select")
      | _ -> Error (`Type_mismatch "select")
    end
    | Some t ->
      let t = List.map typ_of_val_type t in
      let* stack = Stack.pop env.modul t stack in
      let* stack = Stack.pop env.modul t stack in
      let+ stack = Stack.push t stack in
      (env, stack)
    end
  | Ref_func i ->
    if not @@ Hashtbl.mem env.refs i then Error `Undeclared_function_reference
    else begin
      match get_func_type_id env i with
      | None -> assert false
      | Some id ->
        let+ stack = Stack.push [ Ref_type (Text.No_null, TypeUse id) ] stack in
        (env, stack)
    end
  | Br i ->
    let* jt = Env.block_type_get i env in
    let+ _stack = Stack.pop env.modul jt stack in
    (env, [ any ])
  | Br_if i ->
    let* stack = Stack.pop env.modul [ i32 ] stack in
    let* jt = Env.block_type_get i env in
    let* stack = Stack.pop env.modul jt stack in
    let+ stack = Stack.push jt stack in
    (env, stack)
  | Br_table (branches, i) ->
    let* stack = Stack.pop env.modul [ i32 ] stack in
    let* default_jt = Env.block_type_get i env in
    let* _stack = Stack.pop env.modul default_jt stack in
    let+ () =
      array_iter
        (fun (i : indice) ->
          let* jt = Env.block_type_get i env in
          if not (List.length jt = List.length default_jt) then
            Error (`Type_mismatch "br_table")
          else
            let* _stack = Stack.pop env.modul jt stack in
            Ok () )
        branches
    in
    (env, [ any ])
  | Br_on_null i ->
    let* t, stack = Stack.pop_ref stack in
    let* jt = Env.block_type_get i env in
    let* _stack = Stack.pop env.modul jt stack in
    let* t = ref_type_as_non_null t in
    let+ stack = Stack.push [ t ] stack in
    (env, stack)
  | Br_on_non_null i ->
    let* _, stack = Stack.pop_ref stack in
    let* jt = Env.block_type_get i env in
    let+ _stack = Stack.pop env.modul jt stack in
    (env, stack)
  | Table_get i ->
    let* t = Env.table_type_get i env.modul in
    let* stack = Stack.pop env.modul [ i32 ] stack in
    let+ stack = Stack.push [ Ref_type t ] stack in
    (env, stack)
  | Table_set i ->
    let* t = Env.table_type_get i env.modul in
    let+ stack = Stack.pop env.modul [ Ref_type t; i32 ] stack in
    (env, stack)
  | (Extern_externalize | Extern_internalize) as i ->
    Log.err (fun m ->
      m "TODO: unimplemented instruction typecheking %a" (pp_instr ~short:false)
        i );
    assert false

and typecheck_expr env expr ~is_loop (block_type : block_type option)
  ~stack:previous_stack : stack Result.t =
  let pt, rt =
    Option.fold ~none:([], [])
      ~some:(fun (Bt_raw (_, (pt, rt)) : block_type) ->
        (List.rev_map typ_of_pt pt, List.rev_map typ_of_val_type rt) )
      block_type
  in
  let jump_type = if is_loop then pt else rt in
  let env = { env with blocks = jump_type :: env.blocks } in
  let* env, stack =
    list_fold_left
      (fun (env, stack) -> typecheck_instr env stack)
      (env, pt) expr.raw
  in
  let* b = Stack.equal env.modul rt stack in
  if not b then
    Error
      (`Type_mismatch
         (Fmt.str "expected a prefix of %a but stack has type %a" Stack.pp rt
            Stack.pp stack ) )
  else
    let* res = Stack.match_prefix env.modul ~prefix:pt ~stack:previous_stack in
    match res with
    | None ->
      Error
        (`Type_mismatch
           (Fmt.str "expected a prefix of %a but stack has type %a" Stack.pp pt
              Stack.pp previous_stack ) )
    | Some stack_to_push -> Stack.push rt stack_to_push

let typecheck_function (modul : Module.t) func refs =
  match func with
  | Origin.Imported _ -> Ok ()
  | Local (func : Func.t) ->
    let (Bt_raw (_, (params, result))) = func.type_f in
    let env =
      Env.make ~params ~modul ~locals:func.locals ~result_type:result ~refs
    in
    let* stack =
      typecheck_expr env func.body ~is_loop:false
        (Some (Bt_raw (None, ([], result))))
        ~stack:[]
    in
    let required = List.rev_map typ_of_val_type result in
    let* b = Stack.equal modul required stack in
    if not b then Error (`Type_mismatch "typecheck_function") else Ok ()

let typecheck_const_instr ?known_globals ~is_init (modul : Module.t) refs stack
  instr =
  match instr.Annotated.raw with
  | I32_const _ -> Stack.push [ i32 ] stack
  | I64_const _ -> Stack.push [ i64 ] stack
  | F32_const _ -> Stack.push [ f32 ] stack
  | F64_const _ -> Stack.push [ f64 ] stack
  | V128_const _ -> Stack.push [ v128 ] stack
  | Ref_null t -> Stack.push [ Ref_type (Null, t) ] stack
  | Ref_func i ->
    let* ity = Env.func_get i modul in
    let resty =
      match
        Array.find_index (fun (_, ty) -> func_type_eq ty ity) modul.types
      with
      | Some tyid -> TypeUse tyid
      | None -> Func_ht
    in
    Hashtbl.add refs i ();
    Stack.push [ Ref_type (No_null, resty) ] stack
  | Global_get i ->
    if
      Option.fold ~none:false ~some:(fun n -> i >= n) known_globals
      || i >= Array.length modul.global
    then Error (`Unknown_global (Text.Raw i))
    else
      let* mut, typ =
        match modul.global.(i) with
        | Origin.Local _ when is_init -> Error (`Unknown_global (Text.Raw i))
        | Local { typ; _ } | Imported { typ; _ } -> Ok typ
      in
      let* () =
        match mut with
        | Const -> Ok ()
        | Var -> Error `Constant_expression_required
      in
      Stack.push [ typ_of_val_type typ ] stack
  | I_binop (t, _op) ->
    let t = itype t in
    let* stack = Stack.pop modul [ t; t ] stack in
    Stack.push [ t ] stack
  | _ -> Error `Constant_expression_required

let typecheck_const_expr ?known_globals ?(is_init = false) (modul : Module.t)
  refs expr =
  list_fold_left
    (typecheck_const_instr ?known_globals ~is_init modul refs)
    [] expr.Annotated.raw

let typecheck_global (modul : Module.t) refs known_globals
  (global : (Global.t, Global.Type.t) Origin.t) =
  match global with
  | Imported _ -> Ok ()
  | Local { typ; init; _ } -> (
    let* real_type = typecheck_const_expr ~known_globals modul refs init in
    match real_type with
    | [ real_type ] ->
      let expected = typ_of_val_type @@ snd typ in
      let* b = typ_equal modul ~expected ~got:real_type in
      if b then Ok () else Error (`Type_mismatch "typecheck global 1")
    | _whatever -> Error (`Type_mismatch "typecheck_global 2") )

let typecheck_elem modul refs (elem : Elem.t) =
  let elem_null, elem_ty = elem.typ in
  let* () =
    list_iter
      (fun init ->
        let* real_type = typecheck_const_expr modul refs init in
        match real_type with
        | [ real_type ] ->
          let expected_type = Ref_type (elem_null, elem_ty) in
          let* b = typ_equal modul ~expected:expected_type ~got:real_type in
          if not b then
            Error
              (`Type_mismatch
                 (Fmt.str "expected %a got %a" pp_typ expected_type pp_typ
                    real_type ) )
          else Ok ()
        | _whatever -> Error (`Type_mismatch "typecheck elem 2") )
      elem.init
  in
  match elem.mode with
  | Passive | Declarative -> Ok ()
  | Active (None, _e) -> assert false
  | Active (Some tbl_i, e) -> (
    let* tbl_null, tbl_ty = Env.table_type_get tbl_i modul in
    if
      (elem.explicit_typ && Text.compare_nullable tbl_null elem_null > 0)
      || not (heap_type_eq tbl_ty elem_ty)
    then Error (`Type_mismatch "typecheck elem 3")
    else
      let* t = typecheck_const_expr modul refs e in
      match t with
      | [ Ref_type (_, t) ] ->
        if not @@ heap_type_eq t tbl_ty then
          Error (`Type_mismatch "typecheck_elem 4")
        else Ok ()
      | [ _t ] -> Ok ()
      | _whatever -> Error (`Type_mismatch "typecheck_elem 5") )

let typecheck_data modul refs (data : Data.t) =
  match data.mode with
  | Passive -> Ok ()
  | Active (n, e) -> (
    let* () = check_mem modul n in
    let* t = typecheck_const_expr modul refs e in
    match t with
    | [ _t ] -> Ok ()
    | _whatever -> Error (`Type_mismatch "typecheck_data") )

let typecheck_start { start; func; _ } =
  match start with
  | None -> Ok ()
  | Some idx -> (
    let* f =
      if idx >= Array.length func then Error (`Unknown_func (Text.Raw idx))
      else Ok func.(idx)
    in
    match f with
    | Local { type_f = Bt_raw (_, ([], [])); _ }
    | Imported { typ = Bt_raw (_, ([], [])); _ } ->
      Ok ()
    | _ -> Error `Start_function )

let validate_exports modul =
  let* () =
    array_iter
      (fun { Export.id; name = _ } ->
        let* _t = Env.func_get id modul in
        Ok () )
      modul.exports.func
  in
  let* () =
    array_iter
      (fun { Export.id; name = _ } ->
        let* _t = Env.table_type_get id modul in
        Ok () )
      modul.exports.table
  in
  let* () =
    array_iter
      (fun { Export.id; name = _ } ->
        let* _t = Env.global_get id modul in
        Ok () )
      modul.exports.global
  in
  let* () =
    array_iter
      (fun { Export.id; name = _ } ->
        let* _t = Env.tag_get id modul in
        Ok () )
      modul.exports.tag
  in
  array_iter
    (fun { id; Export.name = _ } ->
      let* () = check_mem modul id in
      Ok () )
    modul.exports.mem

let check_limit ?(table = false) { Text.min; max } =
  match max with
  | None ->
    if table && not (Int64.fits_in_u32 (Int64.of_int min)) then
      Error `Table_size
    else Ok ()
  | Some max ->
    if min > max then Error `Size_minimum_greater_than_maximum
    else if
      table
      && ( (not (Int64.fits_in_u32 (Int64.of_int min)))
         || not (Int64.fits_in_u32 (Int64.of_int max)) )
    then Error `Table_size
    else Ok ()

let validate_table_init modul refs id init ((nullable, _) as rt) =
  match init with
  | None -> begin
    match nullable with
    | Text.Null -> Ok ()
    | No_null ->
      let has_init_elem =
        Array.exists
          (fun e ->
            match e with
            | Elem.{ mode = Active (Some id', _); _ } when id = id' -> true
            | _ -> false )
          modul.elem
      in
      if has_init_elem then Ok ()
      else
        Error
          (`Type_mismatch
             (Fmt.str
                "Table type is %a but init was not provided and is not nullable"
                pp_ref_type rt ) )
  end
  | Some init ->
    let* res_tys = typecheck_const_expr ~is_init:true modul refs init in
    begin match res_tys with
    | [ Ref_type got ] ->
      let* res = Stack.match_ref_type ~subtype:true ~expected:rt ~got modul in
      if res then Ok ()
      else
        Error
          (`Type_mismatch
             (Fmt.str "Table type is %a but init expr is of type %a" pp_ref_type
                rt pp_typ_list res_tys ) )
    | _ ->
      Error
        (`Type_mismatch
           (Fmt.str "Table type is %a but init expr is of type %a" pp_ref_type
              rt pp_typ_list res_tys ) )
    end

let validate_table modul refs id t =
  match t with
  | Origin.Local Table.{ typ = limits, rt; init; _ } ->
    let* () = validate_table_init modul refs id init rt in
    check_limit ~table:true limits
  | Imported { typ = limits, _; _ } -> check_limit ~table:true limits

let validate_tables modul refs =
  array_iteri (validate_table modul refs) modul.table

let validate_mem modul =
  array_iter
    (function
      | Origin.Local (_, typ) | Imported { typ; _ } ->
        let* () =
          if typ.Text.min > 65536 then Error `Memory_size_too_large
          else
            match typ.max with
            | Some max when max > 65536 -> Error `Memory_size_too_large
            | Some _ | None -> Ok ()
        in
        check_limit typ )
    modul.mem

let modul (modul : Module.t) =
  Log.info (fun m -> m "typechecking ...");
  Log.bench_fn "typechecking time" @@ fun () ->
  let refs = Hashtbl.create 512 in
  let* () = array_iteri (typecheck_global modul refs) modul.global in
  let* () = array_iter (typecheck_elem modul refs) modul.elem in
  let* () = array_iter (typecheck_data modul refs) modul.data in
  let* () = typecheck_start modul in
  let* () = validate_exports modul in
  let* () = validate_tables modul refs in
  let* () = validate_mem modul in
  Array.iter
    (fun (export : Export.t) -> Hashtbl.add refs export.id ())
    modul.exports.func;
  array_iter (fun func -> typecheck_function modul func refs) modul.func
