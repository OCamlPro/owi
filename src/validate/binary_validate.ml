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
  else
    match modul.mem.(n) with
    | Local (_, I64 _) | Imported { typ = I64 _; _ } -> Ok true
    | Local (_, I32 _) | Imported { typ = I32 _; _ } -> Ok false

let check_data modul n =
  if n >= Array.length modul.data then Error (`Unknown_data (Text.Raw n))
  else Ok ()

let check_memarg ~is_i64 ({ align; offset } : memarg) max_align =
  let* () =
    if Int32.le max_align align then Error `Alignment_too_large else Ok ()
  in
  if (not is_i64) && Int64.lt_u 0xffff_ffffL offset then
    Error `Offset_out_of_range
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
    | ((Text.Null : Text.nullable), expected), (Text.No_null, got)
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

  let push t stack = Result.ok @@ t @ stack

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

let typecheck_i32_instr (env : Env.t) stack : Binary.i32_instr -> _ = function
  | Const _ ->
    let+ stack = Stack.push [ i32 ] stack in
    (env, stack)
  | Clz | Ctz | Popcnt | Eqz | Extend8_s | Extend16_s ->
    (* Unary operators + Test operators *)
    let* stack = Stack.pop env.modul [ i32 ] stack in
    let+ stack = Stack.push [ i32 ] stack in
    (env, stack)
  | Add | Sub | Mul | Div _ | Rem _ | And | Or | Xor | Shl | Shr _ | Rotl | Rotr
    ->
    (* Binary operators *)
    let* stack = Stack.pop env.modul [ i32; i32 ] stack in
    let+ stack = Stack.push [ i32 ] stack in
    (env, stack)
  | Eq | Ne | Lt _ | Gt _ | Le _ | Ge _ ->
    (* Relation operators *)
    let* stack = Stack.pop env.modul [ i32; i32 ] stack in
    let+ stack = Stack.push [ i32 ] stack in
    (env, stack)
  | Load8 (id, _, memarg) ->
    let* is_i64 = check_mem env.modul id in
    let* () = check_memarg ~is_i64 memarg 1l in
    let* stack = Stack.pop env.modul [ i32 ] stack in
    let+ stack = Stack.push [ i32 ] stack in
    (env, stack)
  | Load16 (id, _, memarg) ->
    let* is_i64 = check_mem env.modul id in
    let* () = check_memarg ~is_i64 memarg 2l in
    let* stack = Stack.pop env.modul [ i32 ] stack in
    let+ stack = Stack.push [ i32 ] stack in
    (env, stack)
  | Load (id, memarg) ->
    let* is_i64 = check_mem env.modul id in
    let* () = check_memarg ~is_i64 memarg 4l in
    let* stack = Stack.pop env.modul [ i32 ] stack in
    let+ stack = Stack.push [ i32 ] stack in
    (env, stack)
  | Store8 (id, memarg) ->
    let* is_i64 = check_mem env.modul id in
    let* () = check_memarg ~is_i64 memarg 1l in
    let+ stack = Stack.pop env.modul [ i32; i32 ] stack in
    (env, stack)
  | Store16 (id, memarg) ->
    let* is_i64 = check_mem env.modul id in
    let* () = check_memarg ~is_i64 memarg 2l in
    let+ stack = Stack.pop env.modul [ i32; i32 ] stack in
    (env, stack)
  | Store (id, memarg) ->
    let* is_i64 = check_mem env.modul id in
    let* () = check_memarg ~is_i64 memarg 4l in
    let+ stack = Stack.pop env.modul [ i32; i32 ] stack in
    (env, stack)
  | Reinterpret_f fnn | Trunc_f (fnn, _) | Trunc_sat_f (fnn, _) ->
    let* stack = Stack.pop env.modul [ ftype fnn ] stack in
    let+ stack = Stack.push [ i32 ] stack in
    (env, stack)
  | Wrap_i64 ->
    let* stack = Stack.pop env.modul [ i64 ] stack in
    let+ stack = Stack.push [ i32 ] stack in
    (env, stack)

let typecheck_i64_instr (env : Env.t) stack : Binary.i64_instr -> _ = function
  | Const _ ->
    let+ stack = Stack.push [ i64 ] stack in
    (env, stack)
  | Clz | Ctz | Popcnt | Extend8_s | Extend16_s | Extend32_s ->
    (* Unary operators *)
    let* stack = Stack.pop env.modul [ i64 ] stack in
    let+ stack = Stack.push [ i64 ] stack in
    (env, stack)
  | Eqz ->
    (* Test operators *)
    let* stack = Stack.pop env.modul [ i64 ] stack in
    let+ stack = Stack.push [ i32 ] stack in
    (env, stack)
  | Add | Sub | Mul | Div _ | Rem _ | And | Or | Xor | Shl | Shr _ | Rotl | Rotr
    ->
    (* Binary operators *)
    let* stack = Stack.pop env.modul [ i64; i64 ] stack in
    let+ stack = Stack.push [ i64 ] stack in
    (env, stack)
  | Eq | Ne | Lt _ | Gt _ | Le _ | Ge _ ->
    (* Relation operators *)
    let* stack = Stack.pop env.modul [ i64; i64 ] stack in
    let+ stack = Stack.push [ i32 ] stack in
    (env, stack)
  | Load8 (id, _, memarg) ->
    let* is_i64 = check_mem env.modul id in
    let* () = check_memarg ~is_i64 memarg 1l in
    let* stack = Stack.pop env.modul [ i32 ] stack in
    let+ stack = Stack.push [ i64 ] stack in
    (env, stack)
  | Load16 (id, _, memarg) ->
    let* is_i64 = check_mem env.modul id in
    let* () = check_memarg ~is_i64 memarg 2l in
    let* stack = Stack.pop env.modul [ i32 ] stack in
    let+ stack = Stack.push [ i64 ] stack in
    (env, stack)
  | Load32 (id, _, memarg) ->
    let* is_i64 = check_mem env.modul id in
    let* () = check_memarg ~is_i64 memarg 4l in
    let* stack = Stack.pop env.modul [ i32 ] stack in
    let+ stack = Stack.push [ i64 ] stack in
    (env, stack)
  | Load (id, memarg) ->
    let* is_i64 = check_mem env.modul id in
    let* () = check_memarg ~is_i64 memarg 8l in
    let* stack = Stack.pop env.modul [ i32 ] stack in
    let+ stack = Stack.push [ i64 ] stack in
    (env, stack)
  | Store8 (id, memarg) ->
    let* is_i64 = check_mem env.modul id in
    let* () = check_memarg ~is_i64 memarg 1l in
    let+ stack = Stack.pop env.modul [ i64; i32 ] stack in
    (env, stack)
  | Store16 (id, memarg) ->
    let* is_i64 = check_mem env.modul id in
    let* () = check_memarg ~is_i64 memarg 2l in
    let+ stack = Stack.pop env.modul [ i64; i32 ] stack in
    (env, stack)
  | Store32 (id, memarg) ->
    let* is_i64 = check_mem env.modul id in
    let* () = check_memarg ~is_i64 memarg 4l in
    let+ stack = Stack.pop env.modul [ i64; i32 ] stack in
    (env, stack)
  | Store (id, memarg) ->
    let* is_i64 = check_mem env.modul id in
    let* () = check_memarg ~is_i64 memarg 8l in
    let+ stack = Stack.pop env.modul [ i64; i32 ] stack in
    (env, stack)
  | Reinterpret_f fnn | Trunc_f (fnn, _) | Trunc_sat_f (fnn, _) ->
    let* stack = Stack.pop env.modul [ ftype fnn ] stack in
    let+ stack = Stack.push [ i64 ] stack in
    (env, stack)
  | Extend_i32 _ ->
    let* stack = Stack.pop env.modul [ i32 ] stack in
    let+ stack = Stack.push [ i64 ] stack in
    (env, stack)

let typecheck_f32_instr (env : Env.t) stack : Binary.f32_instr -> _ = function
  | Const _ ->
    let+ stack = Stack.push [ f32 ] stack in
    (env, stack)
  | Abs | Neg | Sqrt | Ceil | Floor | Trunc | Nearest ->
    (* Unary operators *)
    let* stack = Stack.pop env.modul [ f32 ] stack in
    let+ stack = Stack.push [ f32 ] stack in
    (env, stack)
  | Add | Sub | Mul | Div | Min | Max | Copysign ->
    (* Binary operators *)
    let* stack = Stack.pop env.modul [ f32; f32 ] stack in
    let+ stack = Stack.push [ f32 ] stack in
    (env, stack)
  | Eq | Ne | Lt | Gt | Le | Ge ->
    (* Relation operators *)
    let* stack = Stack.pop env.modul [ f32; f32 ] stack in
    let+ stack = Stack.push [ i32 ] stack in
    (env, stack)
  | Load (id, memarg) ->
    let* is_i64 = check_mem env.modul id in
    let* () = check_memarg ~is_i64 memarg 4l in
    let* stack = Stack.pop env.modul [ i32 ] stack in
    let+ stack = Stack.push [ f32 ] stack in
    (env, stack)
  | Store (id, memarg) ->
    let* is_i64 = check_mem env.modul id in
    let* () = check_memarg ~is_i64 memarg 4l in
    let+ stack = Stack.pop env.modul [ f32; i32 ] stack in
    (env, stack)
  | Reinterpret_i inn | Convert_i (inn, _) ->
    let* stack = Stack.pop env.modul [ itype inn ] stack in
    let+ stack = Stack.push [ f32 ] stack in
    (env, stack)
  | Demote_f64 ->
    let* stack = Stack.pop env.modul [ f64 ] stack in
    let+ stack = Stack.push [ f32 ] stack in
    (env, stack)

let typecheck_f64_instr (env : Env.t) stack : Binary.f64_instr -> _ = function
  | Const _ ->
    let+ stack = Stack.push [ f64 ] stack in
    (env, stack)
  | Abs | Neg | Sqrt | Ceil | Floor | Trunc | Nearest ->
    (* Unary operators *)
    let* stack = Stack.pop env.modul [ f64 ] stack in
    let+ stack = Stack.push [ f64 ] stack in
    (env, stack)
  | Add | Sub | Mul | Div | Min | Max | Copysign ->
    (* Binary operators *)
    let* stack = Stack.pop env.modul [ f64; f64 ] stack in
    let+ stack = Stack.push [ f64 ] stack in
    (env, stack)
  | Eq | Ne | Lt | Gt | Le | Ge ->
    (* Relation operators *)
    let* stack = Stack.pop env.modul [ f64; f64 ] stack in
    let+ stack = Stack.push [ i32 ] stack in
    (env, stack)
  | Load (id, memarg) ->
    let* is_i64 = check_mem env.modul id in
    let* () = check_memarg ~is_i64 memarg 8l in
    let* stack = Stack.pop env.modul [ i32 ] stack in
    let+ stack = Stack.push [ f64 ] stack in
    (env, stack)
  | Store (id, memarg) ->
    let* is_i64 = check_mem env.modul id in
    let* () = check_memarg ~is_i64 memarg 8l in
    let+ stack = Stack.pop env.modul [ f64; i32 ] stack in
    (env, stack)
  | Reinterpret_i inn ->
    let* stack = Stack.pop env.modul [ itype inn ] stack in
    let+ stack = Stack.push [ f64 ] stack in
    (env, stack)
  | Convert_i (inn, _) ->
    let* stack = Stack.pop env.modul [ itype inn ] stack in
    let+ stack = Stack.push [ f64 ] stack in
    (env, stack)
  | Promote_f32 ->
    let* stack = Stack.pop env.modul [ f32 ] stack in
    let+ stack = Stack.push [ f64 ] stack in
    (env, stack)

let typecheck_v128_instr env stack : Text.v128_instr -> _ = function
  | Const _ ->
    let+ stack = Stack.push [ v128 ] stack in
    (env, stack)

let typecheck_i8x16_instr (env : Env.t) stack : Text.i8x16_instr -> _ = function
  | Add | Sub ->
    let* stack = Stack.pop env.modul [ v128; v128 ] stack in
    let+ stack = Stack.push [ v128 ] stack in
    (env, stack)

let typecheck_i16x8_instr (env : Env.t) stack : Text.i16x8_instr -> _ = function
  | Add | Sub ->
    let* stack = Stack.pop env.modul [ v128; v128 ] stack in
    let+ stack = Stack.push [ v128 ] stack in
    (env, stack)

let typecheck_i32x4_instr (env : Env.t) stack : Text.i32x4_instr -> _ = function
  | Add | Sub ->
    let* stack = Stack.pop env.modul [ v128; v128 ] stack in
    let+ stack = Stack.push [ v128 ] stack in
    (env, stack)

let typecheck_i64x2_instr (env : Env.t) stack : Text.i64x2_instr -> _ = function
  | Add | Sub ->
    let* stack = Stack.pop env.modul [ v128; v128 ] stack in
    let+ stack = Stack.push [ v128 ] stack in
    (env, stack)

let typecheck_ref_instr (env : Env.t) stack = function
  | Is_null ->
    let* _, stack = Stack.pop_ref stack in
    let+ stack = Stack.push [ i32 ] stack in
    (env, stack)
  | As_non_null ->
    let* t, stack = Stack.pop_ref stack in
    let* t = ref_type_as_non_null t in
    let+ stack = Stack.push [ t ] stack in
    (env, stack)
    (* TODO: The type can be Something/Any, and if its a Ref_type the heap_type
      can be a TypeUse or Extern_ht. The pushed type should account for that
      and restrict whatever the type is to non_null. *)
  | Null rt ->
    let+ stack = Stack.push [ Ref_type (Null, rt) ] stack in
    (env, stack)
  | Func i ->
    if not @@ Hashtbl.mem env.refs i then Error `Undeclared_function_reference
    else
      begin match get_func_type_id env i with
      | None -> assert false
      | Some id ->
        let+ stack = Stack.push [ Ref_type (Text.No_null, TypeUse id) ] stack in
        (env, stack)
      end

let typecheck_local_instr env stack : Binary.local_instr -> _ = function
  | Get i ->
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
  | Set i ->
    let* { typ = t; _ } = Env.local_get i env in
    let* env = Env.local_set_init i env in
    let+ stack = Stack.pop env.modul [ t ] stack in
    (env, stack)
  | Tee i ->
    let* { typ = t; _ } = Env.local_get i env in
    let* stack = Stack.pop env.modul [ t ] stack in
    let+ stack = Stack.push [ t ] stack in
    (env, stack)

let typecheck_global_instr (env : Env.t) stack : Binary.global_instr -> _ =
  function
  | Get i ->
    let* _mut, t = Env.global_get i env.modul in
    let t = typ_of_val_type t in
    let+ stack = Stack.push [ t ] stack in
    (env, stack)
  | Set i ->
    let* mut, t = Env.global_get i env.modul in
    let* () =
      match mut with Var -> Ok () | Const -> Error `Global_is_immutable
    in
    let t = typ_of_val_type t in
    let+ stack = Stack.pop env.modul [ t ] stack in
    (env, stack)

let typecheck_table_instr (env : Env.t) stack = function
  | Get i ->
    let* t = Env.table_type_get i env.modul in
    let* stack = Stack.pop env.modul [ i32 ] stack in
    let+ stack = Stack.push [ Ref_type t ] stack in
    (env, stack)
  | Set i ->
    let* t = Env.table_type_get i env.modul in
    let+ stack = Stack.pop env.modul [ Ref_type t; i32 ] stack in
    (env, stack)
  | Init (ti, ei) ->
    let* _, table_typ = Env.table_type_get ti env.modul in
    let* _, elem_typ = Env.elem_type_get ei env.modul in
    let* b =
      Stack.match_heap_type env.modul ~expected:table_typ ~got:elem_typ
    in
    if not b then Error (`Type_mismatch "table_init")
    else
      let+ stack = Stack.pop env.modul [ i32; i32; i32 ] stack in
      (env, stack)
  | Copy (id1, id2) ->
    let* _null1, typ1 = Env.table_type_get id1 env.modul in
    let* _null2, typ2 = Env.table_type_get id2 env.modul in
    let* b =
      Stack.match_heap_type ~subtype:true env.modul ~expected:typ1 ~got:typ2
    in
    if not b then Error (`Type_mismatch "table_copy")
    else
      let+ stack = Stack.pop env.modul [ i32; i32; i32 ] stack in
      (env, stack)
  | Fill i ->
    let* t = Env.table_type_get i env.modul in
    let+ stack = Stack.pop env.modul [ i32; Ref_type t; i32 ] stack in
    (env, stack)
  | Grow i ->
    let* t = Env.table_type_get i env.modul in
    let* stack = Stack.pop env.modul [ i32; Ref_type t ] stack in
    let+ stack = Stack.push [ i32 ] stack in
    (env, stack)
  | Size i ->
    let* _null, _t = Env.table_type_get i env.modul in
    let+ stack = Stack.push [ i32 ] stack in
    (env, stack)

let typecheck_elem_instr (env : Env.t) stack : Binary.elem_instr -> _ = function
  | Drop id ->
    let+ _elem_typ = Env.elem_type_get id env.modul in
    (env, stack)

let typecheck_memory_instr (env : Env.t) stack = function
  | Grow id ->
    let* (_ : bool) = check_mem env.modul id in
    let* stack = Stack.pop env.modul [ i32 ] stack in
    let+ stack = Stack.push [ i32 ] stack in
    (env, stack)
  | Size id ->
    let* (_ : bool) = check_mem env.modul id in
    let+ stack = Stack.push [ i32 ] stack in
    (env, stack)
  | Copy (id1, id2) ->
    let* (_ : bool) = check_mem env.modul id1 in
    let* (_ : bool) = check_mem env.modul id2 in
    let+ stack = Stack.pop env.modul [ i32; i32; i32 ] stack in
    (env, stack)
  | Fill id ->
    let* (_ : bool) = check_mem env.modul id in
    let+ stack = Stack.pop env.modul [ i32; i32; i32 ] stack in
    (env, stack)
  | Init (memidx, dataidx) ->
    let* (_ : bool) = check_mem env.modul memidx in
    let* () = check_data env.modul dataidx in
    let+ stack = Stack.pop env.modul [ i32; i32; i32 ] stack in
    (env, stack)

let typecheck_data_instr (env : Env.t) stack : Binary.data_instr -> _ = function
  | Drop id ->
    let+ () = check_data env.modul id in
    (env, stack)

let rec typecheck_instr (env : Env.t) (stack : stack) (instr : instr Annotated.t)
  : (Env.t * stack) Result.t =
  Log.debug (fun m -> m "stack             : %a" Stack.pp stack);
  Log.debug (fun m ->
    m "typechecking instr: %a" (Binary.pp_instr ~short:true) instr.raw );
  match instr.raw with
  | I32 i -> typecheck_i32_instr env stack i
  | I64 i -> typecheck_i64_instr env stack i
  | F32 i -> typecheck_f32_instr env stack i
  | F64 i -> typecheck_f64_instr env stack i
  | V128 i -> typecheck_v128_instr env stack i
  | I8x16 i -> typecheck_i8x16_instr env stack i
  | I16x8 i -> typecheck_i16x8_instr env stack i
  | I32x4 i -> typecheck_i32x4_instr env stack i
  | I64x2 i -> typecheck_i64x2_instr env stack i
  | Ref i -> typecheck_ref_instr env stack i
  | Local i -> typecheck_local_instr env stack i
  | Global i -> typecheck_global_instr env stack i
  | Table i -> typecheck_table_instr env stack i
  | Elem i -> typecheck_elem_instr env stack i
  | Memory i -> typecheck_memory_instr env stack i
  | Data i -> typecheck_data_instr env stack i
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
  | If_else (_id, block_type, e1, e2) ->
    let* stack = Stack.pop env.modul [ i32 ] stack in
    let* stack_e1 = typecheck_expr env e1 ~is_loop:false block_type ~stack in
    let+ _stack_e2 = typecheck_expr env e2 ~is_loop:false block_type ~stack in
    (env, stack_e1)
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
  | Select t ->
    let* stack = Stack.pop env.modul [ i32 ] stack in
    begin match t with
    | None ->
      begin match stack with
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
  | I32 (Const _) -> Stack.push [ i32 ] stack
  | I64 (Const _) -> Stack.push [ i64 ] stack
  | F32 (Const _) -> Stack.push [ f32 ] stack
  | F64 (Const _) -> Stack.push [ f64 ] stack
  | V128 (Const _) -> Stack.push [ v128 ] stack
  | Ref (Null t) -> Stack.push [ Ref_type (Null, t) ] stack
  | Ref (Func i) ->
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
  | Global (Get i) ->
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
  | I32
      ( Add | Mul | Sub | Div _ | Rem _ | And | Or | Xor | Shl | Shr _ | Rotl
      | Rotr ) ->
    let* stack = Stack.pop modul [ i32; i32 ] stack in
    Stack.push [ i32 ] stack
  | I64
      ( Add | Mul | Sub | Div _ | Rem _ | And | Or | Xor | Shl | Shr _ | Rotl
      | Rotr ) ->
    let* stack = Stack.pop modul [ i64; i64 ] stack in
    Stack.push [ i64 ] stack
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
    let* _ = check_mem modul n in
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
      let* _ = check_mem modul id in
      Ok () )
    modul.exports.mem

let validate_table_limits : Binary.Table.Type.limits -> unit Result.t = function
  | I32 { min; max = Some max } when Int32.lt_u max min ->
    Error `Size_minimum_greater_than_maximum
  | I64 { min; max = Some max } when Int64.lt_u max min ->
    Error `Size_minimum_greater_than_maximum
  | _ -> Ok ()

let validate_table_init modul refs id init ((nullable, _) as rt) =
  match init with
  | None ->
    begin match nullable with
    | (Null : Text.nullable) -> Ok ()
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
    validate_table_limits limits
  | Imported { typ = limits, _; _ } -> validate_table_limits limits

let validate_tables modul refs =
  array_iteri (validate_table modul refs) modul.table

let validate_memory_limit : Binary.Mem.Type.limits -> unit Result.t = function
  | I32 { min; max } ->
    begin match max with
    | None ->
      if Int32.lt_u 0x1_0000l min then Error `Memory_size_too_large else Ok ()
    | Some max ->
      if Int32.lt_u max min then Error `Size_minimum_greater_than_maximum
      else if Int32.lt_u 0x1_0000l min || Int32.lt_u 0x1_0000l max then
        Error `Memory_size_too_large
      else Ok ()
    end
  | I64 { min; max } ->
    begin match max with
    | None ->
      if min > 0x1_0000_0000_0000 then Error `Memory_size_too_large else Ok ()
    | Some max ->
      if min > max then Error `Size_minimum_greater_than_maximum
      else if min > 0x1_0000_0000_0000 || max > 0x1_0000_0000_0000 then
        Error `Memory_size_too_large
      else Ok ()
    end

let validate_mem modul =
  array_iter
    (function
      | Origin.Local (_, typ) | Imported { typ; _ } -> validate_memory_limit typ )
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
