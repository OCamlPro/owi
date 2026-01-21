(* SPDX-License-Identifier: AGPL-3.0-or-later *)

(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Binary
open Module
open Syntax
open Fmt

type typ =
  | Num_type of Text.num_type
  | Ref_type of Text.heap_type
  | Any
  | Something

let typ_equal t1 t2 =
  match (t1, t2) with
  | Num_type t1, Num_type t2 -> Text.num_type_eq t1 t2
  | Ref_type t1, Ref_type t2 -> Text.heap_type_eq t1 t2
  | Any, _ | _, Any -> true
  | Something, _ | _, Something -> true
  | _, _ -> false

let sp fmt () = Fmt.string fmt " "

let pp_typ fmt = function
  | Num_type t -> Text.pp_num_type fmt t
  | Ref_type t -> Text.pp_heap_type fmt t
  | Any -> string fmt "any"
  | Something -> string fmt "something"

let pp_typ_list fmt l = list ~sep:sp pp_typ fmt l

let typ_of_val_type = function
  | Text.Ref_type (_null, t) -> Ref_type t
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

let check_align memarg_align align =
  if Int32.ge memarg_align align then Error `Alignment_too_large else Ok ()

module Env = struct
  type t =
    { locals : typ Index.Map.t
    ; result_type : Text.result_type
    ; blocks : typ list list
    ; modul : Binary.Module.t
    ; refs : (int, unit) Hashtbl.t
    }

  let local_get i env =
    match Index.Map.find_opt i env.locals with
    | None -> Error (`Unknown_local (Text.Raw i))
    | Some v -> Ok v

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

  let table_type_get i (modul : Binary.Module.t) =
    if i >= Array.length modul.table then Error (`Unknown_table (Text.Raw i))
    else
      match modul.table.(i) with
      | Origin.Local (_, (_, t)) | Imported { typ = _, t; _ } -> Ok t

  let elem_type_get i modul =
    if i >= Array.length modul.elem then Error (`Unknown_elem (Text.Raw i))
    else match modul.elem.(i) with value -> Ok value.typ

  let make ~params ~locals ~modul ~result_type ~refs =
    let l = List.mapi (fun i v -> (i, v)) (params @ locals) in
    let locals =
      List.fold_left
        (fun locals (i, (_, typ)) ->
          let typ = typ_of_val_type typ in
          Index.Map.add i typ locals )
        Index.Map.empty l
    in
    { locals; modul; result_type; blocks = []; refs }
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

module Stack : sig
  type t = typ list

  val drop : t -> t Result.t

  val pop : t -> t -> t Result.t

  val push : t -> t -> t Result.t

  val pop_push : block_type -> t -> t Result.t

  val pop_ref : t -> t Result.t

  val equal : t -> t -> bool

  val match_ref_type : Text.heap_type -> Text.heap_type -> bool

  val match_types : typ -> typ -> bool

  val pp : t Fmt.t

  val match_prefix : prefix:t -> stack:t -> t option
end = struct
  type t = typ list

  let pp fmt (s : stack) = pf fmt "[%a]" pp_typ_list s

  let match_num_type (required : Text.num_type) (got : Text.num_type) =
    match (required, got) with
    | I32, I32 -> true
    | I64, I64 -> true
    | F32, F32 -> true
    | F64, F64 -> true
    | V128, V128 -> true
    | _, _ -> false

  let match_ref_type required got =
    match (required, got) with
    | Text.Func_ht, Text.Func_ht -> true
    | Extern_ht, Extern_ht -> true
    | Func_ht, Extern_ht -> false
    | Extern_ht, Func_ht -> false

  let match_types required got =
    match (required, got) with
    | Something, _ | _, Something -> true
    | Any, _ | _, Any -> true
    | Num_type required, Num_type got -> match_num_type required got
    | Ref_type required, Ref_type got -> match_ref_type required got
    | Num_type _, Ref_type _ | Ref_type _, Num_type _ -> false

  let rec equal s s' =
    match (s, s') with
    | [], s | s, [] -> List.for_all (function Any -> true | _ -> false) s
    | Any :: tl, Any :: tl' -> equal tl s' || equal s tl'
    | Any :: tl, hd :: tl' | hd :: tl', Any :: tl ->
      equal tl (hd :: tl') || equal (Any :: tl) tl'
    | hd :: tl, hd' :: tl' -> match_types hd hd' && equal tl tl'

  let ( ||| ) l r = match (l, r) with None, v | v, None -> v | _l, r -> r

  let rec match_prefix ~prefix ~stack =
    match (prefix, stack) with
    | [], stack -> Some stack
    | _hd :: _tl, [] -> None
    | _hd :: tl, Any :: tl' ->
      match_prefix ~prefix ~stack:tl' ||| match_prefix ~prefix:tl ~stack
    | hd :: tl, hd' :: tl' ->
      if match_types hd hd' then match_prefix ~prefix:tl ~stack:tl' else None

  let pop required stack =
    match match_prefix ~prefix:required ~stack with
    | None ->
      let msg = Fmt.str "expected %a but stack is %a" pp required pp stack in
      Error (`Type_mismatch msg)
    | Some stack -> Ok stack

  let pop_ref = function
    | (Something | Ref_type _) :: tl -> Ok tl
    | Any :: _ as stack -> Ok stack
    | _ -> Error (`Type_mismatch "pop_ref")

  let drop stack =
    match stack with
    | [] -> Error (`Type_mismatch "drop")
    | Any :: _ -> Ok [ Any ]
    | _ :: tl -> Ok tl

  let push t stack = ok @@ t @ stack

  let pop_push (Bt_raw (_, (pt, rt)) : block_type) stack =
    let pt, rt = (List.rev_map typ_of_pt pt, List.rev_map typ_of_val_type rt) in
    let* stack = pop pt stack in
    push rt stack
end

let rec typecheck_instr (env : Env.t) (stack : stack) (instr : instr Annotated.t)
  : stack Result.t =
  match instr.raw with
  | Nop -> Ok stack
  | Drop -> Stack.drop stack
  | Return ->
    let+ _stack =
      Stack.pop (List.rev_map typ_of_val_type env.result_type) stack
    in
    [ any ]
  | Unreachable -> Ok [ any ]
  | I32_const _ -> Stack.push [ i32 ] stack
  | I64_const _ -> Stack.push [ i64 ] stack
  | F32_const _ -> Stack.push [ f32 ] stack
  | F64_const _ -> Stack.push [ f64 ] stack
  | V128_const _ -> Stack.push [ v128 ] stack
  | I_unop (s, _op) ->
    let t = itype s in
    let* stack = Stack.pop [ t ] stack in
    Stack.push [ t ] stack
  | I_binop (s, _op) ->
    let t = itype s in
    let* stack = Stack.pop [ t; t ] stack in
    Stack.push [ t ] stack
  | F_unop (s, _op) ->
    let t = ftype s in
    let* stack = Stack.pop [ t ] stack in
    Stack.push [ t ] stack
  | F_binop (s, _op) ->
    let t = ftype s in
    let* stack = Stack.pop [ t; t ] stack in
    Stack.push [ t ] stack
  | V_ibinop (_shape, _op) ->
    let* stack = Stack.pop [ v128; v128 ] stack in
    Stack.push [ v128 ] stack
  | I_testop (nn, _) ->
    let* stack = Stack.pop [ itype nn ] stack in
    Stack.push [ i32 ] stack
  | I_relop (nn, _) ->
    let t = itype nn in
    let* stack = Stack.pop [ t; t ] stack in
    Stack.push [ i32 ] stack
  | F_relop (nn, _) ->
    let t = ftype nn in
    let* stack = Stack.pop [ t; t ] stack in
    Stack.push [ i32 ] stack
  | Local_get i ->
    let* t = Env.local_get i env in
    Stack.push [ t ] stack
  | Local_set i ->
    let* t = Env.local_get i env in
    Stack.pop [ t ] stack
  | Local_tee i ->
    let* t = Env.local_get i env in
    let* stack = Stack.pop [ t ] stack in
    Stack.push [ t ] stack
  | Global_get i ->
    let* _mut, t = Env.global_get i env.modul in
    let t = typ_of_val_type t in
    Stack.push [ t ] stack
  | Global_set i ->
    let* mut, t = Env.global_get i env.modul in
    let* () =
      match mut with Var -> Ok () | Const -> Error `Global_is_immutable
    in
    let t = typ_of_val_type t in
    Stack.pop [ t ] stack
  | If_else (_id, block_type, e1, e2) ->
    let* stack = Stack.pop [ i32 ] stack in
    let* stack_e1 = typecheck_expr env e1 ~is_loop:false block_type ~stack in
    let+ _stack_e2 = typecheck_expr env e2 ~is_loop:false block_type ~stack in
    stack_e1
  | I_load8 (id, nn, _, memarg) ->
    let* () = check_mem env.modul id in
    let* () = check_align memarg.align 1l in
    let* stack = Stack.pop [ i32 ] stack in
    Stack.push [ itype nn ] stack
  | I_load16 (id, nn, _, memarg) ->
    let* () = check_mem env.modul id in
    let* () = check_align memarg.align 2l in
    let* stack = Stack.pop [ i32 ] stack in
    Stack.push [ itype nn ] stack
  | I_load (id, nn, memarg) ->
    let* () = check_mem env.modul id in
    let max_allowed = match nn with S32 -> 4l | S64 -> 8l in
    let* () = check_align memarg.align max_allowed in
    let* stack = Stack.pop [ i32 ] stack in
    Stack.push [ itype nn ] stack
  | I64_load32 (id, _, memarg) ->
    let* () = check_mem env.modul id in
    let* () = check_align memarg.align 4l in
    let* stack = Stack.pop [ i32 ] stack in
    Stack.push [ i64 ] stack
  | I_store8 (id, nn, memarg) ->
    let* () = check_mem env.modul id in
    let* () = check_align memarg.align 1l in
    Stack.pop [ itype nn; i32 ] stack
  | I_store16 (id, nn, memarg) ->
    let* () = check_mem env.modul id in
    let* () = check_align memarg.align 2l in
    Stack.pop [ itype nn; i32 ] stack
  | I_store (id, nn, memarg) ->
    let* () = check_mem env.modul id in
    let max_allowed = match nn with S32 -> 4l | S64 -> 8l in
    let* () = check_align memarg.align max_allowed in
    Stack.pop [ itype nn; i32 ] stack
  | I64_store32 (id, memarg) ->
    let* () = check_mem env.modul id in
    let* () = check_align memarg.align 4l in
    Stack.pop [ i64; i32 ] stack
  | F_load (id, nn, memarg) ->
    let* () = check_mem env.modul id in
    let max_allowed = match nn with S32 -> 4l | S64 -> 8l in
    let* () = check_align memarg.align max_allowed in
    let* stack = Stack.pop [ i32 ] stack in
    Stack.push [ ftype nn ] stack
  | F_store (id, nn, memarg) ->
    let* () = check_mem env.modul id in
    let max_allowed = match nn with S32 -> 4l | S64 -> 8l in
    let* () = check_align memarg.align max_allowed in
    Stack.pop [ ftype nn; i32 ] stack
  | I_reinterpret_f (inn, fnn) ->
    let* stack = Stack.pop [ ftype fnn ] stack in
    Stack.push [ itype inn ] stack
  | F_reinterpret_i (fnn, inn) ->
    let* stack = Stack.pop [ itype inn ] stack in
    Stack.push [ ftype fnn ] stack
  | F32_demote_f64 ->
    let* stack = Stack.pop [ f64 ] stack in
    Stack.push [ f32 ] stack
  | F64_promote_f32 ->
    let* stack = Stack.pop [ f32 ] stack in
    Stack.push [ f64 ] stack
  | F_convert_i (fnn, inn, _) ->
    let* stack = Stack.pop [ itype inn ] stack in
    Stack.push [ ftype fnn ] stack
  | I_trunc_f (inn, fnn, _) | I_trunc_sat_f (inn, fnn, _) ->
    let* stack = Stack.pop [ ftype fnn ] stack in
    Stack.push [ itype inn ] stack
  | I32_wrap_i64 ->
    let* stack = Stack.pop [ i64 ] stack in
    Stack.push [ i32 ] stack
  | I_extend8_s nn | I_extend16_s nn ->
    let t = itype nn in
    let* stack = Stack.pop [ t ] stack in
    Stack.push [ t ] stack
  | I64_extend32_s ->
    let* stack = Stack.pop [ i64 ] stack in
    Stack.push [ i64 ] stack
  | I64_extend_i32 _ ->
    let* stack = Stack.pop [ i32 ] stack in
    Stack.push [ i64 ] stack
  | Memory_grow id ->
    let* () = check_mem env.modul id in
    let* stack = Stack.pop [ i32 ] stack in
    Stack.push [ i32 ] stack
  | Memory_size id ->
    let* () = check_mem env.modul id in
    Stack.push [ i32 ] stack
  | Memory_copy (id1, id2) ->
    let* () = check_mem env.modul id1 in
    let* () = check_mem env.modul id2 in
    Stack.pop [ i32; i32; i32 ] stack
  | Memory_fill id ->
    let* () = check_mem env.modul id in
    Stack.pop [ i32; i32; i32 ] stack
  | Memory_init (memidx, dataidx) ->
    let* () = check_mem env.modul memidx in
    let* () = check_data env.modul dataidx in
    Stack.pop [ i32; i32; i32 ] stack
  | Block (_, bt, expr) -> typecheck_expr env expr ~is_loop:false bt ~stack
  | Loop (_, bt, expr) -> typecheck_expr env expr ~is_loop:true bt ~stack
  | Call_indirect (tbl_id, bt) ->
    let* _tbl_type = Env.table_type_get tbl_id env.modul in
    let* stack = Stack.pop [ i32 ] stack in
    Stack.pop_push bt stack
  | Call i ->
    let* pt, rt = Env.func_get i env.modul in
    let* stack = Stack.pop (List.rev_map typ_of_pt pt) stack in
    Stack.push (List.rev_map typ_of_val_type rt) stack
  | Call_ref _t ->
    let+ stack = Stack.pop_ref stack in
    (* TODO:
       let bt = Env.type_get t env in
         Stack.pop_push (Some bt) stack
    *)
    stack
  | Return_call i ->
    let* pt, rt = Env.func_get i env.modul in
    if
      not
        (Stack.equal
           (List.rev_map typ_of_val_type rt)
           (List.rev_map typ_of_val_type env.result_type) )
    then Error (`Type_mismatch "return_call")
    else
      let+ _stack = Stack.pop (List.rev_map typ_of_pt pt) stack in
      [ any ]
  | Return_call_indirect (tbl_id, Bt_raw (_, (pt, rt))) ->
    let* _tbl_type = Env.table_type_get tbl_id env.modul in
    if
      not
        (Stack.equal
           (List.rev_map typ_of_val_type rt)
           (List.rev_map typ_of_val_type env.result_type) )
    then Error (`Type_mismatch "return_call_indirect")
    else
      let* stack = Stack.pop [ i32 ] stack in
      let+ _stack = Stack.pop (List.rev_map typ_of_pt pt) stack in
      [ any ]
  | Return_call_ref (Bt_raw (_, (pt, rt))) ->
    if
      not
        (Stack.equal
           (List.rev_map typ_of_val_type rt)
           (List.rev_map typ_of_val_type env.result_type) )
    then Error (`Type_mismatch "return_call_ref")
    else
      let* stack = Stack.pop_ref stack in
      let+ _stack = Stack.pop (List.rev_map typ_of_pt pt) stack in
      [ any ]
  | Data_drop id ->
    let* () = check_data env.modul id in
    Ok stack
  | Table_init (ti, ei) ->
    let* table_typ = Env.table_type_get ti env.modul in
    let* elem_typ = Env.elem_type_get ei env.modul in
    if not @@ Stack.match_ref_type (snd table_typ) (snd elem_typ) then
      Error (`Type_mismatch "table_init")
    else Stack.pop [ i32; i32; i32 ] stack
  | Table_copy (i, i') ->
    let* typ = Env.table_type_get i env.modul in
    let* typ' = Env.table_type_get i' env.modul in
    if not @@ Text.ref_type_eq typ typ' then Error (`Type_mismatch "table_copy")
    else Stack.pop [ i32; i32; i32 ] stack
  | Table_fill i ->
    let* _null, t = Env.table_type_get i env.modul in
    Stack.pop [ i32; Ref_type t; i32 ] stack
  | Table_grow i ->
    let* _null, t = Env.table_type_get i env.modul in
    let* stack = Stack.pop [ i32; Ref_type t ] stack in
    Stack.push [ i32 ] stack
  | Table_size i ->
    let* _null, _t = Env.table_type_get i env.modul in
    Stack.push [ i32 ] stack
  | Ref_is_null ->
    let* stack = Stack.pop_ref stack in
    Stack.push [ i32 ] stack
  | Ref_null rt -> Stack.push [ Ref_type rt ] stack
  | Elem_drop id ->
    let* _elem_typ = Env.elem_type_get id env.modul in
    Ok stack
  | Select t ->
    let* stack = Stack.pop [ i32 ] stack in
    begin match t with
    | None -> begin
      match stack with
      | Ref_type _ :: _tl -> Error (`Type_mismatch "select implicit")
      | Any :: _ -> Ok [ Something; Any ]
      | hd :: Any :: _ -> ok @@ (hd :: [ Any ])
      | hd :: hd' :: tl when Stack.match_types hd hd' -> ok @@ (hd :: tl)
      | _ -> Error (`Type_mismatch "select")
    end
    | Some t ->
      let t = List.map typ_of_val_type t in
      let* stack = Stack.pop t stack in
      let* stack = Stack.pop t stack in
      Stack.push t stack
    end
  | Ref_func i ->
    if not @@ Hashtbl.mem env.refs i then Error `Undeclared_function_reference
    else Stack.push [ Ref_type Func_ht ] stack
  | Br i ->
    let* jt = Env.block_type_get i env in
    let* _stack = Stack.pop jt stack in
    Ok [ any ]
  | Br_if i ->
    let* stack = Stack.pop [ i32 ] stack in
    let* jt = Env.block_type_get i env in
    let* stack = Stack.pop jt stack in
    Stack.push jt stack
  | Br_table (branches, i) ->
    let* stack = Stack.pop [ i32 ] stack in
    let* default_jt = Env.block_type_get i env in
    let* _stack = Stack.pop default_jt stack in
    let* () =
      array_iter
        (fun (i : indice) ->
          let* jt = Env.block_type_get i env in
          if not (List.length jt = List.length default_jt) then
            Error (`Type_mismatch "br_table")
          else
            let* _stack = Stack.pop jt stack in
            Ok () )
        branches
    in
    Ok [ any ]
  | Table_get i ->
    let* _null, t = Env.table_type_get i env.modul in
    let* stack = Stack.pop [ i32 ] stack in
    Stack.push [ Ref_type t ] stack
  | Table_set i ->
    let* _null, t = Env.table_type_get i env.modul in
    Stack.pop [ Ref_type t; i32 ] stack
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
  let* stack = list_fold_left (typecheck_instr env) pt expr.raw in
  if not (Stack.equal rt stack) then
    Error
      (`Type_mismatch
         (Fmt.str "expected a prefix of %a but stack has type %a" Stack.pp rt
            Stack.pp stack ) )
  else
    match Stack.match_prefix ~prefix:pt ~stack:previous_stack with
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
    if not @@ Stack.equal required stack then
      Error (`Type_mismatch "typecheck_function")
    else Ok ()

let typecheck_const_instr (modul : Module.t) refs stack instr =
  match instr.Annotated.raw with
  | I32_const _ -> Stack.push [ i32 ] stack
  | I64_const _ -> Stack.push [ i64 ] stack
  | F32_const _ -> Stack.push [ f32 ] stack
  | F64_const _ -> Stack.push [ f64 ] stack
  | V128_const _ -> Stack.push [ v128 ] stack
  | Ref_null t -> Stack.push [ Ref_type t ] stack
  | Ref_func i ->
    let* _t = Env.func_get i modul in
    Hashtbl.add refs i ();
    Stack.push [ Ref_type Func_ht ] stack
  | Global_get i ->
    if i >= Array.length modul.global then Error (`Unknown_global (Text.Raw i))
    else
      let* mut, typ =
        match modul.global.(i) with
        | Origin.Local _ -> Error (`Unknown_global (Text.Raw i))
        | Imported { typ; _ } -> Ok typ
      in
      let* () =
        match mut with
        | Const -> Ok ()
        | Var -> Error `Constant_expression_required
      in
      Stack.push [ typ_of_val_type typ ] stack
  | I_binop (t, _op) ->
    let t = itype t in
    let* stack = Stack.pop [ t; t ] stack in
    Stack.push [ t ] stack
  | _ -> Error `Constant_expression_required

let typecheck_const_expr (modul : Module.t) refs expr =
  list_fold_left (typecheck_const_instr modul refs) [] expr.Annotated.raw

let typecheck_global (modul : Module.t) refs
  (global : (Global.t, Text.Global.Type.t) Origin.t) =
  match global with
  | Imported _ -> Ok ()
  | Local { typ; init; _ } -> (
    let* real_type = typecheck_const_expr modul refs init in
    match real_type with
    | [ real_type ] ->
      let expected = typ_of_val_type @@ snd typ in
      if not @@ typ_equal expected real_type then
        Error (`Type_mismatch "typecheck global 1")
      else Ok ()
    | _whatever -> Error (`Type_mismatch "typecheck_global 2") )

let typecheck_elem modul refs (elem : Elem.t) =
  let _null, expected_type = elem.typ in
  let* () =
    list_iter
      (fun init ->
        let* real_type = typecheck_const_expr modul refs init in
        match real_type with
        | [ real_type ] ->
          let expected_type = Ref_type expected_type in
          if not @@ typ_equal expected_type real_type then
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
    let* _null, tbl_type = Env.table_type_get tbl_i modul in
    if not @@ Text.heap_type_eq tbl_type expected_type then
      Error (`Type_mismatch "typecheck elem 3")
    else
      let* t = typecheck_const_expr modul refs e in
      match t with
      | [ Ref_type t ] ->
        if not @@ Text.heap_type_eq t tbl_type then
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
  array_iter
    (fun { id; Export.name = _ } ->
      let* () = check_mem modul id in
      Ok () )
    modul.exports.mem

let check_limit { Text.min; max } =
  match max with
  | None -> Ok ()
  | Some max ->
    if min > max then Error `Size_minimum_greater_than_maximum else Ok ()

let validate_tables modul =
  array_iter
    (function
      | Origin.Local (_, (limits, _)) | Imported { typ = limits, _; _ } ->
        check_limit limits )
    modul.table

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
  let* () = array_iter (typecheck_global modul refs) modul.global in
  let* () = array_iter (typecheck_elem modul refs) modul.elem in
  let* () = array_iter (typecheck_data modul refs) modul.data in
  let* () = typecheck_start modul in
  let* () = validate_exports modul in
  let* () = validate_tables modul in
  let* () = validate_mem modul in
  Array.iter
    (fun (export : Export.t) -> Hashtbl.add refs export.id ())
    modul.exports.func;
  array_iter (fun func -> typecheck_function modul func refs) modul.func
