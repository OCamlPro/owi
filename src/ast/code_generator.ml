(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Types
open Binary
open Spec
open Syntax

type type_env =
  { param_types : binary val_type array
  ; binder_types : binary val_type array
  ; global_types : binary val_type array
  ; result_types : binary val_type array
  ; binder : int -> binary indice
  ; result : int -> binary indice
  ; copy : binary expr
  ; void_to_i32 : binary indice
  ; i32_to_i32 : binary indice
  ; owi_assert : binary indice
  }

let build_type_env (m : modul)
  (func_ty : binary param_type * binary result_type)
  (owi_funcs : (string * int) array) : type_env * modul =
  let param_types = Array.of_list (List.map snd (fst func_ty)) in
  let binder_types = [||] in
  let global_types =
    Array.map
      (fun (x : (global, binary global_type) Runtime.t) ->
        match x with
        | Runtime.Local { typ = _, gt; _ } -> gt
        | Runtime.Imported { desc = _, gt; _ } -> gt )
      m.global
  in
  let result_types = Array.of_list (snd func_ty) in
  let binder i =
    Raw (Array.length param_types + Array.length result_types + i)
  in
  let result i = Raw (Array.length param_types + i + 1) in
  let copy =
    [ Local_tee (Raw (Array.length param_types))
    ; Local_get (Raw (Array.length param_types))
    ]
  in

  let void_to_i32 =
    [ (None, (Final, [], Def_func_t ([], [ Num_type I32 ]))) ]
  in
  let i32_to_i32 =
    [ ( None
      , (Final, [], Def_func_t ([ (None, Num_type I32) ], [ Num_type I32 ])) )
    ]
  in
  let types = m.types in
  let types, void_to_i32 =
    match Array.find_index (rec_type_eq void_to_i32) types with
    | Some i -> (types, Raw i)
    | None -> (Array.append types [| void_to_i32 |], Raw (Array.length types))
  in
  let types, i32_to_i32 =
    match Array.find_index (rec_type_eq i32_to_i32) types with
    | Some i -> (types, Raw i)
    | None -> (Array.append types [| i32_to_i32 |], Raw (Array.length types))
  in
  let owi_assert =
    match
      Array.find_index (fun (name, _) -> String.equal "assert" name) owi_funcs
    with
    | Some i -> Raw i
    | None -> assert false
  in
  ( { param_types
    ; binder_types
    ; global_types
    ; result_types
    ; binder
    ; result
    ; copy
    ; void_to_i32
    ; i32_to_i32
    ; owi_assert
    }
  , { m with types } )

let prop_true = I32_const (Int32.of_int 1)

let prop_false = I32_const (Int32.of_int 0)

let unop_generate (u : unop) (expr1 : binary expr) (ty1 : binary val_type) :
  (binary expr * binary val_type) Result.t =
  match u with
  | Neg -> (
    match ty1 with
    | Num_type I32 ->
      let expr =
        (I32_const (Int32.of_int 0) :: expr1) @ [ I_binop (S32, Sub) ]
      in
      Ok (expr, Num_type I32)
    | Num_type I64 ->
      let expr =
        (I64_const (Int64.of_int 0) :: expr1) @ [ I_binop (S64, Sub) ]
      in
      Ok (expr, Num_type I64)
    | Num_type F32 ->
      let expr =
        (F32_const (Float32.of_float 0.) :: expr1) @ [ F_binop (S32, Sub) ]
      in
      Ok (expr, Num_type F32)
    | Num_type F64 ->
      let expr =
        (F64_const (Float64.of_float 0.) :: expr1) @ [ F_binop (S64, Sub) ]
      in
      Ok (expr, Num_type F64)
    | Ref_type _ -> Error (`Spec_type_error Fmt.(str "%a" pp_unop u)) )

let binop_generate (b : binop) (expr1 : binary expr) (ty1 : binary val_type)
  (expr2 : binary expr) (ty2 : binary val_type) :
  (binary expr * binary val_type) Result.t =
  match b with
  | Plus -> (
    match (ty1, ty2) with
    | Num_type I32, Num_type I32 ->
      let expr = expr1 @ expr2 @ [ I_binop (S32, Add) ] in
      Ok (expr, Num_type I32)
    | Num_type I64, Num_type I64 ->
      let expr = expr1 @ expr2 @ [ I_binop (S64, Add) ] in
      Ok (expr, Num_type I64)
    | Num_type F32, Num_type F32 ->
      let expr = expr1 @ expr2 @ [ F_binop (S32, Add) ] in
      Ok (expr, Num_type F32)
    | Num_type F64, Num_type F64 ->
      let expr = expr1 @ expr2 @ [ F_binop (S64, Add) ] in
      Ok (expr, Num_type F64)
    | _, _ -> Error (`Spec_type_error Fmt.(str "%a" pp_binop b)) )
  | Minus -> (
    match (ty1, ty2) with
    | Num_type I32, Num_type I32 ->
      let expr = expr1 @ expr2 @ [ I_binop (S32, Sub) ] in
      Ok (expr, Num_type I32)
    | Num_type I64, Num_type I64 ->
      let expr = expr1 @ expr2 @ [ I_binop (S64, Sub) ] in
      Ok (expr, Num_type I64)
    | Num_type F32, Num_type F32 ->
      let expr = expr1 @ expr2 @ [ F_binop (S32, Sub) ] in
      Ok (expr, Num_type F32)
    | Num_type F64, Num_type F64 ->
      let expr = expr1 @ expr2 @ [ F_binop (S64, Sub) ] in
      Ok (expr, Num_type F64)
    | _, _ -> Error (`Spec_type_error Fmt.(str "%a" pp_binop b)) )
  | Mult -> (
    match (ty1, ty2) with
    | Num_type I32, Num_type I32 ->
      let expr = expr1 @ expr2 @ [ I_binop (S32, Mul) ] in
      Ok (expr, Num_type I32)
    | Num_type I64, Num_type I64 ->
      let expr = expr1 @ expr2 @ [ I_binop (S64, Mul) ] in
      Ok (expr, Num_type I64)
    | Num_type F32, Num_type F32 ->
      let expr = expr1 @ expr2 @ [ F_binop (S32, Mul) ] in
      Ok (expr, Num_type F32)
    | Num_type F64, Num_type F64 ->
      let expr = expr1 @ expr2 @ [ F_binop (S64, Mul) ] in
      Ok (expr, Num_type F64)
    | _, _ -> Error (`Spec_type_error Fmt.(str "%a" pp_binop b)) )
  | Div -> (
    match (ty1, ty2) with
    | Num_type I32, Num_type I32 ->
      let expr = expr1 @ expr2 @ [ I_binop (S32, Div S) ] in
      Ok (expr, Num_type I32)
    | Num_type I64, Num_type I64 ->
      let expr = expr1 @ expr2 @ [ I_binop (S64, Div S) ] in
      Ok (expr, Num_type I64)
    | Num_type F32, Num_type F32 ->
      let expr = expr1 @ expr2 @ [ F_binop (S32, Div) ] in
      Ok (expr, Num_type F32)
    | Num_type F64, Num_type F64 ->
      let expr = expr1 @ expr2 @ [ F_binop (S64, Div) ] in
      Ok (expr, Num_type F64)
    | _, _ -> Error (`Spec_type_error Fmt.(str "%a" pp_binop b)) )

let rec term_generate tenv (term : binary term) :
  (binary expr * binary val_type) Result.t =
  match term with
  | Int32 i32 -> Ok ([ I32_const i32 ], Num_type I32)
  | Int64 i64 -> Ok ([ I64_const i64 ], Num_type I64)
  | Float32 f32 -> Ok ([ F32_const f32 ], Num_type F32)
  | Float64 f64 -> Ok ([ F64_const f64 ], Num_type F64)
  | ParamVar (Raw i as id) ->
    if i < 0 || i >= Array.length tenv.param_types then
      Error (`Spec_invalid_indice (Int.to_string i))
    else Ok ([ Local_get id ], tenv.param_types.(i))
  | GlobalVar (Raw i as id) ->
    if i < 0 || i >= Array.length tenv.global_types then
      Error (`Spec_invalid_indice (Int.to_string i))
    else Ok ([ Global_get id ], tenv.global_types.(i))
  | BinderVar (Raw i) ->
    if i < 0 || i >= Array.length tenv.binder_types then
      Error (`Spec_invalid_indice (Int.to_string i))
    else Ok ([ Local_get (tenv.binder i) ], tenv.binder_types.(i))
  | UnOp (u, tm1) ->
    let* expr1, ty1 = term_generate tenv tm1 in
    unop_generate u expr1 ty1
  | BinOp (b, tm1, tm2) ->
    let* expr1, ty1 = term_generate tenv tm1 in
    let* expr2, ty2 = term_generate tenv tm2 in
    binop_generate b expr1 ty1 expr2 ty2
  | Result (Some i) ->
    if i < 0 || i >= Array.length tenv.result_types then
      Error (`Spec_invalid_indice (Int.to_string i))
    else Ok ([ Local_get (tenv.result i) ], tenv.result_types.(i))
  | Result None ->
    if Array.length tenv.result_types = 0 then Error (`Spec_invalid_indice "0")
    else Ok ([ Local_get (tenv.result 0) ], tenv.result_types.(0))
  | Memory tm1 -> (
    let* expr1, ty1 = term_generate tenv tm1 in
    match ty1 with
    | Num_type I32 ->
      Ok
        ( expr1
          @ [ I_load (S32, { offset = Int32.of_int 0; align = Int32.of_int 0 })
            ]
        , Num_type I32 )
    | _ -> Error (`Spec_type_error Fmt.(str "%a" pp_term tm1)) )

let binpred_generate (b : binpred) (expr1 : binary expr) (ty1 : binary val_type)
  (expr2 : binary expr) (ty2 : binary val_type) : binary expr Result.t =
  match b with
  | Ge -> (
    match (ty1, ty2) with
    | Num_type I32, Num_type I32 -> Ok (expr1 @ expr2 @ [ I_relop (S32, Ge S) ])
    | Num_type I64, Num_type I64 -> Ok (expr1 @ expr2 @ [ I_relop (S64, Ge S) ])
    | Num_type F32, Num_type F32 -> Ok (expr1 @ expr2 @ [ F_relop (S32, Ge) ])
    | Num_type F64, Num_type F64 -> Ok (expr1 @ expr2 @ [ F_relop (S64, Ge) ])
    | _, _ -> Error (`Spec_type_error Fmt.(str "%a" pp_binpred b)) )
  | Gt -> (
    match (ty1, ty2) with
    | Num_type I32, Num_type I32 -> Ok (expr1 @ expr2 @ [ I_relop (S32, Gt S) ])
    | Num_type I64, Num_type I64 -> Ok (expr1 @ expr2 @ [ I_relop (S64, Gt S) ])
    | Num_type F32, Num_type F32 -> Ok (expr1 @ expr2 @ [ F_relop (S32, Gt) ])
    | Num_type F64, Num_type F64 -> Ok (expr1 @ expr2 @ [ F_relop (S64, Gt) ])
    | _, _ -> Error (`Spec_type_error Fmt.(str "%a" pp_binpred b)) )
  | Le -> (
    match (ty1, ty2) with
    | Num_type I32, Num_type I32 -> Ok (expr1 @ expr2 @ [ I_relop (S32, Le S) ])
    | Num_type I64, Num_type I64 -> Ok (expr1 @ expr2 @ [ I_relop (S64, Le S) ])
    | Num_type F32, Num_type F32 -> Ok (expr1 @ expr2 @ [ F_relop (S32, Le) ])
    | Num_type F64, Num_type F64 -> Ok (expr1 @ expr2 @ [ F_relop (S64, Le) ])
    | _, _ -> Error (`Spec_type_error Fmt.(str "%a" pp_binpred b)) )
  | Lt -> (
    match (ty1, ty2) with
    | Num_type I32, Num_type I32 -> Ok (expr1 @ expr2 @ [ I_relop (S32, Lt S) ])
    | Num_type I64, Num_type I64 -> Ok (expr1 @ expr2 @ [ I_relop (S64, Lt S) ])
    | Num_type F32, Num_type F32 -> Ok (expr1 @ expr2 @ [ F_relop (S32, Lt) ])
    | Num_type F64, Num_type F64 -> Ok (expr1 @ expr2 @ [ F_relop (S64, Lt) ])
    | _, _ -> Error (`Spec_type_error Fmt.(str "%a" pp_binpred b)) )
  | Eq -> (
    match (ty1, ty2) with
    | Num_type I32, Num_type I32 -> Ok (expr1 @ expr2 @ [ I_relop (S32, Eq) ])
    | Num_type I64, Num_type I64 -> Ok (expr1 @ expr2 @ [ I_relop (S64, Eq) ])
    | Num_type F32, Num_type F32 -> Ok (expr1 @ expr2 @ [ F_relop (S32, Eq) ])
    | Num_type F64, Num_type F64 -> Ok (expr1 @ expr2 @ [ F_relop (S64, Eq) ])
    | _, _ -> Error (`Spec_type_error Fmt.(str "%a" pp_binpred b)) )
  | Neq -> (
    match (ty1, ty2) with
    | Num_type I32, Num_type I32 -> Ok (expr1 @ expr2 @ [ I_relop (S32, Ne) ])
    | Num_type I64, Num_type I64 -> Ok (expr1 @ expr2 @ [ I_relop (S64, Ne) ])
    | Num_type F32, Num_type F32 -> Ok (expr1 @ expr2 @ [ F_relop (S32, Ne) ])
    | Num_type F64, Num_type F64 -> Ok (expr1 @ expr2 @ [ F_relop (S64, Ne) ])
    | _, _ -> Error (`Spec_type_error Fmt.(str "%a" pp_binpred b)) )

let unconnect_generate (u : unconnect) (expr1 : binary expr) :
  binary expr Result.t =
  match u with Not -> Ok ((prop_true :: expr1) @ [ I_binop (S32, Xor) ])

let binconnect_generate (b : binconnect) (expr1 : binary expr)
  (expr2 : binary expr) : binary expr Result.t =
  let bt = Bt_raw (None, ([ (None, Num_type I32) ], [ Num_type I32 ])) in
  match b with
  | And -> Ok (expr1 @ [ If_else (None, Some bt, expr2, [ prop_false ]) ])
  | Or -> Ok (expr1 @ [ If_else (None, Some bt, [ prop_true ], expr2) ])
  | Imply -> Ok (expr1 @ [ If_else (None, Some bt, expr2, [ prop_true ]) ])
  | Equiv ->
    Ok
      ( expr1
      @ [ If_else
            (None, Some bt, expr2, (prop_true :: expr2) @ [ I_binop (S32, Xor) ])
        ] )

let bounded_quantification :
     binary prop
  -> (binder * binder_type * binary term * binary term * binary prop) Result.t =
  function
  | Binder (b, ((I32 | I64) as bt), _, pr1) -> (
    match pr1 with
    | BinConnect
        ( Imply
        , BinConnect
            ( And
            , BinPred (Ge, BinderVar (Raw 0), tm1)
            , BinPred (Le, BinderVar (Raw 0), tm2) )
        , pr2 ) ->
      Ok (b, bt, tm1, tm2, pr2)
    | _ -> Error `Unbounded_quantification )
  | _ -> Error `Unbounded_quantification

let prop_generate tenv : binary prop -> (type_env * binary expr) Result.t =
  let rec prop_generate_aux tenv = function
    | Const true -> Ok (tenv, [ prop_true ])
    | Const false -> Ok (tenv, [ prop_false ])
    | BinPred (b, tm1, tm2) ->
      let* expr1, ty1 = term_generate tenv tm1 in
      let* expr2, ty2 = term_generate tenv tm2 in
      let+ expr = binpred_generate b expr1 ty1 expr2 ty2 in
      (tenv, expr)
    | UnConnect (u, pr1) ->
      let* tenv1, expr1 = prop_generate_aux tenv pr1 in
      let+ expr = unconnect_generate u expr1 in
      (tenv1, expr)
    | BinConnect (b, pr1, pr2) ->
      let* tenv1, expr1 = prop_generate_aux tenv pr1 in
      let* tenv2, expr2 = prop_generate_aux tenv1 pr2 in
      let+ expr = binconnect_generate b expr1 expr2 in
      (tenv2, expr)
    | Binder _ as pr1 ->
      let* b, bt, lower, upper, pr2 = bounded_quantification pr1 in
      let* lower, lower_ty = term_generate tenv lower in
      let* upper, upper_ty = term_generate tenv upper in
      if val_type_eq lower_ty upper_ty && val_type_eq (Num_type bt) lower_ty
      then
        let tenv =
          { tenv with
            binder_types = Array.append [| Num_type bt |] tenv.binder_types
          ; binder =
              (fun i ->
                let (Raw i) = tenv.binder i in
                Raw (i + 1) )
          }
        in
        let+ tenv, expr1 = prop_generate_aux tenv pr2 in
        match b with
        | Forall ->
          let init = lower @ [ Local_set (tenv.binder 0); prop_true ] in
          let incr =
            match bt with
            | I32 ->
              [ Local_get (tenv.binder 0)
              ; I32_const (Int32.of_int 1)
              ; I_binop (S32, Add)
              ; Local_set (tenv.binder 0)
              ]
            | I64 ->
              [ Local_get (tenv.binder 0)
              ; I64_const (Int64.of_int 1)
              ; I_binop (S64, Add)
              ; Local_set (tenv.binder 0)
              ]
            | _ -> assert false
          in
          let check_smaller =
            match bt with
            | I32 ->
              [ Local_get (tenv.binder 0) ] @ upper @ [ I_relop (S32, Le S) ]
            | I64 ->
              [ Local_get (tenv.binder 0) ] @ upper @ [ I_relop (S64, Le S) ]
            | _ -> assert false
          in
          let loop_body =
            expr1
            @ [ I_binop (S32, And) ]
            @ tenv.copy
            @ [ I32_const (Int32.of_int 1); I_binop (S32, Xor); Br_if (Raw 1) ]
            @ incr @ check_smaller @ [ Br_if (Raw 0) ]
          in
          let loop =
            [ Loop
                ( Some "__weasel_loop"
                , Some
                    (Bt_raw
                       ( Some tenv.i32_to_i32
                       , ([ (None, Num_type I32) ], [ Num_type I32 ]) ) )
                , loop_body )
            ]
          in
          ( tenv
          , [ Block
                ( Some "__weasel_forall"
                , Some (Bt_raw (Some tenv.void_to_i32, ([], [ Num_type I32 ])))
                , init @ loop )
            ] )
        | Exists ->
          let init = lower @ [ Local_set (tenv.binder 0); prop_false ] in
          let incr =
            match bt with
            | I32 ->
              [ Local_get (tenv.binder 0)
              ; I32_const (Int32.of_int 1)
              ; I_binop (S32, Add)
              ; Local_set (tenv.binder 0)
              ]
            | I64 ->
              [ Local_get (tenv.binder 0)
              ; I64_const (Int64.of_int 1)
              ; I_binop (S64, Add)
              ; Local_set (tenv.binder 0)
              ]
            | _ -> assert false
          in
          let check_smaller =
            match bt with
            | I32 ->
              [ Local_get (tenv.binder 0) ] @ upper @ [ I_relop (S32, Le S) ]
            | I64 ->
              [ Local_get (tenv.binder 0) ] @ upper @ [ I_relop (S64, Le S) ]
            | _ -> assert false
          in
          let loop_body =
            expr1
            @ [ I_binop (S32, Or) ]
            @ tenv.copy
            @ [ I32_const (Int32.of_int 1); I_binop (S32, Xor); Br_if (Raw 1) ]
            @ incr @ check_smaller @ [ Br_if (Raw 0) ]
          in
          let loop =
            [ Loop
                ( Some "__weasel_loop"
                , Some
                    (Bt_raw
                       ( Some tenv.i32_to_i32
                       , ([ (None, Num_type I32) ], [ Num_type I32 ]) ) )
                , loop_body )
            ]
          in
          ( tenv
          , [ Block
                ( Some "__weasel_exists"
                , Some (Bt_raw (Some tenv.void_to_i32, ([], [ Num_type I32 ])))
                , init @ loop )
            ] )
      else Error `Unbounded_quantification
  in
  fun pr ->
    let+ tenv, expr = prop_generate_aux tenv pr in
    (tenv, expr @ [ Call tenv.owi_assert ])

let subst_index ?(subst_custom = false) (subst_task : (int * int) list)
  (m : modul) : modul =
  let subst i =
    match List.assoc_opt i subst_task with Some j -> j | None -> i
  in
  let rec subst_instr (instr : binary instr) : binary instr =
    match instr with
    | Ref_func (Raw i) -> Ref_func (Raw (subst i))
    | Block (str_opt, bt_opt, expr1) -> Block (str_opt, bt_opt, subst_expr expr1)
    | Loop (str_opt, bt_opt, expr1) -> Loop (str_opt, bt_opt, subst_expr expr1)
    | If_else (str_opt, bt_opt, expr1, expr2) ->
      If_else (str_opt, bt_opt, subst_expr expr1, subst_expr expr2)
    | Return_call (Raw i) -> Return_call (Raw (subst i))
    | Call (Raw i) -> Call (Raw (subst i))
    | instr -> instr
  and subst_expr (expr : binary expr) = List.map subst_instr expr in

  let subst_global (global : (global, binary global_type) Runtime.t) =
    match global with
    | Runtime.Local { typ; init; id } ->
      Runtime.Local { typ; init = subst_expr init; id }
    | Imported _ -> global
  in
  let global = Array.map subst_global m.global in

  let subst_func (func : (binary func, binary block_type) Runtime.t) =
    match func with
    | Runtime.Local { type_f; locals; body; id } ->
      Runtime.Local { type_f; locals; body = subst_expr body; id }
    | Imported _ -> func
  in
  let func = Array.map subst_func m.func in

  let subst_elem_mode = function
    | Elem_passive -> Elem_passive
    | Elem_active (int_opt, expr1) -> Elem_active (int_opt, subst_expr expr1)
    | Elem_declarative -> Elem_declarative
  in
  let subst_elem ({ id; typ; init; mode } : elem) =
    { id; typ; init = List.map subst_expr init; mode = subst_elem_mode mode }
  in
  let elem = Array.map subst_elem m.elem in

  let subst_data_mode = function
    | Data_passive -> Data_passive
    | Data_active (int, expr1) -> Data_active (int, subst_expr expr1)
  in
  let subst_data ({ id; init; mode } : data) =
    { id; init; mode = subst_data_mode mode }
  in
  let data = Array.map subst_data m.data in

  let subst_export ({ name; id } : export) = { name; id = subst id } in
  let exports =
    { m.exports with func = List.map subst_export m.exports.func }
  in

  let start = match m.start with Some i -> Some (subst i) | None -> None in

  let subst_contract
    ({ Contract.funcid = Raw i; preconditions; postconditions } :
      binary Contract.t ) =
    { Contract.funcid = Raw (subst i); preconditions; postconditions }
  in
  let custom =
    if subst_custom then
      List.map
        (function
          | From_annot (Annot.Contract c) ->
            From_annot (Contract (subst_contract c))
          | _ as c -> c )
        m.custom
    else m.custom
  in

  { id = m.id
  ; types = m.types
  ; global
  ; table = m.table
  ; mem = m.mem
  ; func
  ; elem
  ; data
  ; exports
  ; start
  ; custom
  }

let rec binder_locals = function
  | UnConnect (_, pr1) -> binder_locals pr1
  | BinConnect (_, pr1, pr2) -> binder_locals pr1 @ binder_locals pr2
  | Binder (_, bt, _, pr1) -> Num_type bt :: binder_locals pr1
  | _ -> []

let contract_generate (owi_funcs : (string * int) array) (m : modul)
  ({ funcid = Raw old_index; preconditions; postconditions } : binary Contract.t)
  : modul Result.t =
  let func_num = Array.length m.func in
  let* old_id, Bt_raw (ty_index, old_type) =
    if old_index < 0 || old_index >= func_num then
      Error (`Contract_unknown_func (Raw old_index))
    else
      match m.func.(old_index) with
      | Runtime.Local { id; type_f; _ } -> (
        match id with
        | Some name -> Ok (name, type_f)
        | None -> Ok (Fmt.str "func_%i" old_index, type_f) )
      | Imported { modul; name; assigned_name; desc } -> (
        match assigned_name with
        | Some assigned_name -> Ok (assigned_name, desc)
        | None -> Ok (Fmt.str "func_%s_%s_%i" modul name old_index, desc) )
  in
  let id = Fmt.str "__weasel_%s" old_id in
  let index = func_num in

  let tenv, m = build_type_env m old_type owi_funcs in

  let locals =
    [ (Some "__weasel_temp", Num_type I32) ]
    @ List.mapi
        (fun i t -> (Some Fmt.(str "__weasel_res_%i" i), t))
        (Array.to_list tenv.result_types)
    @ List.mapi
        (fun i t -> (Some Fmt.(str "__weasel_binder_%i" i), t))
        (List.concat (List.map binder_locals (preconditions @ postconditions)))
  in
  let call =
    List.init (Array.length tenv.param_types) (fun i -> Local_get (Raw i))
    @ [ Call (Raw old_index) ]
    @ List.init (Array.length tenv.result_types) (fun i ->
          Local_set (tenv.result i) )
  in
  let return =
    List.init (Array.length tenv.result_types) (fun i ->
        Local_get (tenv.result i) )
  in

  let* tenv, precond_checker =
    list_fold_left_map prop_generate tenv preconditions
  in
  let precond_checker = List.concat precond_checker in

  let+ _tenv, postcond_checker =
    list_fold_left_map prop_generate tenv postconditions
  in
  let postcond_checker = List.concat postcond_checker in

  let body = precond_checker @ call @ postcond_checker @ return in

  let m = subst_index [ (old_index, index) ] m in
  let func =
    Array.append m.func
      [| Runtime.Local
           { type_f = Bt_raw (ty_index, old_type); locals; body; id = Some id }
      |]
  in

  { m with func }

let contracts_generate (owi_funcs : (string * int) array) (m : modul)
  (contracts : binary Contract.t list) : modul Result.t =
  let rec join = function
    | ([] | [ _ ]) as l -> l
    | c1 :: c2 :: l ->
      if Contract.compare_funcid c1 c2 <> 0 then c1 :: join (c2 :: l)
      else join (Contract.join_contract c1 c2 :: l)
  in
  (* sort by numerical index and join contracts of a same function *)
  let contracts = join (List.sort Contract.compare_funcid contracts) in
  list_fold_left (contract_generate owi_funcs) m contracts

let add_owi_funcs (owi_funcs : (string * binary func_type) array) (m : modul) :
  modul * (string * int) array =
  (* update module field `types` *)
  let update_types () : modul * (string * (binary func_type * int)) array =
    let func_type2rec_type : binary func_type -> binary rec_type =
     fun ty -> [ (None, (Final, [], Def_func_t ty)) ]
    in
    let owi_funcs : (string * (binary func_type * binary rec_type)) array =
      Array.map
        (fun (name, ty) -> (name, (ty, func_type2rec_type ty)))
        owi_funcs
    in
    let types, owi_funcs =
      Array.fold_left_map
        (fun types (name, (owi_ft, owi_rt)) ->
          match Array.find_index (fun rt -> rec_type_eq rt owi_rt) types with
          | Some index -> (types, (name, (owi_ft, index)))
          | None ->
            ( Array.append types [| owi_rt |]
            , (name, (owi_ft, Array.length types)) ) )
        m.types owi_funcs
    in
    ({ m with types }, owi_funcs)
  in
  let m, owi_funcs = update_types () in

  (* update module field `func` *)
  let update_func () : modul * (string * int) array =
    let func = m.func in
    let func_num = Array.length func in
    let imported, locals =
      let i =
        Option.fold ~none:func_num ~some:Fun.id
          (Array.find_index
             (function Runtime.Local _ -> true | Imported _ -> false)
             func )
      in
      (Array.sub func 0 i, Array.sub func i (func_num - i))
    in
    let owi_funcs =
      Array.map
        (fun (name, (ft, index)) ->
          ( name
          , { Imported.modul = "symbolic"
            ; name
            ; assigned_name = Some name
            ; desc = Bt_raw (Some (Raw index), ft)
            } ) )
        owi_funcs
    in
    let imported =
      Array.append imported
        (Array.map (fun (_, f) -> Runtime.Imported f) owi_funcs)
    in

    let func = Array.append imported locals in
    let m = { m with func } in

    let subst_task =
      List.init (Array.length locals) (fun i -> (i, Array.length imported + i))
    in
    let m = subst_index ~subst_custom:true subst_task m in

    let owi_funcs =
      Array.mapi
        (fun i (name, _) -> (name, Array.length imported + i))
        owi_funcs
    in
    (m, owi_funcs)
  in
  update_func ()

let generate (_symbolic : bool) (m : modul) : modul Result.t =
  let owi_funcs = [| ("assert", ([ (None, Num_type I32) ], [])) |] in
  let m, owi_funcs = add_owi_funcs owi_funcs m in
  contracts_generate owi_funcs m
    (List.filter_map
       (function From_annot (Annot.Contract c) -> Some c | _ -> None)
       m.custom )
