(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Types
open Binary
open Spec
open Syntax

type type_env =
  { param_types : binary val_type list
  ; global_types : binary val_type list
  ; result_types : binary val_type list
  ; result : int -> binary indice
  ; owi_i32 : binary indice
  ; owi_i64 : binary indice
  ; owi_f32 : binary indice
  ; owi_f64 : binary indice
  ; owi_assume : binary indice
  ; owi_assert : binary indice
  }

let build_type_env (m : modul)
  (func_ty : binary param_type * binary result_type)
  (owi_funcs : (string * int) array) : type_env =
  let param_types = List.map snd (fst func_ty) in
  let global_types =
    List.map
      (fun (x : (global, binary global_type) Runtime.t) ->
        match x with
        | Runtime.Local { typ = _, gt; _ } -> gt
        | Runtime.Imported { desc = _, gt; _ } -> gt )
      (Array.to_list m.global)
  in
  let result_types = snd func_ty in
  let result i = Raw (List.length param_types + i + 1) in
  let owi_i32 =
    match
      Array.find_index
        (fun (name, _) -> String.equal "i32_symbol" name)
        owi_funcs
    with
    | Some i -> Raw i
    | None -> assert false
  in
  let owi_i64 =
    match
      Array.find_index
        (fun (name, _) -> String.equal "i64_symbol" name)
        owi_funcs
    with
    | Some i -> Raw i
    | None -> assert false
  in
  let owi_f32 =
    match
      Array.find_index
        (fun (name, _) -> String.equal "f32_symbol" name)
        owi_funcs
    with
    | Some i -> Raw i
    | None -> assert false
  in
  let owi_f64 =
    match
      Array.find_index
        (fun (name, _) -> String.equal "f64_symbol" name)
        owi_funcs
    with
    | Some i -> Raw i
    | None -> assert false
  in
  let owi_assume =
    match
      Array.find_index (fun (name, _) -> String.equal "assume" name) owi_funcs
    with
    | Some i -> Raw i
    | None -> assert false
  in
  let owi_assert =
    match
      Array.find_index (fun (name, _) -> String.equal "assert" name) owi_funcs
    with
    | Some i -> Raw i
    | None -> assert false
  in
  { param_types
  ; global_types
  ; result_types
  ; result
  ; owi_i32
  ; owi_i64
  ; owi_f32
  ; owi_f64
  ; owi_assume
  ; owi_assert
  }

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
  | CustomUnOp _ -> Error (`Spec_type_error Fmt.(str "%a" pp_unop u))

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
  | CustomBinOp _ -> Error (`Spec_type_error Fmt.(str "%a" pp_binop b))

let rec term_generate tenv (term : binary term) :
  (binary expr * binary val_type) Result.t =
  match term with
  | Int32 i32 -> Ok ([ I32_const i32 ], Num_type I32)
  | Int64 i64 -> Ok ([ I64_const i64 ], Num_type I64)
  | Float32 f32 -> Ok ([ F32_const f32 ], Num_type F32)
  | Float64 f64 -> Ok ([ F64_const f64 ], Num_type F64)
  | ParamVar (Raw i as id) -> (
    match List.nth_opt tenv.param_types i with
    | Some t -> Ok ([ Local_get id ], t)
    | None -> Error (`Spec_type_error Fmt.(str "%a" pp_term term)) )
  | GlobalVar (Raw i as id) -> (
    match List.nth_opt tenv.global_types i with
    | Some t -> Ok ([ Global_get id ], t)
    | None -> Error (`Spec_type_error Fmt.(str "%a" pp_term term)) )
  | BinderVar (Raw _i as _id) -> (
    match None with
    | Some (id, t) -> Ok ([ Local_get id ], t)
    | None -> Error (`Spec_type_error Fmt.(str "%a" pp_term term)) )
  | UnOp (u, tm1) ->
    let* expr1, ty1 = term_generate tenv tm1 in
    unop_generate u expr1 ty1
  | BinOp (b, tm1, tm2) ->
    let* expr1, ty1 = term_generate tenv tm1 in
    let* expr2, ty2 = term_generate tenv tm2 in
    binop_generate b expr1 ty1 expr2 ty2
  | Result (Some i) -> (
    match List.nth_opt tenv.result_types i with
    | Some t -> Ok ([ Local_get (tenv.result i) ], t)
    | None -> Error (`Spec_type_error Fmt.(str "%a" pp_term term)) )
  | Result None -> (
    match List.nth_opt tenv.result_types 0 with
    | Some t -> Ok ([ Local_get (tenv.result 0) ], t)
    | None -> Error (`Spec_type_error Fmt.(str "%a" pp_term term)) )

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

let prop_generate tenv : binary prop -> binary expr Result.t =
  let rec prop_generate_aux = function
    | Const true -> Ok [ prop_true ]
    | Const false -> Ok [ prop_false ]
    | BinPred (b, tm1, tm2) ->
      let* expr1, ty1 = term_generate tenv tm1 in
      let* expr2, ty2 = term_generate tenv tm2 in
      binpred_generate b expr1 ty1 expr2 ty2
    | UnConnect (u, pr1) ->
      let* expr1 = prop_generate_aux pr1 in
      unconnect_generate u expr1
    | BinConnect (b, pr1, pr2) ->
      let* expr1 = prop_generate_aux pr1 in
      let* expr2 = prop_generate_aux pr2 in
      binconnect_generate b expr1 expr2
    | Binder (_b, bt, _, _pr1) -> (
      match bt with
      | I32 -> Ok [ Call tenv.owi_i32 ]
      | I64 -> Ok [ Call tenv.owi_i64 ]
      | F32 -> Ok [ Call tenv.owi_f32 ]
      | F64 -> Ok [ Call tenv.owi_f64 ] )
    (* TODO : quantifier checking *)
  in
  fun pr ->
    let+ expr = prop_generate_aux pr in
    expr @ [ Call tenv.owi_assert ]

let subst_index ?(subst_custom = false) (old_index : int) (index : int)
  (m : modul) : modul =
  let subst i = if i = old_index then index else i in
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

let contract_generate (owi_funcs : (string * int) array) (m : modul)
  ({ funcid = Raw old_index; preconditions; postconditions } : binary Contract.t)
  : modul Result.t =
  let func = m.func in
  let func_num = Array.length func in
  let* old_id, Bt_raw (ty_index, old_type) =
    if old_index >= func_num || old_index < 0 then
      Error (`Contract_unknown_func (Raw old_index))
    else
      match Array.get func old_index with
      | Runtime.Local { id; type_f; _ } -> (
        match id with
        | Some id -> Ok (id, type_f)
        | None -> Ok (Fmt.str "func_%i" old_index, type_f) )
      | Imported { modul; name; assigned_name; desc } -> (
        match assigned_name with
        | Some assigned_name -> Ok (assigned_name, desc)
        | None -> Ok (Fmt.str "func_%s_%s_%i" modul name old_index, desc) )
  in
  let id = Fmt.str "__rac_%s" old_id in
  let index = func_num in

  let tenv = build_type_env m old_type owi_funcs in

  let locals =
    List.mapi
      (fun i rt -> (Some Fmt.(str "__rac_res_%i" (i + 1)), rt))
      tenv.result_types
  in
  let call =
    List.init (List.length tenv.param_types) (fun i -> Local_get (Raw i))
    @ [ Call (Raw old_index) ]
    @ List.init (List.length tenv.result_types) (fun i ->
          Local_set (tenv.result i) )
  in
  let return =
    List.init (List.length tenv.result_types) (fun i ->
        Local_get (tenv.result i) )
  in
  let* precond_checker = list_concat_map (prop_generate tenv) preconditions in
  let+ postcond_checker = list_concat_map (prop_generate tenv) postconditions in
  let body = precond_checker @ call @ postcond_checker @ return in

  let m = subst_index old_index index m in
  let func =
    Array.append func
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

let add_owi_funcs (m : modul) : modul * (string * int) array =
  let owi_funcs : (string * binary func_type) array =
    [| ("i32_symbol", ([], [ Num_type I32 ]))
     ; ("i64_symbol", ([], [ Num_type I64 ]))
     ; ("f32_symbol", ([], [ Num_type F32 ]))
     ; ("f64_symbol", ([], [ Num_type F64 ]))
     ; ("assume", ([ (None, Num_type I32) ], []))
     ; ("assert", ([ (None, Num_type I32) ], []))
    |]
  in

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
        Option.fold ~none:func_num
          ~some:(fun x -> x)
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
    let m =
      List.fold_left
        (fun m (old_index, index) ->
          subst_index ~subst_custom:true old_index index m )
        m subst_task
    in

    let owi_funcs =
      Array.mapi
        (fun i (name, _) -> (name, Array.length imported + i))
        owi_funcs
    in
    (m, owi_funcs)
  in
  update_func ()

let generate (m : modul) : modul Result.t =
  let m, owi_funcs = add_owi_funcs m in
  contracts_generate owi_funcs m
    (List.filter_map
       (function From_annot (Annot.Contract c) -> Some c | _ -> None)
       m.custom )
