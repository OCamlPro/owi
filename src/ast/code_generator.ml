(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Types
open Binary
open Spec
open Syntax

let type_env (m : modul) (func_ty : binary param_type * binary result_type)
  (owi_funcs : (string * int) list) =
  object
    val param_types : binary val_type list = List.map snd (fst func_ty)

    val global_types : binary val_type list =
      let sorted_global_types =
        List.sort
          (fun x y -> compare (Indexed.get_index x) (Indexed.get_index y))
          m.global.values
      in
      List.map
        (fun (x : (global, binary global_type) Runtime.t Indexed.t) ->
          match Indexed.get x with
          | Runtime.Local { typ = _, gt; _ } -> gt
          | Runtime.Imported { desc = _, gt; _ } -> gt )
        sorted_global_types

    val result_types : binary val_type list = snd func_ty

    val param_number : int = List.length (fst func_ty)

    val result_number : int = List.length (snd func_ty)

    val owi_i32 : int = List.assoc "i32_symbol" owi_funcs

    val owi_i64 : int = List.assoc "i64_symbol" owi_funcs

    val owi_f32 : int = List.assoc "f32_symbol" owi_funcs

    val owi_f64 : int = List.assoc "f64_symbol" owi_funcs

    val owi_assume : int = List.assoc "assume" owi_funcs

    val owi_assert : int = List.assoc "assert" owi_funcs

    method get_param_type (Raw i : binary indice) : binary val_type option =
      List.nth_opt param_types i

    method get_global_type (Raw i : binary indice) : binary val_type option =
      List.nth_opt global_types i

    method get_binder_type_and_index (Raw _i : binary indice)
      : (binary indice * binary val_type) option =
      None (* TODO *)

    method get_result_type (i : int) : binary val_type option =
      List.nth_opt result_types i

    method get_param_number : int = param_number

    method get_result_number : int = result_number

    method get_result_types : binary val_type list = result_types

    method get_owi_i32 : int = owi_i32

    method get_owi_i64 : int = owi_i64

    method get_owi_f32 : int = owi_f32

    method get_owi_f64 : int = owi_f64

    method get_owi_assume : int = owi_assume

    method get_owi_assert : int = owi_assert
  end

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

let rec term_generate tenv term : (binary expr * binary val_type) Result.t =
  match term with
  | Int32 i32 -> Ok ([ I32_const i32 ], Num_type I32)
  | Int64 i64 -> Ok ([ I64_const i64 ], Num_type I64)
  | Float32 f32 -> Ok ([ F32_const f32 ], Num_type F32)
  | Float64 f64 -> Ok ([ F64_const f64 ], Num_type F64)
  | ParamVar id -> (
    match tenv#get_param_type id with
    | Some t -> Ok ([ Local_get id ], t)
    | None -> Error (`Spec_type_error Fmt.(str "%a" pp_term term)) )
  | GlobalVar id -> (
    match tenv#get_global_type id with
    | Some t -> Ok ([ Global_get id ], t)
    | None -> Error (`Spec_type_error Fmt.(str "%a" pp_term term)) )
  | BinderVar id -> (
    match tenv#get_binder_type_and_index id with
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
    match tenv#get_result_type i with
    | Some t -> Ok ([ Local_get (Raw (tenv#get_param_number + i)) ], t)
    | None -> Error (`Spec_type_error Fmt.(str "%a" pp_term term)) )
  | Result None -> (
    match tenv#get_result_type 0 with
    | Some t -> Ok ([ Local_get (Raw tenv#get_param_number) ], t)
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
    | Binder (_b, _bt, _, _pr1) ->
      (* TODO : quantifier checking *)
      Ok []
  in
  fun pr ->
    let+ expr = prop_generate_aux pr in
    expr @ [ Call (Raw tenv#get_owi_assert) ]

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
  let global =
    { m.global with
      values =
        List.map
          (fun v -> Indexed.(return (get_index v) (subst_global (get v))))
          m.global.values
    }
  in

  let subst_func (func : (binary func, binary block_type) Runtime.t) =
    match func with
    | Runtime.Local { type_f; locals; body; id } ->
      Runtime.Local { type_f; locals; body = subst_expr body; id }
    | Imported _ -> func
  in
  let func =
    { m.func with
      values =
        List.map
          (fun v -> Indexed.(return (get_index v) (subst_func (get v))))
          m.func.values
    }
  in

  let subst_elem_mode = function
    | Elem_passive -> Elem_passive
    | Elem_active (int_opt, expr1) -> Elem_active (int_opt, subst_expr expr1)
    | Elem_declarative -> Elem_declarative
  in
  let subst_elem ({ id; typ; init; mode } : elem) =
    { id; typ; init = List.map subst_expr init; mode = subst_elem_mode mode }
  in
  let elem =
    { m.elem with
      values =
        List.map
          (fun v -> Indexed.(return (get_index v) (subst_elem (get v))))
          m.elem.values
    }
  in

  let subst_data_mode = function
    | Data_passive -> Data_passive
    | Data_active (int, expr1) -> Data_active (int, subst_expr expr1)
  in
  let subst_data ({ id; init; mode } : data) =
    { id; init; mode = subst_data_mode mode }
  in
  let data =
    { m.data with
      values =
        List.map
          (fun v -> Indexed.(return (get_index v) (subst_data (get v))))
          m.data.values
    }
  in

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

let contract_generate (owi_funcs : (string * int) list) (m : modul)
  ({ funcid = Raw old_index; preconditions; postconditions } : binary Contract.t)
  : modul Result.t =
  let* old_id, Bt_raw (ty_index, old_type) =
    match Indexed.get_at old_index m.func.values with
    | Some (Runtime.Local { id; type_f; _ }) -> (
      match id with
      | Some id -> Ok (id, type_f)
      | None -> Ok (Fmt.str "func_%i" old_index, type_f) )
    | Some (Imported { modul; name; assigned_name; desc }) -> (
      match assigned_name with
      | Some assigned_name -> Ok (assigned_name, desc)
      | None -> Ok (Fmt.str "func_%s_%s_%i" modul name old_index, desc) )
    | None -> Error (`Contract_unknown_func (Raw old_index))
  in
  let index = List.length m.func.values in
  let id = Fmt.str "__rac_%s" old_id in

  let tenv = type_env m old_type owi_funcs in

  let locals =
    List.mapi
      (fun i rt -> (Some Fmt.(str "__rac_res_%i" i), rt))
      tenv#get_result_types
  in
  let call =
    List.init tenv#get_param_number (fun i -> Local_get (Raw i))
    @ [ Call (Raw old_index) ]
    @ List.init tenv#get_result_number (fun i ->
          Local_set (Raw (tenv#get_param_number + i)) )
  in
  let return =
    List.init tenv#get_result_number (fun i ->
        Local_get (Raw (tenv#get_param_number + i)) )
  in
  let* precond_checker = list_concat_map (prop_generate tenv) preconditions in
  let+ postcond_checker = list_concat_map (prop_generate tenv) postconditions in
  let body = precond_checker @ call @ postcond_checker @ return in

  let m = subst_index old_index index m in

  let value =
    Runtime.Local
      { type_f = Bt_raw (ty_index, old_type); locals; body; id = Some id }
  in
  let func =
    { Named.values = Indexed.return index value :: m.func.values
    ; named = String_map.add id index m.func.named
    }
  in
  { m with func }

let contracts_generate (owi_funcs : (string * int) list) (m : modul)
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

let add_owi_funcs (m : modul) : modul * (string * int) list =
  let owi_funcs : (string * binary func_type) list =
    [ ("i32_symbol", ([], [ Num_type I32 ]))
    ; ("i64_symbol", ([], [ Num_type I64 ]))
    ; ("f32_symbol", ([], [ Num_type F32 ]))
    ; ("f64_symbol", ([], [ Num_type F64 ]))
    ; ("assume", ([ (None, Num_type I32) ], []))
    ; ("assert", ([ (None, Num_type I32) ], []))
    ]
  in

  (* update module field `types` *)
  let update_types () : modul * (string * (binary func_type * int)) list =
    let func_type2rec_type : binary func_type -> binary rec_type =
     fun ty -> [ (None, (Final, [], Def_func_t ty)) ]
    in
    let owi_funcs : (string * (binary func_type * binary rec_type)) list =
      List.map (fun (name, ty) -> (name, (ty, func_type2rec_type ty))) owi_funcs
    in
    let values = m.types.values in
    let values, owi_funcs =
      List.fold_left_map
        (fun values (name, (ft, rt)) ->
          match
            List.find_map
              (fun (index, rt') ->
                if rec_type_eq rt rt' then Some index else None )
              (Indexed.to_assoc_list values)
          with
          | Some index -> (values, (name, (ft, index)))
          | None ->
            let index = List.length values in
            (Indexed.return index rt :: values, (name, (ft, index))) )
        (List.rev values) owi_funcs
    in
    let values = List.rev values in
    ({ m with types = { values; named = m.types.named } }, owi_funcs)
  in
  let m, owi_funcs = update_types () in

  (* update module field `func` *)
  let update_func () : modul * (string * int) list =
    let imported, locals =
      List.partition_map
        (fun i ->
          let v = Indexed.get i in
          match v with
          | Runtime.Imported _ -> Either.Left (Indexed.get_index i, v)
          | Local _ -> Either.Right (Indexed.get_index i, v) )
        m.func.values
    in
    let imported_num = List.length imported in
    let owi_funcs =
      List.mapi
        (fun i (name, (ty, index)) ->
          ( name
          , ( { Imported.modul = "symbolic"
              ; name
              ; assigned_name = Some name
              ; desc = Bt_raw (Some (Raw index), ty)
              }
            , imported_num + i ) ) )
        owi_funcs
    in

    let imported =
      List.map
        (fun (_, (f, index)) -> (index, Runtime.Imported f))
        (List.rev owi_funcs)
      @ imported
    in

    let subst_task, locals =
      List.fold_left_map
        (fun subst_task (old_index, f) ->
          let index = old_index + List.length owi_funcs in
          ((old_index, index) :: subst_task, (index, f)) )
        [] locals
    in

    let values =
      List.map (fun (index, f) -> Indexed.return index f) (imported @ locals)
    in
    let named =
      List.map
        (fun (name, index) ->
          if index < imported_num then (name, index)
          else (name, index + List.length owi_funcs) )
        (String_map.to_list m.func.named)
    in
    let named =
      String_map.of_list
        (List.map (fun (name, (_, index)) -> (name, index)) owi_funcs @ named)
    in

    let m = { m with func = { values; named } } in

    let m =
      List.fold_left
        (fun m (old_index, index) ->
          subst_index ~subst_custom:true old_index index m )
        m subst_task
    in
    let owi_funcs =
      List.map (fun (name, (_, index)) -> (name, index)) owi_funcs
    in
    (m, owi_funcs)
  in
  update_func ()

let generate (enabled : bool) (m : modul) : modul Result.t =
  if not enabled then Ok m
  else
    let m, owi_funcs = add_owi_funcs m in
    contracts_generate owi_funcs m
      (List.filter_map
         (function From_annot (Annot.Contract c) -> Some c | _ -> None)
         m.custom )
