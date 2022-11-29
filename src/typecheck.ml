open Types
module S = Simplify

type typ =
  | Num_type of Types.num_type
  | Ref_type of Types.ref_type
  | Any

let pp fmt = function
  | Num_type t -> Pp.Simplified.num_type fmt t
  | Ref_type t -> Pp.Simplified.ref_type fmt t
  | Any -> Format.fprintf fmt "any"

let typ_of_val_type = function
  | Types.Ref_type t -> Ref_type t
  | Types.Num_type t -> Num_type t

let typ_of_pt pt = typ_of_val_type @@ snd pt

module Index = struct
  module M = struct
    type t = S.index

    let compare = compare
  end

  module Map = Map.Make (M)
  include M
end

let result_type = function None -> [] | Some (_, rt) -> rt

let param_type = function None -> [] | Some (pt, _) -> List.map snd pt

module Env = struct
  type t =
    { locals : typ Index.Map.t
    ; globals : (Const.expr global', global_type) S.runtime S.named
    ; result_type : result_type
    ; funcs : (Simplify.func, func_type) S.runtime S.named
    ; blocks : result_type list
    ; tables : (table, table_type) S.runtime S.named
    ; elems : (int, Const.expr) elem' S.named
    }

  let local_get i env = match Index.Map.find i env.locals with v -> v

  let get_value_at_indice i values =
    match List.find (fun S.{ index; _ } -> index = i) values with
    | { value; _ } -> value

  let global_get i env =
    let value = get_value_at_indice i env.globals.values in
    let _mut, type_ =
      match value with Local { type_; _ } -> type_ | Imported t -> t.desc
    in
    type_

  let func_get i env =
    let value = get_value_at_indice i env.funcs.values in
    match value with Local { type_f; _ } -> type_f | Imported t -> t.desc

  let block_type_get i env = List.nth env.blocks i

  let table_type_get i env =
    let value = get_value_at_indice i env.tables.values in
    match value with Local table -> snd (snd table) | Imported t -> snd t.desc

  let elem_type_get i env =
    let value = get_value_at_indice i env.elems.values in
    value.type_

  let make ~params ~locals ~globals ~funcs ~result_type ~tables ~elems =
    let l = List.mapi (fun i v -> (i, v)) (params @ locals) in
    let locals =
      List.fold_left
        (fun locals (i, (_, typ)) ->
          let typ = typ_of_val_type typ in
          Index.Map.add i typ locals )
        Index.Map.empty l
    in
    { locals; globals; result_type; funcs; tables; elems; blocks = [] }
end

type env = Env.t

type stack = typ list

type instr = (S.index, func_type) instr'

let i32 = Num_type I32

let i64 = Num_type I64

let f32 = Num_type F32

let f64 = Num_type F64

let any = Any

let itype = function S32 -> i32 | S64 -> i64

let ftype = function S32 -> f32 | S64 -> f64

let continue s = s

module Stack = struct
  let pp fmt (s : stack) =
    Format.fprintf fmt "[%a]"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt " ") pp)
      s

  let pp_error fmt (expected, got) =
    Format.fprintf fmt "requires %a but stack has %a" pp expected pp got

  let match_num_type (required : Types.num_type) (got : Types.num_type) =
    match (required, got) with
    | I32, I32 -> true
    | I64, I64 -> true
    | F32, F32 -> true
    | F64, F64 -> true
    | _, _ -> false

  let match_ref_type (required : Types.ref_type) (got : Types.ref_type) =
    match (required, got) with
    | Func_ref, Func_ref -> true
    | Extern_ref, Extern_ref -> true
    | _ -> false

  let match_types required got =
    match (required, got) with
    | Any, _ | _, Any -> true
    | Num_type required, Num_type got -> match_num_type required got
    | Ref_type required, Ref_type got -> match_ref_type required got
    | Num_type _, Ref_type _ | Ref_type _, Num_type _ -> false

  let rec equal s s' =
    Debug.log "EQUAL        : require %a stack %a@." pp s pp s';
    match (s, s') with
    | [], s | s, [] -> List.for_all (( = ) Any) s
    | Any :: tl, Any :: tl' -> equal tl s' || equal s tl'
    | Any :: tl, hd :: tl' | hd :: tl', Any :: tl ->
      equal tl (hd :: tl') || equal (Any :: tl) tl'
    | hd :: tl, hd' :: tl' -> match_types hd hd' && equal tl tl'

  let ( ||| ) l r =
    match (l, r) with None, v | v, None -> v | Some l, _r -> Some l

  let rec match_prefix ~prefix ~stack =
    Debug.log "PREFIX       : require %a stack %a@." pp prefix pp stack;
    match (prefix, stack) with
    | [], stack -> Some stack
    | Any :: tl, [] -> match_prefix ~prefix:tl ~stack:[]
    | Any :: tl, Any :: tl' ->
      match_prefix ~prefix:tl ~stack ||| match_prefix ~prefix ~stack:tl'
    | Any :: tl, _hd :: tl' ->
      match_prefix ~prefix ~stack:tl' ||| match_prefix ~prefix:tl ~stack
    | _hd :: _tl, [] -> None
    | _hd :: tl, Any :: tl' ->
      match_prefix ~prefix ~stack:tl' ||| match_prefix ~prefix:tl ~stack
    | hd :: tl, hd' :: tl' ->
      if match_types hd hd' then match_prefix ~prefix:tl ~stack:tl' else None

  let pop required stack =
    match match_prefix ~prefix:required ~stack with
    | None -> Err.pp "type mismatch (pop) %a" pp_error (required, stack)
    | Some stack -> stack

  let pop_ref = function
    | Ref_type _ :: tl -> tl
    | Any :: _ as stack -> stack
    | _ -> Err.pp "type mismatch (pop_ref)"

  let drop stack =
    match stack with
    | [] -> Err.pp "type mismatch drop"
    | Any :: _ -> stack
    | _ :: tl -> tl

  let push t stack = continue (t @ stack)

  let check_bt bt stack =
    match bt with
    | None ->
      Debug.log "CHECK BT NONE@.";
      if not @@ equal stack [] then Err.pp "type mismatch (check_bt)"
    | Some (pt, rt) -> begin
      Debug.log "CHECK BT SOME@.";
      let pt, rt =
        (List.rev_map typ_of_pt pt, List.rev_map typ_of_val_type rt)
      in
      ignore (pop pt stack |> push rt)
    end

  let pop_push bt stack =
    continue
    @@ Option.fold ~none:[]
         ~some:(fun (pt, rt) ->
           let pt, rt =
             (List.rev_map typ_of_pt pt, List.rev_map typ_of_val_type rt)
           in
           pop pt stack |> push rt )
         bt
end

let rec typecheck_instr (env : env) (stack : stack) (instr : instr) : stack =
  match instr with
  | Nop -> continue stack
  | Drop -> continue (Stack.drop stack)
  | Return ->
    ignore @@ Stack.pop (List.rev_map typ_of_val_type env.result_type) stack;
    continue [ any ]
  | Unreachable -> continue [ any ]
  | I32_const _ -> Stack.push [ i32 ] stack
  | I64_const _ -> Stack.push [ i64 ] stack
  | F32_const _ -> Stack.push [ f32 ] stack
  | F64_const _ -> Stack.push [ f64 ] stack
  | I_unop (s, _op) ->
    let t = itype s in
    Stack.pop [ t ] stack |> Stack.push [ t ]
  | I_binop (s, _op) ->
    let t = itype s in
    Stack.pop [ t; t ] stack |> Stack.push [ t ]
  | F_unop (s, _op) ->
    let t = ftype s in
    Stack.pop [ t ] stack |> Stack.push [ t ]
  | F_binop (s, _op) ->
    let t = ftype s in
    Stack.pop [ t; t ] stack |> Stack.push [ t ]
  | I_testop (nn, _) -> Stack.pop [ itype nn ] stack |> Stack.push [ i32 ]
  | I_relop (nn, _) ->
    let t = itype nn in
    Stack.pop [ t; t ] stack |> Stack.push [ i32 ]
  | F_relop (nn, _) ->
    let t = ftype nn in
    Stack.pop [ t; t ] stack |> Stack.push [ i32 ]
  | Local_get i -> Stack.push [ Env.local_get i env ] stack
  | Local_set i ->
    let t = Env.local_get i env in
    Stack.pop [ t ] stack |> continue
  | Local_tee i ->
    let t = Env.local_get i env in
    Stack.pop [ t ] stack |> Stack.push [ t ]
  | Global_get i -> Stack.push [ typ_of_val_type @@ Env.global_get i env ] stack
  | Global_set i ->
    let t = Env.global_get i env in
    Stack.pop [ typ_of_val_type t ] stack |> continue
  | If_else (_id, block_type, e1, e2) ->
    let stack = Stack.pop [ i32 ] stack in
    let _stack_e1 = typecheck_expr env e1 (result_type block_type) block_type in
    let _stack_e2 = typecheck_expr env e2 (result_type block_type) block_type in
    Stack.check_bt block_type stack;
    Stack.pop_push block_type stack
  | I_load (nn, _) | I_load16 (nn, _, _) | I_load8 (nn, _, _) ->
    Stack.pop [ i32 ] stack |> Stack.push [ itype nn ]
  | I64_load32 _ -> Stack.pop [ i32 ] stack |> Stack.push [ i64 ]
  | I_store8 (nn, _) | I_store16 (nn, _) | I_store (nn, _) ->
    Stack.pop [ itype nn; i32 ] stack |> continue
  | I64_store32 _ -> Stack.pop [ i64; i32 ] stack |> continue
  | F_load (nn, _) -> Stack.pop [ i32 ] stack |> Stack.push [ ftype nn ]
  | F_store (nn, _) -> Stack.pop [ ftype nn; i32 ] stack |> continue
  | I_reinterpret_f (inn, fnn) ->
    Stack.pop [ ftype fnn ] stack |> Stack.push [ itype inn ]
  | F_reinterpret_i (fnn, inn) ->
    Stack.pop [ itype inn ] stack |> Stack.push [ ftype fnn ]
  | F32_demote_f64 -> Stack.pop [ f64 ] stack |> Stack.push [ f32 ]
  | F64_promote_f32 -> Stack.pop [ f32 ] stack |> Stack.push [ f64 ]
  | F_convert_i (fnn, inn, _) ->
    Stack.pop [ itype inn ] stack |> Stack.push [ ftype fnn ]
  | I_trunc_f (inn, fnn, _) | I_trunc_sat_f (inn, fnn, _) ->
    Stack.pop [ ftype fnn ] stack |> Stack.push [ itype inn ]
  | I32_wrap_i64 -> Stack.pop [ i64 ] stack |> Stack.push [ i32 ]
  | I_extend8_s nn | I_extend16_s nn ->
    let t = itype nn in
    Stack.pop [ t ] stack |> Stack.push [ t ]
  | I64_extend32_s -> Stack.pop [ i64 ] stack |> Stack.push [ i64 ]
  | I64_extend_i32 _ -> Stack.pop [ i32 ] stack |> Stack.push [ i64 ]
  | Memory_grow -> Stack.pop [ i32 ] stack |> Stack.push [ i32 ]
  | Memory_size -> Stack.push [ i32 ] stack
  | Memory_copy | Memory_init _ | Memory_fill ->
    Stack.pop [ i32; i32; i32 ] stack |> continue
  | Block (_, bt, expr) -> typecheck_expr env expr (result_type bt) bt
  | Loop (_, bt, expr) -> typecheck_expr env expr (param_type bt) bt
  | Call_indirect (_, bt) ->
    let stack = Stack.pop [ i32 ] stack in
    Stack.check_bt (Some bt) stack;
    Stack.pop_push (Some bt) stack
  | Call i ->
    let pt, rt = Env.func_get i env in
    Stack.pop (List.rev_map typ_of_pt pt) stack
    |> Stack.push (List.map typ_of_val_type rt)
  | Data_drop _i -> stack |> continue
  | Table_init (ti, ei) ->
    let table_typ = Env.table_type_get ti env in
    let elem_typ = Env.elem_type_get ei env in
    if table_typ <> elem_typ then Err.pp "type mismatch";
    Stack.pop [ i32; i32; i32 ] stack |> continue
  | Table_copy (i, i') ->
    let typ = Env.table_type_get i env in
    let typ' = Env.table_type_get i' env in
    if typ <> typ' then Err.pp "type mismatch";
    Stack.pop [ i32; i32; i32 ] stack |> continue
  | Table_fill i ->
    let t_type = Env.table_type_get i env in
    Stack.pop [ i32; Ref_type t_type; i32 ] stack |> continue
  | Table_grow i ->
    let t_type = Env.table_type_get i env in
    Stack.pop [ i32; Ref_type t_type ] stack |> Stack.push [ i32 ]
  | Table_size _ -> Stack.push [ i32 ] stack
  | Ref_is_null -> Stack.pop_ref stack |> Stack.push [ i32 ]
  | Ref_null rt -> Stack.push [ Ref_type rt ] stack
  | Elem_drop _ -> continue stack
  | Select t ->
    let stack = Stack.pop [ i32 ] stack in
    begin
      match t with
      | None -> begin
        Debug.log "NONE@.";
        match stack with
        | Any :: hd :: tl | hd :: Any :: tl -> continue (hd :: tl)
        | hd :: hd' :: tl when Stack.match_types hd hd' -> continue (hd :: tl)
        | _ -> Err.pp "type mismatch (select)"
      end
      | Some t ->
        Debug.log "SOME@.";
        let t = List.map typ_of_val_type t in
        Stack.pop t stack |> Stack.pop t |> Stack.push t
    end
  | Ref_func _i -> Stack.push [ Ref_type Func_ref ] stack
  (* TODO: *)
  | Br i ->
    let jt = Env.block_type_get i env in
    Debug.log "EXPECTED PREFIX: %a@." Pp.Simplified.result_type jt;
    ignore @@ Stack.pop (List.map typ_of_val_type jt) stack;
    continue [ any ]
  | Br_if i ->
    let stack = Stack.pop [ i32 ] stack in
    let jt = Env.block_type_get i env in
    ignore @@ Stack.pop (List.rev_map typ_of_val_type jt) stack;
    continue stack
  | Br_table (_, i) ->
    let stack = Stack.pop [ i32 ] stack in
    let jt = Env.block_type_get i env in
    ignore @@ Stack.pop (List.rev_map typ_of_val_type jt) stack;
    continue [ any ]
  | Table_get i ->
    let t_typ = Env.table_type_get i env in
    Stack.pop [ i32 ] stack |> Stack.push [ Ref_type t_typ ]
  | Table_set i ->
    let t_typ = Env.table_type_get i env in
    Stack.pop [ Ref_type t_typ; i32 ] stack |> continue

and typecheck_expr env expr jump_type (block_type : func_type option) : stack =
  let env = { env with blocks = jump_type :: env.blocks } in
  let rec loop stack expr =
    match expr with
    | [] -> continue stack
    | instr :: tail -> (
      Debug.log "STACK BEFORE: %a@." Stack.pp stack;
      Debug.log "CURRENT INST: %a@." Pp.Simplified.instr instr;
      match typecheck_instr env stack instr with
      | stack ->
        Debug.log "STACK  AFTER: %a@." Stack.pp stack;
        loop stack tail )
  in
  let pt, rt =
    Option.fold ~none:([], [])
      ~some:(fun (pt, rt) ->
        (List.map typ_of_pt pt, List.map typ_of_val_type rt) )
      block_type
  in
  match loop pt expr with
  | stack -> (
    Debug.log "LOOP IS OVER WITH STACK: %a@." Stack.pp stack;
    match Stack.match_prefix ~prefix:(List.rev rt) ~stack with
    | None -> Err.pp "type mismatch (loop)"
    | Some _ -> continue stack )

let typecheck_function (module_ : Simplify.result) func =
  match func with
  | S.Imported _ -> ()
  | Local func -> (
    let params, result = func.type_f in
    let env =
      Env.make ~params ~funcs:module_.func ~locals:func.locals
        ~globals:module_.global ~result_type:result ~tables:module_.table
        ~elems:module_.elem
    in
    Debug.log "TYPECHECK function@.%a@." Pp.Simplified.func func;
    match typecheck_expr env func.body result (Some ([], result)) with
    | stack ->
      let required = List.rev_map typ_of_val_type (snd func.type_f) in
      Debug.log "FUNCTION EXPECTS: %a@." Stack.pp required;
      Debug.log "STACK         IS: %a@." Stack.pp stack;
      begin
        if not @@ Stack.equal required stack then
          Err.pp "type mismatch func %a" Stack.pp_error (required, stack)
      end )

let typecheck_module (module_ : Simplify.result) =
  S.Fields.iter
    (fun _index func -> typecheck_function module_ func)
    module_.func
