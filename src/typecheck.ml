open Types
open Syntax

type typ =
  | Num_type of Types.num_type
  | Ref_type of Types.heap_type
  | Any
  | Something

let pp_typ fmt = function
  | Num_type t -> Pp.Simplified.num_type fmt t
  | Ref_type t -> Pp.Simplified.heap_type fmt t
  | Any -> Format.fprintf fmt "any"
  | Something -> Format.fprintf fmt "something"

let pp_typ_list fmt l =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.pp_print_string fmt " ")
    pp_typ fmt l

let typ_of_val_type = function
  | Types.Ref_type (_null, t) -> Ref_type t
  | Types.Num_type t -> Num_type t

let typ_of_pt pt = typ_of_val_type @@ snd pt

module Index = struct
  module M = Int
  module Map = Map.Make (Int)
  include M
end

let get_value_at_indice i values =
  match List.find (fun Simplify.{ index; _ } -> index = i) values with
  | { value; _ } -> value

module Env = struct
  type t =
    { locals : typ Index.Map.t
    ; globals :
        (Const.expr global', global_type) Simplify.runtime Simplify.Named.t
    ; result_type : result_type
    ; funcs :
        ((int, Types.func_type) Types.func', func_type) Simplify.runtime
        Simplify.Named.t
    ; blocks : typ list list
    ; tables : (table, table_type) Simplify.runtime Simplify.Named.t
    ; elems : (int, Const.expr) elem' Simplify.Named.t
    ; refs : (int, unit) Hashtbl.t
    }

  let local_get i env = match Index.Map.find i env.locals with v -> v

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

  let table_type_get_from_module i (module_ : Simplify.simplified_module) =
    let value = get_value_at_indice i module_.table.values in
    match value with Local table -> snd (snd table) | Imported t -> snd t.desc

  let table_type_get i env =
    let value = get_value_at_indice i env.tables.values in
    match value with Local table -> snd (snd table) | Imported t -> snd t.desc

  let elem_type_get i env =
    let value = get_value_at_indice i env.elems.values in
    value.type_

  let make ~params ~locals ~globals ~funcs ~result_type ~tables ~elems ~refs =
    let l = List.mapi (fun i v -> (i, v)) (params @ locals) in
    let locals =
      List.fold_left
        (fun locals (i, (_, typ)) ->
          let typ = typ_of_val_type typ in
          Index.Map.add i typ locals )
        Index.Map.empty l
    in
    { locals; globals; result_type; funcs; tables; elems; blocks = []; refs }
end

type env = Env.t

type stack = typ list

type instr = (int, func_type) instr'

let i32 = Num_type I32

let i64 = Num_type I64

let f32 = Num_type F32

let f64 = Num_type F64

let any = Any

let itype = function S32 -> i32 | S64 -> i64

let ftype = function S32 -> f32 | S64 -> f64

module Stack = struct
  let pp fmt (s : stack) = Format.fprintf fmt "[%a]" pp_typ_list s

  let pp_error fmt (expected, got) =
    Format.fprintf fmt "requires %a but stack has %a" pp expected pp got

  let match_num_type (required : Types.num_type) (got : Types.num_type) =
    match (required, got) with
    | I32, I32 -> true
    | I64, I64 -> true
    | F32, F32 -> true
    | F64, F64 -> true
    | _, _ -> false

  let match_ref_type required got =
    match (required, got) with
    | Types.Any_ht, _ -> true
    | None_ht, None_ht -> true
    | Eq_ht, Eq_ht -> true
    | I31_ht, I31_ht -> true
    | Struct_ht, Struct_ht -> true
    | Array_ht, Array_ht -> true
    | No_func_ht, No_func_ht -> true
    | Func_ht, Func_ht -> true
    | Extern_ht, Extern_ht -> true
    | No_extern_ht, No_extern_ht -> true
    | _ ->
      (* TODO: complete this *)
      false

  let match_types required got =
    match (required, got) with
    | Something, _ | _, Something -> true
    | Any, _ | _, Any -> true
    | Num_type required, Num_type got -> match_num_type required got
    | Ref_type required, Ref_type got -> match_ref_type required got
    | Num_type _, Ref_type _ | Ref_type _, Num_type _ -> false

  let rec equal s s' =
    match (s, s') with
    | [], s | s, [] -> List.for_all (( = ) Any) s
    | Any :: tl, Any :: tl' -> equal tl s' || equal s tl'
    | Any :: tl, hd :: tl' | hd :: tl', Any :: tl ->
      equal tl (hd :: tl') || equal (Any :: tl) tl'
    | hd :: tl, hd' :: tl' -> match_types hd hd' && equal tl tl'

  let ( ||| ) l r =
    match (l, r) with None, v | v, None -> v | Some l, _r -> Some l

  let rec match_prefix ~prefix ~stack =
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
    | None -> error_s "type mismatch (pop) %a" pp_error (required, stack)
    | Some stack -> Ok stack

  let pop_ref = function
    | (Something | Ref_type _) :: tl -> Ok tl
    | Any :: _ as stack -> Ok stack
    | _ -> Error "type mismatch (pop_ref)"

  let drop stack =
    match stack with
    | [] -> Error "type mismatch drop"
    | Any :: _ -> Ok stack
    | _ :: tl -> Ok tl

  let push t stack = ok @@ t @ stack

  let pop_push bt stack =
    match bt with
    | None -> Ok stack
    | Some (pt, rt) ->
      let pt, rt =
        (List.rev_map typ_of_pt pt, List.rev_map typ_of_val_type rt)
      in
      let* stack = pop pt stack in
      push rt stack
end

let rec typecheck_instr (env : env) (stack : stack) (instr : instr) :
  (stack, string) Result.t =
  match instr with
  | Nop -> Ok stack
  | Drop -> Stack.drop stack
  | Return ->
    let* _stack =
      Stack.pop (List.rev_map typ_of_val_type env.result_type) stack
    in
    Ok [ any ]
  | Unreachable -> Ok [ any ]
  | I32_const _ -> Stack.push [ i32 ] stack
  | I64_const _ -> Stack.push [ i64 ] stack
  | F32_const _ -> Stack.push [ f32 ] stack
  | F64_const _ -> Stack.push [ f64 ] stack
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
  | Local_get i -> Stack.push [ Env.local_get i env ] stack
  | Local_set i ->
    let t = Env.local_get i env in
    Stack.pop [ t ] stack
  | Local_tee i ->
    let t = Env.local_get i env in
    let* stack = Stack.pop [ t ] stack in
    Stack.push [ t ] stack
  | Global_get i -> Stack.push [ typ_of_val_type @@ Env.global_get i env ] stack
  | Global_set i ->
    let t = Env.global_get i env in
    Stack.pop [ typ_of_val_type t ] stack
  | If_else (_id, block_type, e1, e2) ->
    let* stack = Stack.pop [ i32 ] stack in
    let* stack_e1 = typecheck_expr env e1 ~is_loop:false block_type ~stack in
    let* stack_e2 = typecheck_expr env e2 ~is_loop:false block_type ~stack in
    if not (Stack.equal stack_e1 stack_e2) then Error "type mismatch (if else)"
    else Ok stack_e1
  | I_load (nn, _) | I_load16 (nn, _, _) | I_load8 (nn, _, _) ->
    let* stack = Stack.pop [ i32 ] stack in
    Stack.push [ itype nn ] stack
  | I64_load32 _ ->
    let* stack = Stack.pop [ i32 ] stack in
    Stack.push [ i64 ] stack
  | I_store8 (nn, _) | I_store16 (nn, _) | I_store (nn, _) ->
    Stack.pop [ itype nn; i32 ] stack
  | I64_store32 _ -> Stack.pop [ i64; i32 ] stack
  | F_load (nn, _) ->
    let* stack = Stack.pop [ i32 ] stack in
    Stack.push [ ftype nn ] stack
  | F_store (nn, _) -> Stack.pop [ ftype nn; i32 ] stack
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
  | Memory_grow ->
    let* stack = Stack.pop [ i32 ] stack in
    Stack.push [ i32 ] stack
  | Memory_size -> Stack.push [ i32 ] stack
  | Memory_copy | Memory_init _ | Memory_fill ->
    Stack.pop [ i32; i32; i32 ] stack
  | Block (_, bt, expr) -> typecheck_expr env expr ~is_loop:false bt ~stack
  | Loop (_, bt, expr) -> typecheck_expr env expr ~is_loop:true bt ~stack
  | Call_indirect (_, bt) ->
    let* stack = Stack.pop [ i32 ] stack in
    Stack.pop_push (Some bt) stack
  | Call i ->
    let pt, rt = Env.func_get i env in
    let* stack = Stack.pop (List.rev_map typ_of_pt pt) stack in
    Stack.push (List.rev_map typ_of_val_type rt) stack
  | Return_call i ->
    let pt, rt = Env.func_get i env in
    let* _stack = Stack.pop (List.rev_map typ_of_pt pt) stack in
    if
      not
        (Stack.equal
           (List.rev_map typ_of_val_type rt)
           (List.rev_map typ_of_val_type env.result_type) )
    then Error "type mismatch (return_call)"
    else Ok [ any ]
  | Return_call_indirect (_, (pt, rt)) ->
    let* stack = Stack.pop [ i32 ] stack in
    let* _stack = Stack.pop (List.rev_map typ_of_pt pt) stack in
    if
      not
        (Stack.equal
           (List.rev_map typ_of_val_type rt)
           (List.rev_map typ_of_val_type env.result_type) )
    then Error "type mismatch (return_call_indirect)"
    else Ok [ any ]
  | Data_drop _i -> Ok stack
  | Table_init (ti, ei) ->
    let table_typ = Env.table_type_get ti env in
    let elem_typ = Env.elem_type_get ei env in
    if not @@ Stack.match_ref_type (snd table_typ) (snd elem_typ) then
      Error "type mismatch (table_init)"
    else Stack.pop [ i32; i32; i32 ] stack
  | Table_copy (i, i') ->
    let typ = Env.table_type_get i env in
    let typ' = Env.table_type_get i' env in
    if typ <> typ' then Error "type mismatch (table_copy)"
    else Stack.pop [ i32; i32; i32 ] stack
  | Table_fill i ->
    let _null, t = Env.table_type_get i env in
    Stack.pop [ i32; Ref_type t; i32 ] stack
  | Table_grow i ->
    let _null, t = Env.table_type_get i env in
    let* stack = Stack.pop [ i32; Ref_type t ] stack in
    Stack.push [ i32 ] stack
  | Table_size _ -> Stack.push [ i32 ] stack
  | Ref_is_null ->
    let* stack = Stack.pop_ref stack in
    Stack.push [ i32 ] stack
  | Ref_null rt -> Stack.push [ Ref_type rt ] stack
  | Elem_drop _ -> Ok stack
  | Select t ->
    let* stack = Stack.pop [ i32 ] stack in
    begin
      match t with
      | None -> begin
        let* () =
          match stack with
          | Ref_type _ :: _tl -> Error "type mismatch (select implicit)"
          | _ -> Ok ()
        in
        match stack with
        | Any :: _ -> Ok [ Something; Any ]
        | hd :: Any :: _ -> ok @@ (hd :: [ Any ])
        | hd :: hd' :: tl when Stack.match_types hd hd' -> ok @@ (hd :: tl)
        | _ -> error_s "type mismatch (select) %a" Stack.pp stack
      end
      | Some t ->
        let t = List.map typ_of_val_type t in
        let* stack = Stack.pop t stack in
        let* stack = Stack.pop t stack in
        Stack.push t stack
    end
  | Ref_func i ->
    if not @@ Hashtbl.mem env.refs i then Error "undeclared function reference"
    else Stack.push [ Ref_type Func_ht ] stack
  | Br i ->
    let jt = Env.block_type_get i env in
    let* _stack = Stack.pop jt stack in
    Ok [ any ]
  | Br_if i ->
    let* stack = Stack.pop [ i32 ] stack in
    let jt = Env.block_type_get i env in
    let* stack = Stack.pop jt stack in
    Stack.push (List.rev jt) stack
  | Br_table (branches, i) ->
    let* stack = Stack.pop [ i32 ] stack in
    let default_jt = Env.block_type_get i env in
    let* _stack = Stack.pop default_jt stack in
    let* () =
      array_iter
        (fun i ->
          let jt = Env.block_type_get i env in
          if not (List.length jt = List.length default_jt) then
            Error "type mismatch (br table)"
          else
            let* _stack = Stack.pop jt stack in
            Ok () )
        branches
    in
    Ok [ any ]
  | Table_get i ->
    let _null, t = Env.table_type_get i env in
    let* stack = Stack.pop [ i32 ] stack in
    Stack.push [ Ref_type t ] stack
  | Table_set i ->
    let _null, t = Env.table_type_get i env in
    Stack.pop [ Ref_type t; i32 ] stack

and typecheck_expr env expr ~is_loop (block_type : func_type option)
  ~stack:previous_stack : (stack, string) Result.t =
  let pt, rt =
    Option.fold ~none:([], [])
      ~some:(fun (pt, rt) ->
        (List.rev_map typ_of_pt pt, List.rev_map typ_of_val_type rt) )
      block_type
  in
  let jump_type = if is_loop then pt else rt in
  let env = { env with blocks = jump_type :: env.blocks } in
  let* stack = list_fold_left (typecheck_instr env) (List.rev pt) expr in
  if not (Stack.equal rt stack) then Error "type mismatch (loop)"
  else
    match Stack.match_prefix ~prefix:pt ~stack:previous_stack with
    | None -> Error "type mismatch (param type)"
    | Some stack_to_push -> Stack.push rt stack_to_push

let typecheck_function (module_ : Simplify.simplified_module) func refs =
  match func with
  | Simplify.Imported _ -> Ok ()
  | Local func ->
    let params, result = func.type_f in
    let env =
      Env.make ~params ~funcs:module_.func ~locals:func.locals
        ~globals:module_.global ~result_type:result ~tables:module_.table
        ~elems:module_.elem ~refs
    in
    let* stack =
      typecheck_expr env func.body ~is_loop:false (Some ([], result)) ~stack:[]
    in
    let required = List.rev_map typ_of_val_type (snd func.type_f) in
    if not @@ Stack.equal required stack then
      error_s "type mismatch func %a" Stack.pp_error (required, stack)
    else Ok ()

let typecheck_const_instr (module_ : Simplify.simplified_module) refs stack =
  function
  | Types.Const.I32_const _ -> Stack.push [ i32 ] stack
  | I64_const _ -> Stack.push [ i64 ] stack
  | F32_const _ -> Stack.push [ f32 ] stack
  | F64_const _ -> Stack.push [ f64 ] stack
  | Ref_null t -> Stack.push [ Ref_type t ] stack
  | Ref_func i ->
    Hashtbl.add refs i ();
    Stack.push [ Ref_type Func_ht ] stack
  | Global_get i ->
    let value = get_value_at_indice i module_.global.values in
    let* _mut, type_ =
      match value with
      | Local _ -> Error "unknown global"
      | Imported t -> Ok t.desc
    in
    Stack.push [ typ_of_val_type type_ ] stack
  | I_binop (t, _op) ->
    let t = itype t in
    let* stack = Stack.pop [ t; t ] stack in
    Stack.push [ t ] stack

let typecheck_const_expr (module_ : Simplify.simplified_module) refs =
  list_fold_left (typecheck_const_instr module_ refs) []

let typecheck_global (module_ : Simplify.simplified_module) refs global =
  match global.Simplify.value with
  | Simplify.Imported _ -> Ok ()
  | Local ({ type_; init; _ } : 'expr global') -> (
    let* real_type = typecheck_const_expr module_ refs init in
    match real_type with
    | [ real_type ] ->
      if typ_of_val_type @@ snd type_ <> real_type then
        Error "type mismatch (typecheck_global)"
      else Ok ()
    | _whatever -> Error "type mismatch (typecheck_global wrong num)" )

let typecheck_elem module_ refs (elem : (int, Const.expr) elem' Simplify.indexed)
    =
  let _null, expected_type = elem.value.type_ in
  let* () =
    list_iter
      (fun init ->
        let* real_type = typecheck_const_expr module_ refs init in
        match real_type with
        | [ real_type ] ->
          if Ref_type expected_type <> real_type then
            Error "type mismatch (typecheck_elem)"
          else Ok ()
        | _whatever -> Error "type mismatch (typecheck_elem wrong num)" )
      elem.value.init
  in
  match elem.value.mode with
  | Elem_passive | Elem_declarative -> Ok ()
  | Elem_active (tbl_i, e) -> (
    let _null, tbl_type = Env.table_type_get_from_module tbl_i module_ in
    if tbl_type <> expected_type then Error "type mismatch (elem_active)"
    else
      let* t = typecheck_const_expr module_ refs e in
      match t with
      | [ Ref_type t ] ->
        if t <> tbl_type then Error "type mismatch (elem_active bis)" else Ok ()
      | [ _t ] -> Ok ()
      | _whatever -> Error "type mismatch (typecheck_elem)" )

let typecheck_data module_ refs (data : (int, Const.expr) data' Simplify.indexed)
    =
  match data.value.mode with
  | Data_passive -> Ok ()
  | Data_active (_i, e) -> (
    let* t = typecheck_const_expr module_ refs e in
    match t with
    | [ _t ] -> Ok ()
    | _whatever -> Error "type mismatch (typecheck_data)" )

let module_ (module_ : Simplify.simplified_module) =
  Log.debug "typechecking ...@\n";
  let refs = Hashtbl.create 512 in
  let* () = list_iter (typecheck_global module_ refs) module_.global.values in
  let* () = list_iter (typecheck_elem module_ refs) module_.elem.values in
  let* () = list_iter (typecheck_data module_ refs) module_.data.values in
  List.iter
    (fun (export : Simplify.export) -> Hashtbl.add refs export.id ())
    module_.exports.func;
  Simplify.Named.fold
    (fun _index func acc ->
      let* () = acc in
      typecheck_function module_ func refs )
    module_.func (Ok ())
