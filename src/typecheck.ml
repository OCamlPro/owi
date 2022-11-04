open Types
module S = Simplify

type typ = Types.val_type

module Index = struct
  module M = struct
    type t = S.index

    let compare = compare
  end

  module Map = Map.Make (M)
  include M
end

module Env = struct
  type t =
    { locals : typ Index.Map.t
    ; globals : (Const.expr global', global_type) S.runtime S.named
    ; result_type : result_type
    }

  let local_get i t =
    match Index.Map.find i t.locals with
    | exception Not_found -> assert false
    | v -> v

  let global_get i t =
    match List.find (fun S.{ index; _ } -> index = i) t.globals.values with
    | exception Not_found -> assert false
    | { value; _ } ->
      let _mut, type_ =
        match value with Local { type_; _ } -> type_ | Imported t -> t.desc
      in
      type_

  let make ~params ~locals ~globals ~result_type =
    let l = List.mapi (fun i v -> (i, v)) (params @ locals) in
    let locals =
      List.fold_left
        (fun locals (i, (_, typ)) -> Index.Map.add i typ locals)
        Index.Map.empty l
    in
    { locals; globals; result_type }
end

type env = Env.t

type stack = typ list

type instr = (S.index, func_type) instr'

type expr = (S.index, func_type) expr'

let i32 = Num_type I32

let i64 = Num_type I64

let f32 = Num_type F32

let f64 = Num_type F64

let itype = function S32 -> i32 | S64 -> i64

let ftype = function S32 -> f32 | S64 -> f64

type state =
  | Stop
  | Continue of stack

module Stack = struct
  let pp fmt (s : stack) =
    Format.fprintf fmt "[%a]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt " ")
         Pp.Shared.val_type )
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
    | Num_type required, Num_type got -> match_num_type required got
    | Ref_type required, Ref_type got -> match_ref_type required got
    | Num_type _, Ref_type _ | Ref_type _, Num_type _ -> false

  let rec equal (s1 : stack) (s2 : stack) =
    match (s1, s2) with
    | [], [] -> true
    | [], _ :: _ | _ :: _, [] -> false
    | h1 :: t1, h2 :: t2 -> h1 = h2 && equal t1 t2

  let rec match_prefix required stack =
    match (required, stack) with
    | [], stack -> Some stack
    | _ :: _, [] -> None
    | required_head :: required_tail, stack_head :: stack_tail ->
      if not (match_types required_head stack_head) then None
      else match_prefix required_tail stack_tail

  let pop required stack =
    match match_prefix required stack with
    | None -> failwith "type mismatch"
    | Some stack -> stack

  let drop stack =
    match stack with [] -> failwith "type mismatch" | _ :: tl -> tl

  let push t stack = Continue (t @ stack)
end

let check (s1 : state) (s2 : state) =
  match (s1, s2) with
  | Stop, Stop -> Stop
  | Stop, Continue s | Continue s, Stop -> Continue s
  | Continue s1, Continue s2 ->
    if Stack.equal s1 s2 then Continue s1 else failwith "type mismatch"

let rec typecheck_instr (env : env) (stack : stack) (instr : instr) : state =
  match instr with
  | Nop -> Continue stack
  | Drop -> Continue (Stack.drop stack)
  | Return -> begin
    match Stack.match_prefix (List.rev env.result_type) stack with
    | Some _ -> Stop
    | None ->
      let msg =
        Format.asprintf "type mismatch: return %a" Stack.pp_error
          (env.result_type, stack)
      in
      failwith msg
  end
  | Unreachable -> Stop
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
    Stack.pop [ t ] stack |> Stack.push []
  | Global_get i -> Stack.push [ Env.global_get i env ] stack
  | Global_set i ->
    let t = Env.global_get i env in
    Stack.pop [ t ] stack |> Stack.push []
  | If_else (_id, block_type, e1, e2) ->
    let stack = Stack.pop [ i32 ] stack in
    let stack_e1 = typecheck_expr env stack e1 block_type in
    let stack_e2 = typecheck_expr env stack e2 block_type in
    check stack_e1 stack_e2
  | I_load (nn, _) | I_load16 (nn, _, _) | I_load8 (nn, _, _) ->
    Stack.pop [ i32 ] stack |> Stack.push [ itype nn ]
  | I64_load32 _ -> Stack.pop [ i32 ] stack |> Stack.push [ i64 ]
  | I_store8 (nn, _) | I_store16 (nn, _) | I_store (nn, _) ->
    Stack.pop [ itype nn; i32 ] stack |> Stack.push []
  | I64_store32 _ -> Stack.pop [ i64; i32 ] stack |> Stack.push []
  | F_load (nn, _) -> Stack.pop [ i32 ] stack |> Stack.push [ ftype nn ]
  | F_store (nn, _) -> Stack.pop [ ftype nn; i32 ] stack |> Stack.push []
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
    Stack.pop [ i32; i32; i32 ] stack |> Stack.push []
  | Block (_, bt, expr) | Loop (_, bt, expr) -> typecheck_expr env stack expr bt
  | Call _i -> failwith "TODO TYPECHECK CALL"
  | _ as i -> Format.kasprintf failwith "TODO %a" Pp.Simplified.instr i

and typecheck_expr (env : env) (stack : stack) (expr : expr)
    (block_type : func_type option) : state =
  let rec loop stack expr =
    match expr with
    | [] -> Continue stack
    | instr :: tail -> (
      Debug.log "BEFORE: %a@." Stack.pp stack;
      match typecheck_instr env stack instr with
      | Stop -> Stop
      | Continue stack ->
        Debug.log "AFTER: %a@." Stack.pp stack;
        loop stack tail )
  in
  ( match block_type with
  | None -> ()
  | Some (required, _result) ->
    if not (Stack.equal (List.rev_map snd required) stack) then
      failwith "type mismatch" );
  let state = loop stack expr in
  match (block_type, state) with
  | None, _ -> state
  | Some _, Stop -> Stop
  | Some (_params, required), Continue stack ->
    if not (Stack.equal (List.rev required) stack) then begin
      let msg =
        Format.asprintf "type mismatch: expr %a" Stack.pp_error (required, stack)
      in
      failwith msg
    end;
    Continue required

let typecheck_function globals (func : (S.func, func_type) S.runtime) =
  match func with
  | Imported _ -> ()
  | Local func -> (
    let params, result = func.type_f in
    let env =
      Env.make ~params ~locals:func.locals ~globals ~result_type:result
    in
    Debug.log "TYPECHECK function@.%a@." Pp.Simplified.func func;
    match typecheck_expr env [] func.body (Some ([], result)) with
    | Stop -> ()
    | Continue _ -> () )

let typecheck_module (module_ : Simplify.result) =
  S.Fields.iter
    (fun _index func -> typecheck_function module_.global func)
    module_.func
