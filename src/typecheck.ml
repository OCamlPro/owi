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
    ; result_type : result_type
    }

  let local_get i t =
    match Index.Map.find i t.locals with
    | exception Not_found -> failwith "Unbound local"
    | v -> v

  let make ~params ~locals ~result_type =
    let l = List.mapi (fun i v -> (I i, v)) (params @ locals) in
    let locals =
      List.fold_left
        (fun locals (i, (_, typ)) -> Index.Map.add i typ locals)
        Index.Map.empty l
    in
    { locals; result_type }
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
    | None ->
      (* TODO proper message *)
      failwith "Type error"
    | Some stack -> stack

  let drop stack =
    match stack with
    | [] ->
      (* TODO proper message *)
      failwith "Type error"
    | _ :: tl -> tl

  let push t stack = Continue (t @ stack)
end

let check (s1 : state) (s2 : state) =
  match (s1, s2) with
  | Stop, Stop -> Stop
  | Stop, Continue s | Continue s, Stop -> Continue s
  | Continue s1, Continue s2 ->
    if Stack.equal s1 s2 then Continue s1
    else (* TODO proper error *)
      failwith "Type error"

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
  | Local_get i -> Stack.push [ Env.local_get i env ] stack
  | If_else (_id, block_type, e1, e2) ->
    let stack = Stack.pop [ i32 ] stack in
    let stack_e1 = typecheck_expr env stack e1 block_type in
    let stack_e2 = typecheck_expr env stack e2 block_type in
    check stack_e1 stack_e2
  | _ -> failwith "TODO"

and typecheck_expr (env : env) (stack : stack) (expr : expr)
    (block_type : func_type option) : state =
  let rec loop stack expr =
    match expr with
    | [] -> Continue stack
    | instr :: tail -> (
      Debug.debugerr "BEFORE: %a@." Stack.pp stack;
      match typecheck_instr env stack instr with
      | Stop -> Stop
      | Continue stack ->
        Debug.debugerr "AFTER: %a@." Stack.pp stack;
        loop stack tail )
  in
  ( match block_type with
  | None -> ()
  | Some (required, _result) ->
    if not (Stack.equal (List.rev_map snd required) stack) then
      (* TODO proper error *)
      failwith "type error" );
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

let typecheck_function (func : (S.func, func_type) S.runtime) =
  match func with
  | Imported _ -> ()
  | Local func -> (
    let params, result = func.type_f in
    let env = Env.make ~params ~locals:func.locals ~result_type:result in
    Debug.debugerr "TYPECHECK function@.%a@." Pp.Simplified_bis.func func;
    match typecheck_expr env [] func.body (Some ([], result)) with
    | Stop -> ()
    | Continue _ -> () )

let typecheck_module (module_ : Simplify.result) =
  S.Fields.iter (fun _index func -> typecheck_function func) module_.func
