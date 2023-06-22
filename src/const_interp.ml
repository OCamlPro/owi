open Types
open Simplified
open Syntax
module StringMap = Map.Make (String)
module StringSet = Set.Make (String)
module Env = Link_env

type env = Env.Build.t

let exec_ibinop stack nn (op : Const.ibinop) =
  match nn with
  | S32 ->
    let (n1, n2), stack = Stack.pop2_i32 stack in
    Stack.push_i32 stack
      (let open Int32 in
       match op with Add -> add n1 n2 | Sub -> sub n1 n2 | Mul -> mul n1 n2 )
  | S64 ->
    let (n1, n2), stack = Stack.pop2_i64 stack in
    Stack.push_i64 stack
      (let open Int64 in
       match op with Add -> add n1 n2 | Sub -> sub n1 n2 | Mul -> mul n1 n2 )

let exec_instr (env : env) (stack : Env.t' Stack.t) (instr : Const.instr) =
  match instr with
  | I32_const n -> ok @@ Stack.push_i32 stack n
  | I64_const n -> ok @@ Stack.push_i64 stack n
  | F32_const f -> ok @@ Stack.push_f32 stack f
  | F64_const f -> ok @@ Stack.push_f64 stack f
  | I_binop (nn, op) -> ok @@ exec_ibinop stack nn op
  | Ref_null t -> ok @@ Stack.push stack (Value.ref_null t)
  | Ref_func f ->
    let* f = Env.Build.get_func env f in
    let value = Value.Ref (Funcref (Some f)) in
    ok @@ Stack.push stack value
  | Global_get id ->
    let* g = Env.Build.get_const_global env id in
    ok @@ Stack.push stack g
  | Array_new _i ->
    let len, stack = Stack.pop_i32_to_int stack in
    (* TODO: check type of *default* *)
    let _default, stack = Stack.pop stack in
    let a = Array.init len (fun _i -> ()) in
    ok @@ Stack.push_array stack a
  | Array_new_default _i ->
    let len, stack = Stack.pop_i32_to_int stack in
    let a = Array.init len (fun _i -> ()) in
    ok @@ Stack.push_array stack a
  | I31_new ->
    (* TODO *)
    ok stack

let exec_expr env (e : Const.expr) : Env.t' Value.t Result.t =
  let* stack = list_fold_left (exec_instr env) Stack.empty e in
  match stack with
  | [] -> Error "type mismatch (const expr returning zero values)"
  | _ :: _ :: _ ->
    error_s "type mismatch (const expr returning more than one value)"
  | [ result ] -> Ok result
