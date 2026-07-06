(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

include Extern.Func.Make (Abstract_value) (Abstract_monad) (Abstract_memory)
module Stack = Abstract_stack

let exec stack (f : Extern_type.extern_func) =
  let pop_arg (type ty) stack (arg : ty Extern_type.telt) : ty * Stack.t =
    match arg with
    | I32 -> Stack.pop_i32 stack
    | I64 -> Stack.pop_i64 stack
    | F32 -> Stack.pop_f32 stack
    | F64 -> Stack.pop_f64 stack
    | V128 -> Stack.pop_v128 stack
    | Externref _ety ->
      (* TODO: handle when we start thinking about refs *)
      assert false
  in
  let rec split_args : type f r.
    Stack.t -> (f, r) Extern_type.atype -> Stack.t * Stack.t =
   fun stack ty ->
    let[@local] split_one_arg args =
      let elt, stack = Stack.pop stack in
      let elts, stack = split_args stack args in
      (elt :: elts, stack)
    in
    match ty with
    | Mem (_, args) -> split_args stack args
    | Arg (_, args) -> split_one_arg args
    | UArg args -> split_args stack args
    | NArg (_, _, args) -> split_one_arg args
    | Res -> ([], stack)
  in
  let rec apply : type f r. Stack.t -> (f, r) Extern_type.atype -> f -> r =
   fun stack ty f ->
    match ty with
    | Mem (_memid, _args) ->
      (* TODO: Handle correctly *)
      assert false
    | Arg (arg, args) ->
      let v, stack = pop_arg stack arg in
      apply stack args (f v)
    | UArg args -> apply stack args (f ())
    | NArg (_, arg, args) ->
      let v, stack = pop_arg stack arg in
      apply stack args (f v)
    | Res -> f
  in
  let (Extern_func (Func (atype, rtype), func)) = f in
  let args, stack = split_args stack atype in
  let r = apply (List.rev args) atype func in
  let push_val (type ty) (arg : ty Extern_type.telt) (v : ty) stack =
    match arg with
    | I32 -> Stack.push_i32 stack v
    | I64 -> Stack.push_i64 stack v
    | F32 -> Stack.push_f32 stack v
    | F64 -> Stack.push_f64 stack v
    | V128 -> Stack.push_v128 stack v
    | Externref _ty ->
      (* TODO: handle when we start thinking about refs *)
      assert false
  in
  Abstract_monad.return @@
  match (rtype, r) with
  | R0, () -> stack
  | R1 t1, v1 -> push_val t1 v1 stack
  | R2 (t1, t2), (v1, v2) -> push_val t1 v1 stack |> push_val t2 v2
  | R3 (t1, t2, t3), (v1, v2, v3) ->
    push_val t1 v1 stack |> push_val t2 v2 |> push_val t3 v3
  | R4 (t1, t2, t3, t4), (v1, v2, v3, v4) ->
    push_val t1 v1 stack |> push_val t2 v2 |> push_val t3 v3 |> push_val t4 v4

type abs_extern_func =
  | Assume
  | I32_symbol
  | I64_symbol

exception ExternFuncException of abs_extern_func
