(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax
module Value = Concrete_value
module Stack = Stack.Make [@inlined hint] (Value)

type bf =
  | Block
  | Loop
  | Func

type l =
  { form : bf
  ; ty : Binary.func_type
  ; code : Binary.expr
  }

module Locals = PatriciaTree.MakeMap (struct
  include Int

  let to_int i = i
end)

module State = struct
  type t = Stack.t * Value.t Locals.t

  let pp_l fmt { form; ty; code } =
    Fmt.pf fmt "(%s %a %s)"
      (match form with Block -> "b" | Loop -> "l" | Func -> "f")
      Binary.pp_func_type ty
      (if List.length code > 0 then "I" else "[]")

  let pp_map = Locals.pretty (fun fmt k v -> Fmt.pf fmt "%i->%a" k Value.pp v)

  let pp ppf (state : t) =
    let stack, locals = state in
    Fmt.pf ppf "@[<hov>σ:[%a];@;ρ:%a@]@." Stack.pp stack pp_map locals
end

module JumpTarget = struct
  module JumpKey = struct
    (* TODO c'est mieux de définir le type nous même *)
    type t =
      | I of int
      | Ret

    let map : (int -> int) -> t -> t =
     fun f key -> match key with I i -> I (f i) | Ret -> Ret

    let decr = map Int.pred

    let to_int = function I i -> i | Ret -> -1
  end

  module Map = PatriciaTree.MakeMap (JumpKey)

  type 'a t = 'a Map.t

  let of_list = Map.of_list

  let to_list = Map.to_list

  let empty = Map.empty

  let find_opt = Map.find_opt

  let remove = Map.remove

  let add m k state =
    match Map.find_opt k m with
    | Some state_list -> Map.add k (state :: state_list) m
    | None -> Map.add k [ state ] m

  let append (old : 'a t) (neww : 'a t) =
    Map.mapi
      (fun k el ->
        match Map.find_opt k old with
        | Some el' -> List.append el el'
        | None -> el )
      neww

  let decr jt =
    remove (I 0) jt |> to_list
    |> List.map (fun (k, v) -> (JumpKey.decr k, v))
    |> of_list
end

(*=========================================================================*)

let rec input_loop state =
  match In_channel.input_line In_channel.stdin with
  | None | Some "n" | Some "" -> ()
  | Some "p" ->
    Log.debug (fun m -> m "%a" State.pp state);
    input_loop state
  | Some "q" -> exit 0
  | _ ->
    Fmt.pr "Input should be <cr>|n|p@.";
    input_loop state

let option_get = function Some x -> x | None -> assert false [@@inline]

(*=========================================================================*)

let eval_i32 (stack, locals) : Binary.i32_instr -> _ = function
  | Binary.Const i ->
    let stack = Stack.push_i32 stack i in
    (stack, locals)
  | Add ->
    let stack = Stack.apply_i32_i32_i32 stack Value.I32.add in
    (stack, locals)
  | Sub ->
    let stack = Stack.apply_i32_i32_i32 stack Value.I32.sub in
    (stack, locals)
  | _ -> assert false

let eval_i64 (stack, locals) : Binary.i64_instr -> _ = function
  | Binary.Const i ->
    let stack = Stack.push_i64 stack i in
    (stack, locals)
  | Add ->
    let stack = Stack.apply_i64_i64_i64 stack Value.I64.add in
    (stack, locals)
  | Sub ->
    let stack = Stack.apply_i64_i64_i64 stack Value.I64.sub in
    (stack, locals)
  | _ -> assert false

let eval_local (stack, locals) : Binary.local_instr -> _ = function
  | Get i -> (
    match Locals.find_opt i locals with
    | None -> Fmt.failwith "local.get: local %i is not set" i
    | Some v -> (v :: stack, locals) )
  | Set i ->
    let v, stack = Stack.pop stack in
    let locals = Locals.add i v locals in
    (stack, locals)
  | Tee i ->
    let v, stack = Stack.pop stack in
    let locals = Locals.add i v locals in
    let stack = Stack.push stack v in
    (stack, locals)

let join lhs rhs = match rhs with Some x -> Some x | None -> lhs

let rec eval_expr :
     State.t
  -> Binary.instr Annotated.t list
  -> State.t option * State.t JumpTarget.t =
 fun state expr ->
  let rec loop (state, jt) (expr : Binary.instr Annotated.t list) =
    match expr with
    | [] -> (Some state, jt)
    | instr :: instrs -> (
      let new_state, new_jt = eval_instr state instr.raw in
      let new_jt =
        JumpTarget.Map.idempotent_union (fun _ -> assert false) jt new_jt
      in
      match new_state with
      | None -> (None, new_jt)
      | Some s -> loop (s, new_jt) instrs )
  in
  loop (state, JumpTarget.empty) expr

and eval_instr :
  State.t -> Binary.instr -> State.t option * State.t JumpTarget.t =
 fun state instr ->
  let stack, locals = state in
  match instr with
  | Binary.I32 instr -> (Some (eval_i32 state instr), JumpTarget.empty)
  | Binary.I64 instr -> (Some (eval_i64 state instr), JumpTarget.empty)
  | Drop ->
    let _, stack = Stack.pop stack in
    (Some (stack, locals), JumpTarget.empty)
  | Unreachable -> (None, JumpTarget.empty)
  | Block (_str_opt, _bt, body) ->
    let res, mapping = eval_expr state body.raw in
    let m_decr = JumpTarget.decr mapping in
    let s = join res (JumpTarget.find_opt (I 0) mapping) in
    (s, m_decr)
  | Loop (_str_opt, _bt, _block_instrs) -> assert false
  | If_else (str_opt, bt, expr_then, expr_else) -> (
    let v, stack = Stack.pop stack in
    match v with
    | I32 0l -> eval_instr (stack, locals) (Block (str_opt, bt, expr_else))
    | _ -> eval_instr (stack, locals) (Block (str_opt, bt, expr_then)) )
  | Br i -> (None, JumpTarget.of_list [ (I i, state) ])
  | Br_if id -> (
    let v, stack = Stack.pop stack in
    match v with
    | I32 0l -> (Some (stack, locals), JumpTarget.empty)
    | _ ->
      (* br i *)
      (None, JumpTarget.of_list [ (I id, state) ]) )
  | Br_table (cases, default) ->
    let v, stack = Stack.pop stack in
    let v_int =
      match v with
      (* br_table works only on int32 *)
      | I32 i -> Int32.to_int i
      | _ -> assert false
    in
    let matched = Array.find_index (fun case -> case = v_int) cases in
    let jt = match matched with Some i -> i | None -> default in
    (None, JumpTarget.of_list [ (I jt, (stack, locals)) ])
  | Return -> (None, JumpTarget.of_list [ (Ret, state) ])
  | instr ->
    Fmt.failwith "TODO implement instr %a@\n"
      (Binary.pp_instr ~short:false)
      instr

let run (m : Binary.Module.t Result.t) =
  let+ m in
  let start = m.func.(option_get m.start) in
  let start = match start with Local a -> a | _ -> assert false in
  let state = (Stack.empty, Locals.empty) in
  let res, _ = eval_expr state start.body.raw in
  match res with Some _state -> Fmt.pr "Ok@." | None -> Fmt.pr "None@."
