(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module Value = Concrete_value
module Stack = Stack.Make [@inlined hint] (Value)

module Locals = PatriciaTree.MakeMap (struct
  include Int

  let to_int i = i
end)

module State = struct
  type t =
    { stack : Stack.t
    ; locals : Value.t Locals.t
    }

  let pp_map = Locals.pretty (fun fmt k v -> Fmt.pf fmt "%i->%a" k Value.pp v)

  let pp ppf (state : t) =
    let { stack; locals } = state in
    Fmt.pf ppf "@[<hov>σ:[%a];@;ρ:%a@]@." Stack.pp stack pp_map locals
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

module type DATA_STATE = sig
  type t = State.t option * Binary.instr option

  val eval_instr : State.t -> Binary.instr -> t
end

module Fixpoint (DS : DATA_STATE) = struct
  module JumpTarget = struct
    module JumpKey = struct
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

    let append (old : 'a t) (neww : 'a t) =
      Map.idempotent_union
        (fun _ ->
          (* we assume that there is no common keys *)
          assert false )
        old neww

    let decr jt =
      remove (I 0) jt |> to_list
      |> List.map (fun (k, v) -> (JumpKey.decr k, v))
      |> of_list
  end

  let join lhs rhs = match rhs with Some x -> Some x | None -> lhs

  let rec eval_expr :
       no_input:bool
    -> State.t
    -> Binary.instr Annotated.t list
    -> State.t option * State.t JumpTarget.t =
   fun ~no_input state expr ->
    let rec loop (state, jt) (expr : Binary.instr Annotated.t list) =
      match expr with
      | [] -> (Some state, jt)
      | instr :: instrs -> (
        let new_state, new_jt = eval_instr ~no_input state instr.raw in
        let new_jt = JumpTarget.append jt new_jt in
        match new_state with
        | None -> (None, new_jt)
        | Some s -> loop (s, new_jt) instrs )
    in
    loop (state, JumpTarget.empty) expr

  and eval_instr :
       no_input:bool
    -> State.t
    -> Binary.instr
    -> State.t option * State.t JumpTarget.t =
   fun ~no_input ({ stack; locals } as state) instr ->
    if no_input then Log.debug (fun m -> m "%a" State.pp state)
    else input_loop state;
    match instr with
    | Block (_str_opt, _bt, body) ->
      let res, mapping = eval_expr ~no_input state body.raw in
      let m_decr = JumpTarget.decr mapping in
      let s = join res (JumpTarget.find_opt (I 0) mapping) in
      (s, m_decr)
    | Loop (_str_opt, _bt, _block_instrs) -> assert false
    | If_else (str_opt, bt, expr_then, expr_else) ->
      let v, stack = Stack.pop state.stack in
      let to_exec = match v with I32 0l -> expr_else | _ -> expr_then in
      eval_instr ~no_input { state with stack } (Block (str_opt, bt, to_exec))
    | Br i -> (None, JumpTarget.of_list [ (I i, state) ])
    | Br_if id -> (
      let v, stack = Stack.pop stack in
      match v with
      | I32 0l -> (Some { stack; locals }, JumpTarget.empty)
      | _ ->
        (* br i *)
        (None, JumpTarget.of_list [ (I id, state) ]) )
    | Br_table (cases, default) ->
      let v, stack = Stack.pop_i32 stack in
      let matched =
        let v = Int32.to_int v in
        Array.find_index (fun case -> case = v) cases
      in
      let jt = match matched with Some i -> i | None -> default in
      (None, JumpTarget.of_list [ (I jt, State.{ stack; locals }) ])
    | Return -> (None, JumpTarget.of_list [ (Ret, state) ])
    | instr -> (
      let res = DS.eval_instr state instr in
      match res with
      | Some s, None -> (Some s, JumpTarget.empty)
      | None, Some instr -> eval_instr ~no_input state instr
      | Some _, Some _ -> (* should not happen *) assert false
      | None, None -> (* unreachable *) (None, JumpTarget.empty) )
end

module DataDenotConcrete_state : DATA_STATE = struct
  type t = State.t option * Binary.instr option

  let eval_i32 ({ stack; locals } : State.t) : Binary.i32_instr -> _ = function
    | Binary.Const i ->
      let stack = Stack.push_i32 stack i in
      State.{ stack; locals }
    | Add ->
      let stack = Stack.apply_i32_i32_i32 stack Value.I32.add in
      { stack; locals }
    | Sub ->
      let stack = Stack.apply_i32_i32_i32 stack Value.I32.sub in
      { stack; locals }
    | _ -> assert false

  let eval_i64 ({ stack; locals } : State.t) : Binary.i64_instr -> _ = function
    | Binary.Const i ->
      let stack = Stack.push_i64 stack i in
      State.{ stack; locals }
    | Add ->
      let stack = Stack.apply_i64_i64_i64 stack Value.I64.add in
      { stack; locals }
    | Sub ->
      let stack = Stack.apply_i64_i64_i64 stack Value.I64.sub in
      { stack; locals }
    | _ -> assert false

  let eval_local ({ stack; locals } : State.t) : Binary.local_instr -> _ =
    function
    | Get i -> (
      match Locals.find_opt i locals with
      | None -> Fmt.failwith "local.get: local %i is not set" i
      | Some v ->
        let stack = Stack.push stack v in
        State.{ stack; locals } )
    | Set i ->
      let v, stack = Stack.pop stack in
      let locals = Locals.add i v locals in
      { stack; locals }
    | Tee i ->
      let v, stack = Stack.pop stack in
      let locals = Locals.add i v locals in
      let stack = Stack.push stack v in
      { stack; locals }

  let eval_instr : State.t -> Binary.instr -> t =
   fun state instr ->
    match instr with
    | I32 instr ->
      let r = eval_i32 state instr in
      (Some r, None)
    | I64 instr ->
      let r = eval_i64 state instr in
      (Some r, None)
    | Unreachable ->
      (*TODO à gèrer proprement*)
      (None, None)
    | Local instr ->
      let state = eval_local state instr in
      (Some state, None)
    | Drop ->
      let _, stack = Stack.pop state.stack in
      (Some { state with stack }, None)
    | Nop -> (Some state, None)
    | ( If_else _ | Call _ | Block _ | Loop _ | Br _ | Br_if _ | Br_table _
      | Br_on_non_null _ | Br_on_null _ ) as instr ->
      (Some state, Some instr)
    | instr ->
      Fmt.failwith "DataState.eval_instr not implemented for %a"
        (Binary.pp_instr ~short:true)
        instr
end

module Concrete_fixpoint = Fixpoint (DataDenotConcrete_state)

let run ~no_input (m : Binary.Module.t) =
  let start = m.func.(option_get m.start) in
  let start = match start with Local a -> a | _ -> assert false in
  let state = State.{ stack = Stack.empty; locals = Locals.empty } in
  let res, _ = Concrete_fixpoint.eval_expr ~no_input state start.body.raw in
  (match res with Some _state -> Fmt.pr "Ok@." | None -> Fmt.pr "None@.");
  Ok ()
