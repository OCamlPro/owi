(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module Value = Concrete_value
module Stack = Stack.Make [@inlined hint] (Value)

let ( let* ) = Option.bind

module Locals = PatriciaTree.MakeMap (struct
  include Int

  let to_int i = i
end)

module State = struct
  type t =
    { stack : Stack.t
    ; locals : Value.t Locals.t
    ; env : Concrete_extern_func.extern_func Link_env.t
    ; envs : Concrete_extern_func.extern_func Link_env.t Dynarray.t
    ; func_rt : Binary.val_type list
    }

  let pp_map = Locals.pretty (fun fmt k v -> Fmt.pf fmt "%i->%a " k Value.pp v)

  let pp ppf ({ stack; locals; _ } : t) =
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

  and eval_func ~no_input (state : State.t) (func : Binary.Func.t) =
    Log.info (fun m ->
      m "calling func  : func %s" (Option.value func.id ~default:"anonymous") );
    let (Bt_raw ((None | Some _), (param_type, result_type))) = func.type_f in
    let args, caller_popped_stack =
      Stack.pop_n state.stack (List.length param_type)
    in
    let init_value : Binary.val_type -> Value.t = function
      | Num_type I32 -> I32 Value.I32.zero
      | Num_type I64 -> I64 Value.I64.zero
      | _ -> assert false
    in

    let locals =
      (* TODO check args position *)
      args @ List.map (fun (_str_opt, vt) -> init_value vt) func.locals
      |> List.mapi (fun i x -> (i, x))
      |> Locals.of_list
    in
    let fn_state = { state with stack = args; func_rt = result_type; locals } in
    Log.debug (fun m -> m "Func start state : %a" State.pp fn_state);
    (* TODO: handle mapping *)
    let func_end_state, _ = eval_expr ~no_input fn_state func.body.raw in
    Log.debug (fun m ->
      m "Func end state : %a@."
        (Fmt.option ~none:(Fmt.any "None") State.pp)
        func_end_state );
    (* We should probably copy state and join back the return values in the context here *)
    let* func_end_state in
    let stack =
      caller_popped_stack
      @ Stack.keep func_end_state.stack (List.length result_type)
    in
    Some { state with stack }

  and eval_instr :
       no_input:bool
    -> State.t
    -> Binary.instr
    -> State.t option * State.t JumpTarget.t =
   fun ~no_input ({ stack; locals; env; envs; _ } as state) instr ->
    Log.info (fun m -> m "stack         : [ %a ]" Stack.pp stack);
    Log.info (fun m -> m "locals        : [ %a ]" State.pp_map locals);
    Log.info (fun m ->
      m "running instr : %a" (Binary.pp_instr ~short:true) instr );
    if no_input then () else input_loop state;
    match instr with
    | Call idx ->
      let func = Link_env.get_func env idx in
      begin match func with
      | Wasm { func; idx } ->
        let env = Dynarray.get envs idx in
        let r = eval_func ~no_input { state with env } func in
        (r, JumpTarget.empty)
      | Extern { idx } -> (
        match idx with
        | 0 ->
          let stack = Stack.push_i32 stack 0l in
          (Some { state with stack }, JumpTarget.empty)
        | 1 ->
          let stack = Stack.push_i64 stack 0L in
          (Some { state with stack }, JumpTarget.empty)
        | _ ->
          Fmt.failwith "Some day we will have proper external function support"
        )
      end
    | Block (_str_opt, _bt, body) ->
      let state, mapping = eval_expr ~no_input state body.raw in
      let m_decr = JumpTarget.decr mapping in
      let state = join state (JumpTarget.find_opt (I 0) mapping) in
      let state = join state (JumpTarget.find_opt Ret mapping) in
      (state, m_decr)
    | Loop (_str_opt, bt, body) ->
      let to_keep =
        match bt with
        | Some (Bt_raw (_i, (params, _res))) -> List.length params
        | None -> 0
      in
      let rec fixpoint (state : State.t) =
        let initial_stack = state.stack in
        let loop_state =
          { state with stack = Stack.keep initial_stack to_keep }
        in
        let next_state, mapping = eval_expr ~no_input loop_state body.raw in
        (* TODO handle returns *)
        match JumpTarget.find_opt (I 0) mapping with
        | Some br_state ->
          let stack = Stack.keep br_state.stack to_keep in
          fixpoint { br_state with stack }
        | None ->
          let next_state =
            Option.bind next_state @@ fun next_state ->
            let stack = next_state.stack @ initial_stack in
            Some { next_state with stack }
          in
          (next_state, mapping)
      in
      fixpoint state
    | If_else (str_opt, bt, expr_then, expr_else) ->
      let v, stack = Stack.pop state.stack in
      let to_exec = match v with I32 0l -> expr_else | _ -> expr_then in
      eval_instr ~no_input { state with stack } (Block (str_opt, bt, to_exec))
    | Br i -> (None, JumpTarget.of_list [ (I i, state) ])
    | Br_if id -> (
      let v, stack = Stack.pop stack in
      match v with
      | I32 0l -> (Some { state with stack; locals }, JumpTarget.empty)
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
      (None, JumpTarget.of_list [ (I jt, { state with stack; locals }) ])
    | Return ->
      (* this should work without this but i'm keeping it otherwise func_rt is never used and it's a compile error :-) *)
      let stack = Stack.keep state.stack (List.length state.func_rt) in
      (None, JumpTarget.of_list [ (Ret, { state with stack }) ])
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

  let eval_i32 ({ stack; locals; _ } as state : State.t) : Binary.i32_instr -> _
      = function
    | Binary.Const i ->
      let stack = Stack.push_i32 stack i in
      { state with stack; locals }
    | Add ->
      let stack = Stack.apply_i32_i32_i32 stack Value.I32.add in
      { state with stack; locals }
    | Sub ->
      let stack = Stack.apply_i32_i32_i32 stack Value.I32.sub in
      { state with stack; locals }
    | Mul ->
      let stack = Stack.apply_i32_i32_i32 stack Value.I32.mul in
      { state with stack; locals }
    | Div S ->
      let stack = Stack.apply_i32_i32_i32 stack Value.I32.div in
      { state with stack; locals }
    | Div U ->
      let stack = Stack.apply_i32_i32_i32 stack Value.I32.unsigned_div in
      { state with stack; locals }
    | And ->
      let stack = Stack.apply_i32_i32_i32 stack Value.I32.logand in
      { state with stack; locals }
    | Or ->
      let stack = Stack.apply_i32_i32_i32 stack Value.I32.logor in
      { state with stack; locals }
    | Lt S ->
      let stack = Stack.apply_i32_i32_boolean stack Value.I32.lt in
      { state with stack; locals }
    | Lt U ->
      let stack = Stack.apply_i32_i32_boolean stack Value.I32.lt_u in
      { state with stack; locals }
    | Le S ->
      let stack = Stack.apply_i32_i32_boolean stack Value.I32.le in
      { state with stack; locals }
    | Le U ->
      let stack = Stack.apply_i32_i32_boolean stack Value.I32.le_u in
      { state with stack; locals }
    | _ -> assert false

  let eval_i64 ({ stack; locals; _ } as state : State.t) : Binary.i64_instr -> _
      = function
    | Binary.Const i ->
      let stack = Stack.push_i64 stack i in
      State.{ state with stack; locals }
    | Add ->
      let stack = Stack.apply_i64_i64_i64 stack Value.I64.add in
      { state with stack; locals }
    | Sub ->
      let stack = Stack.apply_i64_i64_i64 stack Value.I64.sub in
      { state with stack; locals }
    | Mul ->
      let stack = Stack.apply_i64_i64_i64 stack Value.I64.mul in
      { state with stack; locals }
    | Div S ->
      let stack = Stack.apply_i64_i64_i64 stack Value.I64.div in
      { state with stack; locals }
    | Div U ->
      let stack = Stack.apply_i64_i64_i64 stack Value.I64.unsigned_div in
      { state with stack; locals }
    | And ->
      let stack = Stack.apply_i64_i64_i64 stack Value.I64.logand in
      { state with stack; locals }
    | Or ->
      let stack = Stack.apply_i64_i64_i64 stack Value.I64.logor in
      { state with stack; locals }
    | Lt S ->
      let stack = Stack.apply_i64_i64_boolean stack Value.I64.lt in
      { state with stack; locals }
    | Lt U ->
      let stack = Stack.apply_i64_i64_boolean stack Value.I64.lt_u in
      { state with stack; locals }
    | Le S ->
      let stack = Stack.apply_i64_i64_boolean stack Value.I64.le in
      { state with stack; locals }
    | Le U ->
      let stack = Stack.apply_i64_i64_boolean stack Value.I64.le_u in
      { state with stack; locals }
    | _ -> assert false

  let eval_local ({ stack; locals; _ } as state : State.t) :
    Binary.local_instr -> _ = function
    | Get i -> (
      match Locals.find_opt i locals with
      | None -> Fmt.failwith "local.get: local %i is not set" i
      | Some v ->
        let stack = Stack.push stack v in
        { state with stack; locals } )
    | Set i ->
      let v, stack = Stack.pop stack in
      let locals = Locals.add i v locals in
      { state with stack; locals }
    | Tee i ->
      let v, stack = Stack.pop stack in
      let locals = Locals.add i v locals in
      let stack = Stack.push stack v in
      { state with stack; locals }

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
      (None, Some instr)
    | instr ->
      Fmt.failwith "DataState.eval_instr not implemented for %a"
        (Binary.pp_instr ~short:true)
        instr
end

module Concrete_fixpoint = Fixpoint (DataDenotConcrete_state)

let run ~no_input (link_state : Concrete_extern_func.extern_func Link.State.t)
  (m : Concrete_extern_func.extern_func Linked.Module.t) =
  let envs = Link.State.get_envs link_state in
  let state =
    State.
      { stack = Stack.empty
      ; locals = Locals.empty
      ; env = m.env
      ; envs
      ; func_rt = []
      }
  in
  List.iter
    begin fun (e : Binary.expr Annotated.t) ->
      let end_state, _ = Concrete_fixpoint.eval_expr ~no_input state e.raw in
      Log.info (fun m ->
        m "End Abstract_state : %a@."
          (Fmt.option ~none:(Fmt.any "none") State.pp)
          end_state )
    end
    m.to_run
