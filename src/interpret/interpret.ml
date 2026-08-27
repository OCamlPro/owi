(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

open Binary

module type Parameters = sig
  val use_ite_for_select : bool

  val throw_away_trap : bool

  val timeout : float option

  val timeout_instr : int option

  val abstract_invariant : Abstract_invariant.t
end

module Default_parameters = struct
  let use_ite_for_select = true

  let throw_away_trap = false

  let timeout = None

  let timeout_instr = None

  let abstract_invariant = Abstract_invariant.empty ()
end

module Make
    (Value : Value_intf.T)
    (Data : Data_intf.T)
    (Elem : Elem_intf.T with type reference := Value.t Value.Ref.t)
    (Choice :
      Choice_intf.S
        with type boolean := Value.boolean
         and type i32 := Value.i32
         and type value := Value.t)
    (Table : Table_intf.T with type reference := Value.t Value.Ref.t)
    (Memory :
      Memory_intf.T
        with type i32 := Value.i32
         and type i64 := Value.i64
         and type v128 := Value.v128
         and type 'a choice := 'a Choice.t)
    (Extern_func :
      Extern_intf.T
        with type i32 := Value.i32
         and type i64 := Value.i64
         and type f32 := Value.f32
         and type f64 := Value.f64
         and type v128 := Value.v128
         and type memory := Memory.t
         and type 'a m := 'a Choice.t)
    (Env :
      Env_intf.T
        with type extern_func := Extern_func.t
         and type memory := Memory.t
         and type value := Value.t
         and type table := Table.t
         and type elem := Elem.t
         and type data := Data.t)
    (Parameters : Parameters) =
struct
  open Value
  open Choice
  module Stack = Stack.Make [@inlined hint] (Value)

  let page_size = I64.of_int64 65_536L

  let pop_choice stack ~instr_counter_true ~instr_counter_false =
    let b, stack = Stack.pop_bool stack in
    let* b = select b ~instr_counter_false ~instr_counter_true in
    return (b, stack)

  (*
     `let> cond = e1 in e2` is:
       - `let cond = e1 in e2` in concrete mode
       - possibly two branches in symbolic mode, one where cond is true if e1 is SAT, and another one where cond is false if e2 is SAT
   *)
  let ( let> ) v f =
    (* TODO: can we use something better here? *)
    let instr_counter_true = None in
    let instr_counter_false = None in
    let* v = select v ~instr_counter_false ~instr_counter_true in
    f v

  (* If skip is true, it means we proved it can not happen (for instance via Abstract Interpretation. Thus we know it is impossible and the other branch is SAT, and we assume it without checking if it is SAT.
     In case of throw_away_trap, this is only going in the non-trapping branch, to avoid a useless solver call.
     Otherwise, this will properly try both branches (one trapping, one non-trapping).
     I.e. this can be read as `if v then trap else f` (or assume (not v) and f) in the non-trapping mode).
  *)
  let ( let>! ) (v, trap, instr_counter, skip) f =
    if skip then begin
      let cond = Boolean.not v in
      Log.debug (fun m ->
        m "skipped check on %a and assuming directly" Boolean.pp v );
      let* () = Choice.assume_no_check cond in
      f ()
    end
    else if Parameters.throw_away_trap then
      let cond = Boolean.not v in
      let* () = Choice.assume cond in
      f ()
    else
      (* TODO: can we do something better here? *)
      let instr_counter_true = instr_counter in
      let instr_counter_false = instr_counter in
      let* v = select v ~instr_counter_false ~instr_counter_true in
      if v then Choice.trap trap else f ()

  let default_gc_val = Env.default_gc_val

  module State = struct
    module Locals : sig
      type t = Value.t array

      val of_list : Value.t list -> t

      val get : t -> int -> Value.t

      val set : t -> int -> Value.t -> t
    end = struct
      type t = Value.t array

      let of_list = Array.of_list

      let get t i = Array.unsafe_get t i

      let set t i v =
        let locals = Array.copy t in
        Array.unsafe_set locals i v;
        locals
    end

    type block =
      { branch : expr Annotated.t
      ; branch_rt : Binary.result_type
      ; continue : expr Annotated.t
      ; continue_rt : Binary.result_type
      ; stack : Stack.t
      ; is_loop : Prelude.Bool.t
      }

    type block_stack = block list

    type t =
      { return_state : t option
      ; stack : Stack.t
      ; locals : Locals.t
          (* TODO: rename this PC, it stands for program counter but is easily confused with path condition... *)
      ; pc : expr Annotated.t
      ; block_stack : block_stack
      ; func_rt : result_type
      ; env : Env.t
      }

    let empty ~locals ~env () =
      { return_state = None
      ; stack = []
      ; locals = Locals.of_list locals
      ; pc = Annotated.dummy []
      ; block_stack = []
      ; func_rt = []
      ; env
      }

    type instr_result =
      | Return of t * Value.t list
      | Continue of t

    let return (state : t) =
      let args = Stack.keep state.stack (List.length state.func_rt) in
      match state.return_state with
      | None -> Return (state, args)
      | Some state' ->
        let stack = args @ state'.stack in
        Continue { state' with stack; env = state.env }

    let branch (state : t) n =
      let block_stack = Stack.drop_n state.block_stack n in
      match block_stack with
      | [] -> Choice.return (return state)
      | block :: block_stack_tl ->
        let block_stack =
          if block.is_loop then block_stack else block_stack_tl
        in
        let args = Stack.keep state.stack (List.length block.branch_rt) in
        let stack = args @ block.stack in
        Choice.return
          (Continue { state with block_stack; pc = block.branch; stack })

    let end_block (state : t) =
      match state.block_stack with
      | [] -> Choice.return (return state)
      | block :: block_stack ->
        let args = Stack.keep state.stack (List.length block.continue_rt) in
        let stack = args @ block.stack in
        Choice.return
          (Continue { state with block_stack; pc = block.continue; stack })
  end

  let mk_addr access_size ~(state : State.t) memid ~pos ~offset instr_counter =
    if Int64.(lt_u (sub 0xFFFF_FFFF_FFFF_FFFFL access_size) offset) then
      Choice.trap `Out_of_bounds_memory_access
    else
      let mem = Env.get_memory ~env:state.env memid in
      let pos = I64.extend_i32_u pos in
      let>! () =
        let limit = I64.of_int64 (Int64.add access_size offset) in
        let mem_size = Memory.size mem |> I64.extend_i32_u in
        ( Boolean.or_
            I64.(lt_u mem_size limit)
            I64.(lt_u (sub mem_size limit) pos)
        , `Out_of_bounds_memory_access
        , Some instr_counter
        , false )
      in
      let addr = I32.wrap_i64 I64.(add pos (I64.of_int64 offset)) in
      Choice.return (addr, mem)

  let mk_addr8 = mk_addr 1L

  let mk_addr16 = mk_addr 2L

  let mk_addr32 = mk_addr 4L

  let mk_addr64 = mk_addr 8L

  let mk_addr128 = mk_addr 16L

  let exec_i32_instr ~(state : State.t) instr_counter stack ~uuid :
    Binary.i32_instr -> State.t Choice.t =
   fun x ->
    Log.debug (fun m -> m "UUID IS: %d" uuid);
    x |> function
    | Const n ->
      let stack = Stack.push_concrete_i32 stack n in
      Choice.return { state with stack }
    | Clz ->
      let stack = Stack.apply_i32_i32 stack I32.clz in
      Choice.return { state with stack }
    | Ctz ->
      let stack = Stack.apply_i32_i32 stack I32.ctz in
      Choice.return { state with stack }
    | Popcnt ->
      let stack = Stack.apply_i32_i32 stack I32.popcnt in
      Choice.return { state with stack }
    | Add ->
      let stack = Stack.apply_i32_i32_i32 stack I32.add in
      Choice.return { state with stack }
    | Sub ->
      let stack = Stack.apply_i32_i32_i32 stack I32.sub in
      Choice.return { state with stack }
    | Mul ->
      let stack = Stack.apply_i32_i32_i32 stack I32.mul in
      Choice.return { state with stack }
    | Div_s ->
      let (n1, n2), stack = Stack.pop2_i32 stack in
      let>! () =
        let skip_divide_by_zero_check =
          not
          @@ Abstract_invariant.can_divide_by_zero Parameters.abstract_invariant
               ~uuid
        in
        ( I32.eqz n2
        , `Integer_divide_by_zero
        , (* TODO: get instr counter *) None
        , skip_divide_by_zero_check )
      in
      let>! () =
        ( Boolean.and_ (I32.eq n1 I32.min_int) @@ I32.eq n2 (I32.of_int (-1))
        , `Integer_overflow
        , (* TODO: get instr counter *) None
        , false )
      in
      let stack = Stack.push_i32 stack (I32.div n1 n2) in
      Choice.return { state with stack }
    | Div_u ->
      let (n1, n2), stack = Stack.pop2_i32 stack in
      let>! () =
        let skip_divide_by_zero_check =
          not
          @@ Abstract_invariant.can_divide_by_zero Parameters.abstract_invariant
               ~uuid
        in
        ( I32.eqz n2
        , `Integer_divide_by_zero
        , (* TODO: get instr counter *) None
        , skip_divide_by_zero_check )
      in
      let stack = Stack.push_i32 stack (I32.unsigned_div n1 n2) in
      Choice.return { state with stack }
    | Rem_s ->
      let (n1, n2), stack = Stack.pop2_i32 stack in
      let>! () =
        let skip_divide_by_zero_check =
          not
          @@ Abstract_invariant.can_divide_by_zero Parameters.abstract_invariant
               ~uuid
        in
        ( I32.eqz n2
        , `Integer_divide_by_zero
        , (* TODO: get instr counter *) None
        , skip_divide_by_zero_check )
      in
      let stack = Stack.push_i32 stack (I32.rem n1 n2) in
      Choice.return { state with stack }
    | Rem_u ->
      let (n1, n2), stack = Stack.pop2_i32 stack in
      let>! () =
        let skip_divide_by_zero_check =
          not
          @@ Abstract_invariant.can_divide_by_zero Parameters.abstract_invariant
               ~uuid
        in
        ( I32.eqz n2
        , `Integer_divide_by_zero
        , (* TODO: get instr counter *) None
        , skip_divide_by_zero_check )
      in
      let stack = Stack.push_i32 stack (I32.unsigned_rem n1 n2) in
      Choice.return { state with stack }
    | And ->
      let stack = Stack.apply_i32_i32_i32 stack I32.logand in
      Choice.return { state with stack }
    | Or ->
      let stack = Stack.apply_i32_i32_i32 stack I32.logor in
      Choice.return { state with stack }
    | Xor ->
      let stack = Stack.apply_i32_i32_i32 stack I32.logxor in
      Choice.return { state with stack }
    | Shl ->
      let stack = Stack.apply_i32_i32_i32 stack I32.shl in
      Choice.return { state with stack }
    | Shr_s ->
      let stack = Stack.apply_i32_i32_i32 stack I32.ashr in
      Choice.return { state with stack }
    | Shr_u ->
      let stack = Stack.apply_i32_i32_i32 stack I32.lshr in
      Choice.return { state with stack }
    | Rotl ->
      let stack = Stack.apply_i32_i32_i32 stack I32.rotate_left in
      Choice.return { state with stack }
    | Rotr ->
      let stack = Stack.apply_i32_i32_i32 stack I32.rotate_right in
      Choice.return { state with stack }
    | Eqz ->
      let stack = Stack.apply_i32_boolean stack I32.eqz in
      Choice.return { state with stack }
    | Eq ->
      let stack = Stack.apply_i32_i32_boolean stack I32.eq in
      Choice.return { state with stack }
    | Ne ->
      let stack = Stack.apply_i32_i32_boolean stack I32.ne in
      Choice.return { state with stack }
    | Lt_s ->
      let stack = Stack.apply_i32_i32_boolean stack I32.lt in
      Choice.return { state with stack }
    | Lt_u ->
      let stack = Stack.apply_i32_i32_boolean stack I32.lt_u in
      Choice.return { state with stack }
    | Gt_s ->
      let stack = Stack.apply_i32_i32_boolean stack (Fun.flip I32.lt) in
      Choice.return { state with stack }
    | Gt_u ->
      let stack = Stack.apply_i32_i32_boolean stack (Fun.flip I32.lt_u) in
      Choice.return { state with stack }
    | Le_s ->
      let stack = Stack.apply_i32_i32_boolean stack I32.le in
      Choice.return { state with stack }
    | Le_u ->
      let stack = Stack.apply_i32_i32_boolean stack I32.le_u in
      Choice.return { state with stack }
    | Ge_s ->
      let stack = Stack.apply_i32_i32_boolean stack (Fun.flip I32.le) in
      Choice.return { state with stack }
    | Ge_u ->
      let stack = Stack.apply_i32_i32_boolean stack (Fun.flip I32.le_u) in
      Choice.return { state with stack }
    | Trunc_f_s Text.S32 ->
      let f, stack = Stack.pop_f32 stack in
      let res = I32.trunc_f32_s f in
      begin match res with
      | Error t -> Choice.trap t
      | Ok res ->
        let stack = Stack.push_i32 stack res in
        Choice.return { state with stack }
      end
    | Trunc_f_u Text.S32 ->
      let f, stack = Stack.pop_f32 stack in
      let res = I32.trunc_f32_u f in
      begin match res with
      | Error t -> Choice.trap t
      | Ok res ->
        let stack = Stack.push_i32 stack res in
        Choice.return { state with stack }
      end
    | Trunc_f_s Text.S64 ->
      let f, stack = Stack.pop_f64 stack in
      let res = I32.trunc_f64_s f in
      begin match res with
      | Error t -> Choice.trap t
      | Ok res ->
        let stack = Stack.push_i32 stack res in
        Choice.return { state with stack }
      end
    | Trunc_f_u Text.S64 ->
      let f, stack = Stack.pop_f64 stack in
      let res = I32.trunc_f64_u f in
      begin match res with
      | Error t -> Choice.trap t
      | Ok res ->
        let stack = Stack.push_i32 stack res in
        Choice.return { state with stack }
      end
    | Trunc_sat_f_s Text.S32 ->
      let stack = Stack.apply_f32_i32 stack I32.trunc_sat_f32_s in
      Choice.return { state with stack }
    | Trunc_sat_f_u Text.S32 ->
      let stack = Stack.apply_f32_i32 stack I32.trunc_sat_f32_u in
      Choice.return { state with stack }
    | Trunc_sat_f_s Text.S64 ->
      let stack = Stack.apply_f64_i32 stack I32.trunc_sat_f64_s in
      Choice.return { state with stack }
    | Trunc_sat_f_u Text.S64 ->
      let stack = Stack.apply_f64_i32 stack I32.trunc_sat_f64_u in
      Choice.return { state with stack }
    | Extend8_s ->
      let stack = Stack.apply_i32_i32 stack (I32.extend_s 8) in
      Choice.return { state with stack }
    | Extend16_s ->
      let stack = Stack.apply_i32_i32 stack (I32.extend_s 16) in
      Choice.return { state with stack }
    | Wrap_i64 ->
      let stack = Stack.apply_i64_i32 stack I32.wrap_i64 in
      Choice.return { state with stack }
    | Reinterpret_f Text.S32 ->
      let stack = Stack.apply_f32_i32 stack I32.reinterpret_f32 in
      Choice.return { state with stack }
    | Reinterpret_f Text.S64 ->
      let stack =
        Stack.apply_f64_i32 stack
          (Fun.compose I32.reinterpret_f32 F32.demote_f64)
      in
      Choice.return { state with stack }
    | Load8_s (memid, { offset; _ }) ->
      let pos, stack = Stack.pop_i32 stack in
      let* addr, mem = mk_addr8 ~state memid ~pos ~offset instr_counter in
      let* res = Memory.load_8_s mem addr in
      let stack = Stack.push_i32 stack res in
      Choice.return { state with stack }
    | Load8_u (memid, { offset; _ }) ->
      let pos, stack = Stack.pop_i32 stack in
      let* addr, mem = mk_addr8 ~state memid ~pos ~offset instr_counter in
      let* res = Memory.load_8_u mem addr in
      let stack = Stack.push_i32 stack res in
      Choice.return { state with stack }
    | Load16_s (memid, { offset; _ }) ->
      let pos, stack = Stack.pop_i32 stack in
      let* addr, mem = mk_addr16 ~state memid ~pos ~offset instr_counter in
      let* res = Memory.load_16_s mem addr in
      let stack = Stack.push_i32 stack res in
      Choice.return { state with stack }
    | Load16_u (memid, { offset; _ }) ->
      let pos, stack = Stack.pop_i32 stack in
      let* addr, mem = mk_addr16 ~state memid ~pos ~offset instr_counter in
      let* res = Memory.load_16_u mem addr in
      let stack = Stack.push_i32 stack res in
      Choice.return { state with stack }
    | Load (memid, { offset; _ }) ->
      let pos, stack = Stack.pop_i32 stack in
      let* addr, mem = mk_addr32 ~state memid ~pos ~offset instr_counter in
      let* res = Memory.load_32 mem addr in
      let stack = Stack.push_i32 stack res in
      Choice.return { state with stack }
    | Store8 (memid, { offset; _ }) ->
      let n, stack = Stack.pop_i32 stack in
      let pos, stack = Stack.pop_i32 stack in
      let* addr, mem = mk_addr8 ~state memid ~pos ~offset instr_counter in
      let+ mem = Memory.store_8 mem ~addr n in
      let env = Env.set_memory ~env:state.env memid mem in
      { state with env; stack }
    | Store16 (memid, { offset; _ }) ->
      let n, stack = Stack.pop_i32 stack in
      let pos, stack = Stack.pop_i32 stack in
      let* addr, mem = mk_addr16 ~state memid ~pos ~offset instr_counter in
      let+ mem = Memory.store_16 mem ~addr n in
      let env = Env.set_memory ~env:state.env memid mem in
      { state with env; stack }
    | Store (memid, { offset; _ }) ->
      let n, stack = Stack.pop_i32 stack in
      let pos, stack = Stack.pop_i32 stack in
      let* addr, mem = mk_addr32 ~state memid ~pos ~offset instr_counter in
      let+ mem = Memory.store_32 mem ~addr n in
      let env = Env.set_memory ~env:state.env memid mem in
      { state with env; stack }

  let exec_i64_instr ~(state : State.t) instr_counter stack ~uuid :
    Binary.i64_instr -> State.t Choice.t = function
    | Const n ->
      let stack = Stack.push_concrete_i64 stack n in
      Choice.return { state with stack }
    | Clz ->
      let stack = Stack.apply_i64_i64 stack I64.clz in
      Choice.return { state with stack }
    | Ctz ->
      let stack = Stack.apply_i64_i64 stack I64.ctz in
      Choice.return { state with stack }
    | Popcnt ->
      let stack = Stack.apply_i64_i64 stack I64.popcnt in
      Choice.return { state with stack }
    | Add ->
      let stack = Stack.apply_i64_i64_i64 stack I64.add in
      Choice.return { state with stack }
    | Sub ->
      let stack = Stack.apply_i64_i64_i64 stack I64.sub in
      Choice.return { state with stack }
    | Mul ->
      let stack = Stack.apply_i64_i64_i64 stack I64.mul in
      Choice.return { state with stack }
    | Div_s ->
      let (n1, n2), stack = Stack.pop2_i64 stack in
      let>! () =
        let skip_divide_by_zero_check =
          not
          @@ Abstract_invariant.can_divide_by_zero Parameters.abstract_invariant
               ~uuid
        in
        ( I64.eqz n2
        , `Integer_divide_by_zero
        , (* TODO: get instr counter *) None
        , skip_divide_by_zero_check )
      in
      let>! () =
        ( Boolean.and_ (I64.eq n1 I64.min_int)
          @@ I64.eq n2 (I64.sub (I64.of_int 0) (I64.of_int 1))
        , `Integer_overflow
        , (* TODO: get instr counter *) None
        , false )
      in
      let stack = Stack.push_i64 stack (I64.div n1 n2) in
      Choice.return { state with stack }
    | Div_u ->
      let (n1, n2), stack = Stack.pop2_i64 stack in
      let>! () =
        let skip_divide_by_zero_check =
          not
          @@ Abstract_invariant.can_divide_by_zero Parameters.abstract_invariant
               ~uuid
        in
        ( I64.eqz n2
        , `Integer_divide_by_zero
        , (* TODO: get instr counter *) None
        , skip_divide_by_zero_check )
      in
      let stack = Stack.push_i64 stack (I64.unsigned_div n1 n2) in
      Choice.return { state with stack }
    | Rem_s ->
      let (n1, n2), stack = Stack.pop2_i64 stack in
      let>! () =
        let skip_divide_by_zero_check =
          not
          @@ Abstract_invariant.can_divide_by_zero Parameters.abstract_invariant
               ~uuid
        in
        ( I64.eqz n2
        , `Integer_divide_by_zero
        , (* TODO: get instr counter *) None
        , skip_divide_by_zero_check )
      in
      let stack = Stack.push_i64 stack (I64.rem n1 n2) in
      Choice.return { state with stack }
    | Rem_u ->
      let (n1, n2), stack = Stack.pop2_i64 stack in
      let>! () =
        let skip_divide_by_zero_check =
          not
          @@ Abstract_invariant.can_divide_by_zero Parameters.abstract_invariant
               ~uuid
        in
        ( I64.eqz n2
        , `Integer_divide_by_zero
        , (* TODO: get instr counter *) None
        , skip_divide_by_zero_check )
      in
      let stack = Stack.push_i64 stack (I64.unsigned_rem n1 n2) in
      Choice.return { state with stack }
    | And ->
      let stack = Stack.apply_i64_i64_i64 stack I64.logand in
      Choice.return { state with stack }
    | Or ->
      let stack = Stack.apply_i64_i64_i64 stack I64.logor in
      Choice.return { state with stack }
    | Xor ->
      let stack = Stack.apply_i64_i64_i64 stack I64.logxor in
      Choice.return { state with stack }
    | Shl ->
      let stack = Stack.apply_i64_i64_i64 stack I64.shl in
      Choice.return { state with stack }
    | Shr_s ->
      let stack = Stack.apply_i64_i64_i64 stack I64.ashr in
      Choice.return { state with stack }
    | Shr_u ->
      let stack = Stack.apply_i64_i64_i64 stack I64.lshr in
      Choice.return { state with stack }
    | Rotl ->
      let stack = Stack.apply_i64_i64_i64 stack I64.rotate_left in
      Choice.return { state with stack }
    | Rotr ->
      let stack = Stack.apply_i64_i64_i64 stack I64.rotate_right in
      Choice.return { state with stack }
    | Eqz ->
      let stack = Stack.apply_i64_boolean stack I64.eqz in
      Choice.return { state with stack }
    | Eq ->
      let stack = Stack.apply_i64_i64_boolean stack I64.eq in
      Choice.return { state with stack }
    | Ne ->
      let stack = Stack.apply_i64_i64_boolean stack I64.ne in
      Choice.return { state with stack }
    | Lt_s ->
      let stack = Stack.apply_i64_i64_boolean stack I64.lt in
      Choice.return { state with stack }
    | Lt_u ->
      let stack = Stack.apply_i64_i64_boolean stack I64.lt_u in
      Choice.return { state with stack }
    | Gt_s ->
      let stack = Stack.apply_i64_i64_boolean stack (Fun.flip I64.lt) in
      Choice.return { state with stack }
    | Gt_u ->
      let stack = Stack.apply_i64_i64_boolean stack (Fun.flip I64.lt_u) in
      Choice.return { state with stack }
    | Le_s ->
      let stack = Stack.apply_i64_i64_boolean stack I64.le in
      Choice.return { state with stack }
    | Le_u ->
      let stack = Stack.apply_i64_i64_boolean stack I64.le_u in
      Choice.return { state with stack }
    | Ge_s ->
      let stack = Stack.apply_i64_i64_boolean stack (Fun.flip I64.le) in
      Choice.return { state with stack }
    | Ge_u ->
      let stack = Stack.apply_i64_i64_boolean stack (Fun.flip I64.le_u) in
      Choice.return { state with stack }
    | Trunc_f_s Text.S32 ->
      let f, stack = Stack.pop_f32 stack in
      let res = I64.trunc_f32_s f in
      begin match res with
      | Error t -> Choice.trap t
      | Ok res ->
        let stack = Stack.push_i64 stack res in
        Choice.return { state with stack }
      end
    | Trunc_f_u Text.S32 ->
      let f, stack = Stack.pop_f32 stack in
      let res = I64.trunc_f32_u f in
      begin match res with
      | Error t -> Choice.trap t
      | Ok res ->
        let stack = Stack.push_i64 stack res in
        Choice.return { state with stack }
      end
    | Trunc_f_s Text.S64 ->
      let f, stack = Stack.pop_f64 stack in
      let res = I64.trunc_f64_s f in
      begin match res with
      | Error t -> Choice.trap t
      | Ok res ->
        let stack = Stack.push_i64 stack res in
        Choice.return { state with stack }
      end
    | Trunc_f_u Text.S64 ->
      let f, stack = Stack.pop_f64 stack in
      let res = I64.trunc_f64_u f in
      begin match res with
      | Error t -> Choice.trap t
      | Ok res ->
        let stack = Stack.push_i64 stack res in
        Choice.return { state with stack }
      end
    | Trunc_sat_f_s Text.S32 ->
      let stack = Stack.apply_f32_i64 stack I64.trunc_sat_f32_s in
      Choice.return { state with stack }
    | Trunc_sat_f_u Text.S32 ->
      let stack = Stack.apply_f32_i64 stack I64.trunc_sat_f32_u in
      Choice.return { state with stack }
    | Trunc_sat_f_s Text.S64 ->
      let stack = Stack.apply_f64_i64 stack I64.trunc_sat_f64_s in
      Choice.return { state with stack }
    | Trunc_sat_f_u Text.S64 ->
      let stack = Stack.apply_f64_i64 stack I64.trunc_sat_f64_u in
      Choice.return { state with stack }
    | Extend8_s ->
      let stack = Stack.apply_i64_i64 stack (I64.extend_s 8) in
      Choice.return { state with stack }
    | Extend16_s ->
      let stack = Stack.apply_i64_i64 stack (I64.extend_s 16) in
      Choice.return { state with stack }
    | Extend32_s ->
      let stack = Stack.apply_i64_i64 stack (I64.extend_s 32) in
      Choice.return { state with stack }
    | Extend_i32_s ->
      let stack = Stack.apply_i32_i64 stack I64.extend_i32_s in
      Choice.return { state with stack }
    | Extend_i32_u ->
      let stack = Stack.apply_i32_i64 stack I64.extend_i32_u in
      Choice.return { state with stack }
    | Reinterpret_f S32 ->
      let stack =
        Stack.apply_f32_i64 stack
          (Fun.compose I64.reinterpret_f64 F64.promote_f32)
      in
      Choice.return { state with stack }
    | Reinterpret_f S64 ->
      let stack = Stack.apply_f64_i64 stack I64.reinterpret_f64 in
      Choice.return { state with stack }
    | Load8_s (memid, { offset; _ }) ->
      let pos, stack = Stack.pop_i32 stack in
      let* addr, mem = mk_addr8 ~state memid ~pos ~offset instr_counter in
      let* res = Memory.load_8_s mem addr in
      let stack = Stack.push_i64 stack (I64.of_int32 res) in
      Choice.return { state with stack }
    | Load8_u (memid, { offset; _ }) ->
      let pos, stack = Stack.pop_i32 stack in
      let* addr, mem = mk_addr8 ~state memid ~pos ~offset instr_counter in
      let* res = Memory.load_8_u mem addr in
      let stack = Stack.push_i64 stack (I64.of_int32 res) in
      Choice.return { state with stack }
    | Load16_s (memid, { offset; _ }) ->
      let pos, stack = Stack.pop_i32 stack in
      let* addr, mem = mk_addr16 ~state memid ~pos ~offset instr_counter in
      let* res = Memory.load_16_s mem addr in
      let stack = Stack.push_i64 stack (I64.of_int32 res) in
      Choice.return { state with stack }
    | Load16_u (memid, { offset; _ }) ->
      let pos, stack = Stack.pop_i32 stack in
      let* addr, mem = mk_addr16 ~state memid ~pos ~offset instr_counter in
      let* res = Memory.load_16_u mem addr in
      let stack = Stack.push_i64 stack (I64.of_int32 res) in
      Choice.return { state with stack }
    | Load32_s (memid, { offset; _ }) ->
      let pos, stack = Stack.pop_i32 stack in
      let* addr, mem = mk_addr32 ~state memid ~pos ~offset instr_counter in
      let* res = Memory.load_32 mem addr in
      let stack = Stack.push_i64 stack (I64.of_int32 res) in
      Choice.return { state with stack }
    | Load32_u (memid, { offset; _ }) ->
      let pos, stack = Stack.pop_i32 stack in
      let* addr, mem = mk_addr32 ~state memid ~pos ~offset instr_counter in
      let* res = Memory.load_32 mem addr in
      let res =
        let a = I64.shl (I64.of_int 1) (I64.of_int 32) in
        let b = I64.sub a (I64.of_int 1) in
        I64.logand (I64.of_int32 res) b
      in
      let stack = Stack.push_i64 stack res in
      Choice.return { state with stack }
    | Load (memid, { offset; _ }) ->
      let pos, stack = Stack.pop_i32 stack in
      let* addr, mem = mk_addr64 ~state memid ~pos ~offset instr_counter in
      let* res = Memory.load_64 mem addr in
      let stack = Stack.push_i64 stack res in
      Choice.return { state with stack }
    | Store8 (memid, { offset; _ }) ->
      let n, stack = Stack.pop_i64 stack in
      let pos, stack = Stack.pop_i32 stack in
      let* addr, mem = mk_addr8 ~state memid ~pos ~offset instr_counter in
      let+ mem =
        let n = I64.to_int32 n in
        Memory.store_8 mem ~addr n
      in
      let env = Env.set_memory ~env:state.env memid mem in
      { state with stack; env }
    | Store16 (memid, { offset; _ }) ->
      let n, stack = Stack.pop_i64 stack in
      let pos, stack = Stack.pop_i32 stack in
      let* addr, mem = mk_addr16 ~state memid ~pos ~offset instr_counter in
      let+ mem =
        let n = I64.to_int32 n in
        Memory.store_16 mem ~addr n
      in
      let env = Env.set_memory ~env:state.env memid mem in
      { state with stack; env }
    | Store32 (memid, { offset; _ }) ->
      let n, stack = Stack.pop_i64 stack in
      let pos, stack = Stack.pop_i32 stack in
      let* addr, mem = mk_addr32 ~state memid ~pos ~offset instr_counter in
      let+ mem =
        let n = I64.to_int32 n in
        Memory.store_32 mem ~addr n
      in
      let env = Env.set_memory ~env:state.env memid mem in
      { state with stack; env }
    | Store (memid, { offset; _ }) ->
      let n, stack = Stack.pop_i64 stack in
      let pos, stack = Stack.pop_i32 stack in
      let* addr, mem = mk_addr64 ~state memid ~pos ~offset instr_counter in
      let+ mem = Memory.store_64 mem ~addr n in
      let env = Env.set_memory ~env:state.env memid mem in
      { state with stack; env }

  let exec_f32_instr ~(state : State.t) instr_counter stack :
    Binary.f32_instr -> State.t Choice.t = function
    | Const n ->
      let stack = Stack.push_concrete_f32 stack n in
      Choice.return { state with stack }
    | Abs ->
      let stack = Stack.apply_f32_f32 stack F32.abs in
      Choice.return { state with stack }
    | Neg ->
      let stack = Stack.apply_f32_f32 stack F32.neg in
      Choice.return { state with stack }
    | Sqrt ->
      let stack = Stack.apply_f32_f32 stack F32.sqrt in
      Choice.return { state with stack }
    | Ceil ->
      let stack = Stack.apply_f32_f32 stack F32.ceil in
      Choice.return { state with stack }
    | Floor ->
      let stack = Stack.apply_f32_f32 stack F32.floor in
      Choice.return { state with stack }
    | Trunc ->
      let stack = Stack.apply_f32_f32 stack F32.trunc in
      Choice.return { state with stack }
    | Nearest ->
      let stack = Stack.apply_f32_f32 stack F32.nearest in
      Choice.return { state with stack }
    | Add ->
      let stack = Stack.apply_f32_f32_f32 stack F32.add in
      Choice.return { state with stack }
    | Sub ->
      let stack = Stack.apply_f32_f32_f32 stack F32.sub in
      Choice.return { state with stack }
    | Mul ->
      let stack = Stack.apply_f32_f32_f32 stack F32.mul in
      Choice.return { state with stack }
    | Div ->
      let stack = Stack.apply_f32_f32_f32 stack F32.div in
      Choice.return { state with stack }
    | Min ->
      let stack = Stack.apply_f32_f32_f32 stack F32.min in
      Choice.return { state with stack }
    | Max ->
      let stack = Stack.apply_f32_f32_f32 stack F32.max in
      Choice.return { state with stack }
    | Copysign ->
      let stack = Stack.apply_f32_f32_f32 stack F32.copy_sign in
      Choice.return { state with stack }
    | Eq ->
      let stack = Stack.apply_f32_f32_boolean stack F32.eq in
      Choice.return { state with stack }
    | Ne ->
      let stack = Stack.apply_f32_f32_boolean stack F32.ne in
      Choice.return { state with stack }
    | Lt ->
      let stack = Stack.apply_f32_f32_boolean stack F32.lt in
      Choice.return { state with stack }
    | Gt ->
      let stack = Stack.apply_f32_f32_boolean stack (Fun.flip F32.lt) in
      Choice.return { state with stack }
    | Le ->
      let stack = Stack.apply_f32_f32_boolean stack F32.le in
      Choice.return { state with stack }
    | Ge ->
      let stack = Stack.apply_f32_f32_boolean stack (Fun.flip F32.le) in
      Choice.return { state with stack }
    | Demote_f64 ->
      let stack = Stack.apply_f64_f32 stack F32.demote_f64 in
      Choice.return { state with stack }
    | Convert_i_s S32 ->
      let stack = Stack.apply_i32_f32 stack F32.convert_i32_s in
      Choice.return { state with stack }
    | Convert_i_u S32 ->
      let stack = Stack.apply_i32_f32 stack F32.convert_i32_u in
      Choice.return { state with stack }
    | Convert_i_s S64 ->
      let stack = Stack.apply_i64_f32 stack F32.convert_i64_s in
      Choice.return { state with stack }
    | Convert_i_u S64 ->
      let stack = Stack.apply_i64_f32 stack F32.convert_i64_u in
      Choice.return { state with stack }
    | Reinterpret_i S32 ->
      let stack = Stack.apply_i32_f32 stack F32.reinterpret_i32 in
      Choice.return { state with stack }
    | Reinterpret_i S64 ->
      let stack =
        Stack.apply_i64_f32 stack (Fun.compose F32.reinterpret_i32 I64.to_int32)
      in
      Choice.return { state with stack }
    | Load (memid, { offset; _ }) ->
      let pos, stack = Stack.pop_i32 stack in
      let* addr, mem = mk_addr32 ~state memid ~pos ~offset instr_counter in
      let+ res = Memory.load_32 mem addr in
      let stack = Stack.push_f32 stack (F32.of_bits res) in
      { state with stack }
    | Store (memid, { offset; _ }) ->
      let n, stack = Stack.pop_f32 stack in
      let pos, stack = Stack.pop_i32 stack in
      let* addr, mem = mk_addr32 ~state memid ~pos ~offset instr_counter in
      let+ mem = Memory.store_32 mem ~addr (F32.to_bits n) in
      let env = Env.set_memory ~env:state.env memid mem in
      { state with stack; env }

  let exec_f64_instr ~(state : State.t) instr_counter stack :
    Binary.f64_instr -> State.t Choice.t = function
    | Const n ->
      let stack = Stack.push_concrete_f64 stack n in
      Choice.return { state with stack }
    | Abs ->
      let stack = Stack.apply_f64_f64 stack F64.abs in
      Choice.return { state with stack }
    | Neg ->
      let stack = Stack.apply_f64_f64 stack F64.neg in
      Choice.return { state with stack }
    | Sqrt ->
      let stack = Stack.apply_f64_f64 stack F64.sqrt in
      Choice.return { state with stack }
    | Ceil ->
      let stack = Stack.apply_f64_f64 stack F64.ceil in
      Choice.return { state with stack }
    | Floor ->
      let stack = Stack.apply_f64_f64 stack F64.floor in
      Choice.return { state with stack }
    | Trunc ->
      let stack = Stack.apply_f64_f64 stack F64.trunc in
      Choice.return { state with stack }
    | Nearest ->
      let stack = Stack.apply_f64_f64 stack F64.nearest in
      Choice.return { state with stack }
    | Add ->
      let stack = Stack.apply_f64_f64_f64 stack F64.add in
      Choice.return { state with stack }
    | Sub ->
      let stack = Stack.apply_f64_f64_f64 stack F64.sub in
      Choice.return { state with stack }
    | Mul ->
      let stack = Stack.apply_f64_f64_f64 stack F64.mul in
      Choice.return { state with stack }
    | Div ->
      let stack = Stack.apply_f64_f64_f64 stack F64.div in
      Choice.return { state with stack }
    | Min ->
      let stack = Stack.apply_f64_f64_f64 stack F64.min in
      Choice.return { state with stack }
    | Max ->
      let stack = Stack.apply_f64_f64_f64 stack F64.max in
      Choice.return { state with stack }
    | Copysign ->
      let stack = Stack.apply_f64_f64_f64 stack F64.copy_sign in
      Choice.return { state with stack }
    | Eq ->
      let stack = Stack.apply_f64_f64_boolean stack F64.eq in
      Choice.return { state with stack }
    | Ne ->
      let stack = Stack.apply_f64_f64_boolean stack F64.ne in
      Choice.return { state with stack }
    | Lt ->
      let stack = Stack.apply_f64_f64_boolean stack F64.lt in
      Choice.return { state with stack }
    | Gt ->
      let stack = Stack.apply_f64_f64_boolean stack (Fun.flip F64.lt) in
      Choice.return { state with stack }
    | Le ->
      let stack = Stack.apply_f64_f64_boolean stack F64.le in
      Choice.return { state with stack }
    | Ge ->
      let stack = Stack.apply_f64_f64_boolean stack (Fun.flip F64.le) in
      Choice.return { state with stack }
    | Promote_f32 ->
      let stack = Stack.apply_f32_f64 stack F64.promote_f32 in
      Choice.return { state with stack }
    | Convert_i_s S32 ->
      let stack = Stack.apply_i32_f64 stack F64.convert_i32_s in
      Choice.return { state with stack }
    | Convert_i_u S32 ->
      let stack = Stack.apply_i32_f64 stack F64.convert_i32_u in
      Choice.return { state with stack }
    | Convert_i_s S64 ->
      let stack = Stack.apply_i64_f64 stack F64.convert_i64_s in
      Choice.return { state with stack }
    | Convert_i_u S64 ->
      let stack = Stack.apply_i64_f64 stack F64.convert_i64_u in
      Choice.return { state with stack }
    | Reinterpret_i S32 ->
      let stack =
        Stack.apply_i32_f64 stack (Fun.compose F64.reinterpret_i64 I64.of_int32)
      in
      Choice.return { state with stack }
    | Reinterpret_i S64 ->
      let stack = Stack.apply_i64_f64 stack F64.reinterpret_i64 in
      Choice.return { state with stack }
    | Load (memid, { offset; _ }) ->
      let pos, stack = Stack.pop_i32 stack in
      (* I32.of_concrete 8l |> I64.extend_i32_u = I64.of_concrete 8L, right?  *)
      let* addr, mem = mk_addr64 ~state memid ~pos ~offset instr_counter in
      let+ res = Memory.load_64 mem addr in
      let stack = Stack.push_f64 stack (F64.of_bits res) in
      { state with stack }
    | Store (memid, { offset; _ }) ->
      let n, stack = Stack.pop_f64 stack in
      let pos, stack = Stack.pop_i32 stack in
      let* addr, mem = mk_addr64 ~state memid ~pos ~offset instr_counter in
      let+ mem = Memory.store_64 mem ~addr (F64.to_bits n) in
      let env = Env.set_memory ~env:state.env memid mem in
      { state with stack; env }

  let exec_v128_instr ~(state : State.t) instr_counter stack
    (i : Binary.v128_instr) : State.t Choice.t =
    match i with
    | Const n ->
      let stack = Stack.push_concrete_v128 stack n in
      Choice.return { state with stack }
    | Not ->
      let stack = Stack.apply_v128_v128 stack V128.lognot in
      Choice.return { state with stack }
    | And ->
      let stack = Stack.apply_v128_v128_v128 stack V128.logand in
      Choice.return { state with stack }
    | Andnot ->
      let stack = Stack.apply_v128_v128_v128 stack V128.andnot in
      Choice.return { state with stack }
    | Or ->
      let stack = Stack.apply_v128_v128_v128 stack V128.logor in
      Choice.return { state with stack }
    | Xor ->
      let stack = Stack.apply_v128_v128_v128 stack V128.logxor in
      Choice.return { state with stack }
    | Any_true ->
      let stack = Stack.apply_v128_boolean stack V128.any_true in
      Choice.return { state with stack }
    | Bitselect ->
      let stack = Stack.apply_v128_v128_v128_v128 stack V128.bitselect in
      Choice.return { state with stack }
    | Load32_lane (memid, { offset; _ }, lane) ->
      let vec, stack = Stack.pop_v128 stack in
      let pos, stack = Stack.pop_i32 stack in
      let* addr, mem = mk_addr32 ~state memid ~pos ~offset instr_counter in
      let+ x = Memory.load_32 mem addr in
      let vec = V128.replace_lane32 lane x vec in
      let stack = Stack.push_v128 stack vec in
      { state with stack }
    | Load64_zero (memid, { offset; _ }) ->
      let pos, stack = Stack.pop_i32 stack in
      let* addr, mem = mk_addr64 ~state memid ~pos ~offset instr_counter in
      let+ value = Memory.load_64 mem addr in
      let res = V128.of_i64x2 value I64.zero in
      let stack = Stack.push_v128 stack res in
      { state with stack }
    | Load (memid, { offset; _ }) ->
      let pos, stack = Stack.pop_i32 stack in
      let* addr, mem = mk_addr128 ~state memid ~pos ~offset instr_counter in
      let+ res = Memory.load_128 mem addr in
      let stack = Stack.push_v128 stack res in
      { state with stack }
    | Store (memid, { offset; _ }) ->
      let n, stack = Stack.pop_v128 stack in
      let pos, stack = Stack.pop_i32 stack in
      let* addr, mem = mk_addr128 ~state memid ~pos ~offset instr_counter in
      let+ mem = Memory.store_128 mem ~addr n in
      let env = Env.set_memory ~env:state.env memid mem in
      { state with stack; env }
    | Load16x4_s (memid, { offset; _ }) ->
      let pos, stack = Stack.pop_i32 stack in
      let* addr, mem = mk_addr64 ~state memid ~pos ~offset instr_counter in
      let* a = Memory.load_16_s mem addr in
      let* b = Memory.load_16_s mem I32.(add addr (of_int 2)) in
      let* c = Memory.load_16_s mem I32.(add addr (of_int 4)) in
      let+ d = Memory.load_16_s mem I32.(add addr (of_int 6)) in
      let res = V128.of_i32x4 a b c d in
      let stack = Stack.push_v128 stack res in
      { state with stack }
    | Load16x4_u (memid, { offset; _ }) ->
      let pos, stack = Stack.pop_i32 stack in
      let* addr, mem = mk_addr64 ~state memid ~pos ~offset instr_counter in
      let* a = Memory.load_16_u mem addr in
      let* b = Memory.load_16_u mem I32.(add addr (of_int 2)) in
      let* c = Memory.load_16_u mem I32.(add addr (of_int 4)) in
      let+ d = Memory.load_16_u mem I32.(add addr (of_int 6)) in
      let res = V128.of_i32x4 a b c d in
      let stack = Stack.push_v128 stack res in
      { state with stack }
    | Load8_splat (memid, { offset; _ }) ->
      let pos, stack = Stack.pop_i32 stack in
      let* addr, mem = mk_addr8 ~state memid ~pos ~offset instr_counter in
      let+ a = Memory.load_8_s mem addr in
      let a = I32.(logor a (shl a (of_int 8))) in
      let a = I32.(logor a (shl a (of_int 16))) in
      let res = V128.of_i32x4 a a a a in
      let stack = Stack.push_v128 stack res in
      { state with stack }
    | Load8_lane (memid, { offset; _ }, lane) ->
      let vec, stack = Stack.pop_v128 stack in
      let pos, stack = Stack.pop_i32 stack in
      let* addr, mem = mk_addr8 ~state memid ~pos ~offset instr_counter in
      let+ x = Memory.load_8_u mem addr in
      let vec = V128.replace_lane8 lane x vec in
      let stack = Stack.push_v128 stack vec in
      { state with stack }
    | Load8x8_s (memid, { offset; _ }) ->
      let pos, stack = Stack.pop_i32 stack in
      let* addr, mem = mk_addr64 ~state memid ~pos ~offset instr_counter in
      let* a0 = Memory.load_8_s mem addr in
      let* a1 = Memory.load_8_s mem I32.(add addr (of_int 1)) in
      let* a2 = Memory.load_8_s mem I32.(add addr (of_int 2)) in
      let* a3 = Memory.load_8_s mem I32.(add addr (of_int 3)) in
      let* a4 = Memory.load_8_s mem I32.(add addr (of_int 4)) in
      let* a5 = Memory.load_8_s mem I32.(add addr (of_int 5)) in
      let* a6 = Memory.load_8_s mem I32.(add addr (of_int 6)) in
      let+ a7 = Memory.load_8_s mem I32.(add addr (of_int 7)) in
      let pack16 lo hi =
        I32.(
          logor
            (logand lo (of_int 0xffff))
            (shl (logand hi (of_int 0xffff)) (of_int 16)) )
      in
      let res =
        V128.of_i32x4 (pack16 a0 a1) (pack16 a2 a3) (pack16 a4 a5)
          (pack16 a6 a7)
      in
      let stack = Stack.push_v128 stack res in
      { state with stack }
    | Load8x8_u (memid, { offset; _ }) ->
      let pos, stack = Stack.pop_i32 stack in
      let* addr, mem = mk_addr64 ~state memid ~pos ~offset instr_counter in
      let* a0 = Memory.load_8_u mem addr in
      let* a1 = Memory.load_8_u mem I32.(add addr (of_int 1)) in
      let* a2 = Memory.load_8_u mem I32.(add addr (of_int 2)) in
      let* a3 = Memory.load_8_u mem I32.(add addr (of_int 3)) in
      let* a4 = Memory.load_8_u mem I32.(add addr (of_int 4)) in
      let* a5 = Memory.load_8_u mem I32.(add addr (of_int 5)) in
      let* a6 = Memory.load_8_u mem I32.(add addr (of_int 6)) in
      let+ a7 = Memory.load_8_u mem I32.(add addr (of_int 7)) in
      let pack16 lo hi =
        I32.(
          logor
            (logand lo (of_int 0xffff))
            (shl (logand hi (of_int 0xffff)) (of_int 16)) )
      in
      let res =
        V128.of_i32x4 (pack16 a0 a1) (pack16 a2 a3) (pack16 a4 a5)
          (pack16 a6 a7)
      in
      let stack = Stack.push_v128 stack res in
      { state with stack }
    | Load16_splat (memid, { offset; _ }) ->
      let pos, stack = Stack.pop_i32 stack in
      let* addr, mem = mk_addr16 ~state memid ~pos ~offset instr_counter in
      let+ a = Memory.load_16_s mem addr in
      let a = I32.(logor (logand a (of_int 0xFFFF)) (shl a (of_int 16))) in
      let res = V128.of_i32x4 a a a a in
      let stack = Stack.push_v128 stack res in
      { state with stack }
    | Load16_lane (memid, { offset; _ }, lane) ->
      let vec, stack = Stack.pop_v128 stack in
      let pos, stack = Stack.pop_i32 stack in
      let* addr, mem = mk_addr16 ~state memid ~pos ~offset instr_counter in
      let+ x = Memory.load_16_s mem addr in
      let vec = V128.replace_lane16 lane x vec in
      let stack = Stack.push_v128 stack vec in
      { state with stack }
    | Load32_splat (memid, { offset; _ }) ->
      let pos, stack = Stack.pop_i32 stack in
      let* addr, mem = mk_addr32 ~state memid ~pos ~offset instr_counter in
      let+ a = Memory.load_32 mem addr in
      let res = V128.of_i32x4 a a a a in
      let stack = Stack.push_v128 stack res in
      { state with stack }
    | Load32_zero (memid, { offset; _ }) ->
      let pos, stack = Stack.pop_i32 stack in
      let* addr, mem = mk_addr32 ~state memid ~pos ~offset instr_counter in
      let+ a = Memory.load_32 mem addr in
      let res = V128.of_i32x4 a I32.zero I32.zero I32.zero in
      let stack = Stack.push_v128 stack res in
      { state with stack }
    | Load64_splat (memid, { offset; _ }) ->
      let pos, stack = Stack.pop_i32 stack in
      let* addr, mem = mk_addr64 ~state memid ~pos ~offset instr_counter in
      let+ a = Memory.load_64 mem addr in
      let res = V128.of_i64x2 a a in
      let stack = Stack.push_v128 stack res in
      { state with stack }
    | Load64_lane (memid, { offset; _ }, lane) ->
      let vec, stack = Stack.pop_v128 stack in
      let pos, stack = Stack.pop_i32 stack in
      let* addr, mem = mk_addr64 ~state memid ~pos ~offset instr_counter in
      let+ x = Memory.load_64 mem addr in
      let vec = V128.replace_lane64 lane x vec in
      let stack = Stack.push_v128 stack vec in
      { state with stack }
    | Store8_lane (memid, { offset; _ }, lane) ->
      let vec, stack = Stack.pop_v128 stack in
      let pos, stack = Stack.pop_i32 stack in
      let* addr, mem = mk_addr8 ~state memid ~pos ~offset instr_counter in
      let+ mem = Memory.store_8 mem ~addr (V128.extract_lane8 lane vec) in
      let env = Env.set_memory ~env:state.env memid mem in
      { state with stack; env }
    | Store64_lane (memid, { offset; _ }, lane) ->
      let vec, stack = Stack.pop_v128 stack in
      let pos, stack = Stack.pop_i32 stack in
      let* addr, mem = mk_addr64 ~state memid ~pos ~offset instr_counter in
      let+ mem = Memory.store_64 mem ~addr (V128.extract_lane64 lane vec) in
      let env = Env.set_memory ~env:state.env memid mem in
      { state with stack; env }
    | Store32_zero (memid, { offset; _ }) ->
      let vec, stack = Stack.pop_v128 stack in
      let pos, stack = Stack.pop_i32 stack in
      let* addr, mem = mk_addr32 ~state memid ~pos ~offset instr_counter in
      let a, _, _, _ = V128.to_i32x4 vec in
      let+ mem = Memory.store_32 mem ~addr a in
      let env = Env.set_memory ~env:state.env memid mem in
      { state with stack; env }
    | Store32_lane (memid, { offset; _ }, lane) ->
      let vec, stack = Stack.pop_v128 stack in
      let pos, stack = Stack.pop_i32 stack in
      let* addr, mem = mk_addr32 ~state memid ~pos ~offset instr_counter in
      let+ mem = Memory.store_32 mem ~addr (V128.extract_lane32 lane vec) in
      let env = Env.set_memory ~env:state.env memid mem in
      { state with stack; env }
    | Store16_lane (memid, { offset; _ }, lane) ->
      let vec, stack = Stack.pop_v128 stack in
      let pos, stack = Stack.pop_i32 stack in
      let* addr, mem = mk_addr16 ~state memid ~pos ~offset instr_counter in
      let+ mem = Memory.store_16 mem ~addr (V128.extract_lane16 lane vec) in
      let env = Env.set_memory ~env:state.env memid mem in
      { state with stack; env }
    | Load32x2_s (memid, { offset; _ }) ->
      let pos, stack = Stack.pop_i32 stack in
      let* addr, mem = mk_addr64 ~state memid ~pos ~offset instr_counter in
      let* a = Memory.load_32 mem addr in
      let+ b = Memory.load_32 mem I32.(add addr (of_int 4)) in
      let res = V128.of_i64x2 (I64.of_int32 a) (I64.of_int32 b) in
      let stack = Stack.push_v128 stack res in
      { state with stack }
    | Load32x2_u (memid, { offset; _ }) ->
      let pos, stack = Stack.pop_i32 stack in
      let* addr, mem = mk_addr64 ~state memid ~pos ~offset instr_counter in
      let* a = Memory.load_32 mem addr in
      let+ b = Memory.load_32 mem I32.(add addr (of_int 4)) in
      let res =
        V128.of_i64x2
          (I64.logand (I64.of_int32 a) (I64.of_int 0xffff_ffff))
          (I64.logand (I64.of_int32 b) (I64.of_int 0xffff_ffff))
      in
      let stack = Stack.push_v128 stack res in
      { state with stack }

  let exec_i8x16_instr stack = function
    | (Add : Text.i8x16_instr) ->
      Stack.apply_v128_v128_v128 stack V128.I8x16.add |> Choice.return
    | Sub -> Stack.apply_v128_v128_v128 stack V128.I8x16.sub |> Choice.return
    | Eq -> Stack.apply_v128_v128_v128 stack V128.I8x16.eq |> Choice.return
    | Ne -> Stack.apply_v128_v128_v128 stack V128.I8x16.ne |> Choice.return
    | Abs -> Stack.apply_v128_v128 stack V128.I8x16.abs |> Choice.return
    | Neg -> Stack.apply_v128_v128 stack V128.I8x16.neg |> Choice.return
    | Popcnt -> Stack.apply_v128_v128 stack V128.I8x16.popcnt |> Choice.return
    | All_true ->
      Stack.apply_v128_i32 stack V128.I8x16.all_true |> Choice.return
    | Bitmask -> Stack.apply_v128_i32 stack V128.I8x16.bitmask |> Choice.return
    | Swizzle ->
      Stack.apply_v128_v128_v128 stack V128.I8x16.swizzle |> Choice.return
    | Splat -> Stack.apply_i32_v128 stack V128.I8x16.splat |> Choice.return
    | Lt_s -> Stack.apply_v128_v128_v128 stack V128.I8x16.lt_s |> Choice.return
    | Lt_u -> Stack.apply_v128_v128_v128 stack V128.I8x16.lt_u |> Choice.return
    | Gt_s -> Stack.apply_v128_v128_v128 stack V128.I8x16.gt_s |> Choice.return
    | Gt_u -> Stack.apply_v128_v128_v128 stack V128.I8x16.gt_u |> Choice.return
    | Le_s -> Stack.apply_v128_v128_v128 stack V128.I8x16.le_s |> Choice.return
    | Le_u -> Stack.apply_v128_v128_v128 stack V128.I8x16.le_u |> Choice.return
    | Ge_s -> Stack.apply_v128_v128_v128 stack V128.I8x16.ge_s |> Choice.return
    | Ge_u -> Stack.apply_v128_v128_v128 stack V128.I8x16.ge_u |> Choice.return
    | Shuffle lanes ->
      Stack.apply_v128_v128_v128 stack (V128.I8x16.shuffle lanes)
      |> Choice.return
    | Shl -> Stack.apply_i32_v128_v128 stack V128.I8x16.shl |> Choice.return
    | Min_s ->
      Stack.apply_v128_v128_v128 stack V128.I8x16.min_s |> Choice.return
    | Extract_lane_s lane_index ->
      Stack.apply_v128_i32 stack (V128.I8x16.extract_lane_s lane_index)
      |> Choice.return
    | Extract_lane_u lane ->
      Stack.apply_v128_i32 stack (V128.I8x16.extract_lane_u lane)
      |> Choice.return
    | Add_sat_s ->
      Stack.apply_v128_v128_v128 stack V128.I8x16.add_sat_s |> Choice.return
    | Shr_s -> Stack.apply_i32_v128_v128 stack V128.I8x16.shr_s |> Choice.return
    | Shr_u -> Stack.apply_i32_v128_v128 stack V128.I8x16.shr_u |> Choice.return
    | Min_u ->
      Stack.apply_v128_v128_v128 stack V128.I8x16.min_u |> Choice.return
    | Add_sat_u ->
      Stack.apply_v128_v128_v128 stack V128.I8x16.add_sat_u |> Choice.return
    | Sub_sat_s ->
      Stack.apply_v128_v128_v128 stack V128.I8x16.sub_sat_s |> Choice.return
    | Sub_sat_u ->
      Stack.apply_v128_v128_v128 stack V128.I8x16.sub_sat_u |> Choice.return
    | Max_s ->
      Stack.apply_v128_v128_v128 stack V128.I8x16.max_s |> Choice.return
    | Max_u ->
      Stack.apply_v128_v128_v128 stack V128.I8x16.max_u |> Choice.return
    | Narrow_i16x8_s ->
      Stack.apply_v128_v128_v128 stack V128.I8x16.narrow_i16x8_s
      |> Choice.return
    | Narrow_i16x8_u ->
      Stack.apply_v128_v128_v128 stack V128.I8x16.narrow_i16x8_u
      |> Choice.return
    | Avgr_u ->
      Stack.apply_v128_v128_v128 stack V128.I8x16.avgr_u |> Choice.return
    | Replace_lane lane ->
      Stack.apply_i32_v128_v128 stack (V128.I8x16.replace_lane lane)
      |> Choice.return

  let exec_i16x8_instr stack = function
    | (Add : Text.i16x8_instr) ->
      Stack.apply_v128_v128_v128 stack V128.I16x8.add |> Choice.return
    | Sub -> Stack.apply_v128_v128_v128 stack V128.I16x8.sub |> Choice.return
    | Mul -> Stack.apply_v128_v128_v128 stack V128.I16x8.mul |> Choice.return
    | Eq -> Stack.apply_v128_v128_v128 stack V128.I16x8.eq |> Choice.return
    | Ne -> Stack.apply_v128_v128_v128 stack V128.I16x8.ne |> Choice.return
    | Splat -> Stack.apply_i32_v128 stack V128.I16x8.splat |> Choice.return
    | Lt_s -> Stack.apply_v128_v128_v128 stack V128.I16x8.lt_s |> Choice.return
    | Lt_u -> Stack.apply_v128_v128_v128 stack V128.I16x8.lt_u |> Choice.return
    | Gt_s -> Stack.apply_v128_v128_v128 stack V128.I16x8.gt_s |> Choice.return
    | Gt_u -> Stack.apply_v128_v128_v128 stack V128.I16x8.gt_u |> Choice.return
    | Le_s -> Stack.apply_v128_v128_v128 stack V128.I16x8.le_s |> Choice.return
    | Le_u -> Stack.apply_v128_v128_v128 stack V128.I16x8.le_u |> Choice.return
    | Ge_s -> Stack.apply_v128_v128_v128 stack V128.I16x8.ge_s |> Choice.return
    | Ge_u -> Stack.apply_v128_v128_v128 stack V128.I16x8.ge_u |> Choice.return
    | Extract_lane_s lane ->
      Stack.apply_v128_i32 stack (V128.I16x8.extract_lane_s lane)
      |> Choice.return
    | Extract_lane_u lane ->
      Stack.apply_v128_i32 stack (V128.I16x8.extract_lane_u lane)
      |> Choice.return
    | Q15mulr_sat_s ->
      Stack.apply_v128_v128_v128 stack V128.I16x8.q15mulr_sat_s |> Choice.return
    | Min_s ->
      Stack.apply_v128_v128_v128 stack V128.I16x8.min_s |> Choice.return
    | Min_u ->
      Stack.apply_v128_v128_v128 stack V128.I16x8.min_u |> Choice.return
    | Extmul_low_i8x16_s ->
      Stack.apply_v128_v128_v128 stack V128.I16x8.extmul_low_i8x16_s
      |> Choice.return
    | Extmul_low_i8x16_u ->
      Stack.apply_v128_v128_v128 stack V128.I16x8.extmul_low_i8x16_u
      |> Choice.return
    | Extmul_high_i8x16_s ->
      Stack.apply_v128_v128_v128 stack V128.I16x8.extmul_high_i8x16_s
      |> Choice.return
    | Extmul_high_i8x16_u ->
      Stack.apply_v128_v128_v128 stack V128.I16x8.extmul_high_i8x16_u
      |> Choice.return
    | Extend_low_i8x16_s ->
      Stack.apply_v128_v128 stack V128.I16x8.extend_low_i8x16_s |> Choice.return
    | Extend_low_i8x16_u ->
      Stack.apply_v128_v128 stack V128.I16x8.extend_low_i8x16_u |> Choice.return
    | Extend_high_i8x16_s ->
      Stack.apply_v128_v128 stack V128.I16x8.extend_high_i8x16_s
      |> Choice.return
    | Extend_high_i8x16_u ->
      Stack.apply_v128_v128 stack V128.I16x8.extend_high_i8x16_u
      |> Choice.return
    | Extadd_pairwise_i8x16_s ->
      Stack.apply_v128_v128 stack V128.I16x8.extadd_pairwise_i8x16_s
      |> Choice.return
    | Extadd_pairwise_i8x16_u ->
      Stack.apply_v128_v128 stack V128.I16x8.extadd_pairwise_i8x16_u
      |> Choice.return
    | Add_sat_s ->
      Stack.apply_v128_v128_v128 stack V128.I16x8.add_sat_s |> Choice.return
    | Add_sat_u ->
      Stack.apply_v128_v128_v128 stack V128.I16x8.add_sat_u |> Choice.return
    | Sub_sat_s ->
      Stack.apply_v128_v128_v128 stack V128.I16x8.sub_sat_s |> Choice.return
    | Sub_sat_u ->
      Stack.apply_v128_v128_v128 stack V128.I16x8.sub_sat_u |> Choice.return
    | Max_s ->
      Stack.apply_v128_v128_v128 stack V128.I16x8.max_s |> Choice.return
    | Max_u ->
      Stack.apply_v128_v128_v128 stack V128.I16x8.max_u |> Choice.return
    | Shl -> Stack.apply_i32_v128_v128 stack V128.I16x8.shl |> Choice.return
    | Neg -> Stack.apply_v128_v128 stack V128.I16x8.neg |> Choice.return
    | All_true ->
      Stack.apply_v128_i32 stack V128.I16x8.all_true |> Choice.return
    | Shr_s -> Stack.apply_i32_v128_v128 stack V128.I16x8.shr_s |> Choice.return
    | Shr_u -> Stack.apply_i32_v128_v128 stack V128.I16x8.shr_u |> Choice.return
    | Bitmask -> Stack.apply_v128_i32 stack V128.I16x8.bitmask |> Choice.return
    | Avgr_u ->
      Stack.apply_v128_v128_v128 stack V128.I16x8.avgr_u |> Choice.return
    | Abs -> Stack.apply_v128_v128 stack V128.I16x8.abs |> Choice.return
    | Narrow_i32x4_s ->
      Stack.apply_v128_v128_v128 stack V128.I16x8.narrow_i32x4_s
      |> Choice.return
    | Narrow_i32x4_u ->
      Stack.apply_v128_v128_v128 stack V128.I16x8.narrow_i32x4_u
      |> Choice.return
    | Replace_lane lane ->
      Stack.apply_i32_v128_v128 stack (V128.I16x8.replace_lane lane)
      |> Choice.return

  let exec_i32x4_instr stack : Text.i32x4_instr -> _ = function
    | Add -> Stack.apply_v128_v128_v128 stack V128.I32x4.add |> Choice.return
    | Sub -> Stack.apply_v128_v128_v128 stack V128.I32x4.sub |> Choice.return
    | Mul -> Stack.apply_v128_v128_v128 stack V128.I32x4.mul |> Choice.return
    | Shl -> Stack.apply_i32_v128_v128 stack V128.I32x4.shl |> Choice.return
    | Shr_s -> Stack.apply_i32_v128_v128 stack V128.I32x4.shr_s |> Choice.return
    | Shr_u -> Stack.apply_i32_v128_v128 stack V128.I32x4.shr_u |> Choice.return
    | Eq -> Stack.apply_v128_v128_v128 stack V128.I32x4.eq |> Choice.return
    | Ne -> Stack.apply_v128_v128_v128 stack V128.I32x4.ne |> Choice.return
    | Lt_s -> Stack.apply_v128_v128_v128 stack V128.I32x4.lt_s |> Choice.return
    | Lt_u -> Stack.apply_v128_v128_v128 stack V128.I32x4.lt_u |> Choice.return
    | Gt_s -> Stack.apply_v128_v128_v128 stack V128.I32x4.gt_s |> Choice.return
    | Gt_u -> Stack.apply_v128_v128_v128 stack V128.I32x4.gt_u |> Choice.return
    | Le_s -> Stack.apply_v128_v128_v128 stack V128.I32x4.le_s |> Choice.return
    | Le_u -> Stack.apply_v128_v128_v128 stack V128.I32x4.le_u |> Choice.return
    | Ge_s -> Stack.apply_v128_v128_v128 stack V128.I32x4.ge_s |> Choice.return
    | Ge_u -> Stack.apply_v128_v128_v128 stack V128.I32x4.ge_u |> Choice.return
    | Splat -> Stack.apply_i32_v128 stack V128.I32x4.splat |> Choice.return
    | Extract_lane lane ->
      Stack.apply_v128_i32 stack (V128.I32x4.extract_lane lane) |> Choice.return
    | Replace_lane lane ->
      Stack.apply_i32_v128_v128 stack (V128.I32x4.replace_lane lane)
      |> Choice.return
    | Extend_low_i16x8_s ->
      Stack.apply_v128_v128 stack V128.I32x4.extend_low_i16x8_s |> Choice.return
    | Extend_high_i16x8_s ->
      Stack.apply_v128_v128 stack V128.I32x4.extend_high_i16x8_s
      |> Choice.return
    | Extend_low_i16x8_u ->
      Stack.apply_v128_v128 stack V128.I32x4.extend_low_i16x8_u |> Choice.return
    | Extend_high_i16x8_u ->
      Stack.apply_v128_v128 stack V128.I32x4.extend_high_i16x8_u
      |> Choice.return
    | Trunc_sat_f64x2_s_zero ->
      Stack.apply_v128_v128 stack V128.I32x4.trunc_sat_f64x2_s_zero
      |> Choice.return
    | Trunc_sat_f64x2_u_zero ->
      Stack.apply_v128_v128 stack V128.I32x4.trunc_sat_f64x2_u_zero
      |> Choice.return
    | Trunc_sat_f32x4_s ->
      Stack.apply_v128_v128 stack V128.I32x4.trunc_sat_f32x4_s |> Choice.return
    | Trunc_sat_f32x4_u ->
      Stack.apply_v128_v128 stack V128.I32x4.trunc_sat_f32x4_u |> Choice.return
    | Min_s ->
      Stack.apply_v128_v128_v128 stack V128.I32x4.min_s |> Choice.return
    | Min_u ->
      Stack.apply_v128_v128_v128 stack V128.I32x4.min_u |> Choice.return
    | Extmul_low_i16x8_s ->
      Stack.apply_v128_v128_v128 stack V128.I32x4.extmul_low_i16x8_s
      |> Choice.return
    | Extmul_low_i16x8_u ->
      Stack.apply_v128_v128_v128 stack V128.I32x4.extmul_low_i16x8_u
      |> Choice.return
    | Extmul_high_i16x8_s ->
      Stack.apply_v128_v128_v128 stack V128.I32x4.extmul_high_i16x8_s
      |> Choice.return
    | Extmul_high_i16x8_u ->
      Stack.apply_v128_v128_v128 stack V128.I32x4.extmul_high_i16x8_u
      |> Choice.return
    | Extadd_pairwise_i16x8_s ->
      Stack.apply_v128_v128 stack V128.I32x4.extadd_pairwise_i16x8_s
      |> Choice.return
    | Extadd_pairwise_i16x8_u ->
      Stack.apply_v128_v128 stack V128.I32x4.extadd_pairwise_i16x8_u
      |> Choice.return
    | Dot_i16x8_s ->
      Stack.apply_v128_v128_v128 stack V128.I32x4.dot_i16x8_s |> Choice.return
    | Neg -> Stack.apply_v128_v128 stack V128.I32x4.neg |> Choice.return
    | Max_s ->
      Stack.apply_v128_v128_v128 stack V128.I32x4.max_s |> Choice.return
    | Max_u ->
      Stack.apply_v128_v128_v128 stack V128.I32x4.max_u |> Choice.return
    | Abs -> Stack.apply_v128_v128 stack V128.I32x4.abs |> Choice.return
    | All_true ->
      Stack.apply_v128_i32 stack V128.I32x4.all_true |> Choice.return
    | Bitmask -> Stack.apply_v128_i32 stack V128.I32x4.bitmask |> Choice.return

  let exec_i64x2_instr stack : Text.i64x2_instr -> _ = function
    | Add -> Stack.apply_v128_v128_v128 stack V128.I64x2.add |> Choice.return
    | Sub -> Stack.apply_v128_v128_v128 stack V128.I64x2.sub |> Choice.return
    | Mul -> Stack.apply_v128_v128_v128 stack V128.I64x2.mul |> Choice.return
    | Extend_low_i32x4_s ->
      Stack.apply_v128_v128 stack V128.I64x2.extend_low_i32x4_s |> Choice.return
    | Extend_low_i32x4_u ->
      Stack.apply_v128_v128 stack V128.I64x2.extend_low_i32x4_u |> Choice.return
    | Splat -> Stack.apply_i64_v128 stack V128.I64x2.splat |> Choice.return
    | Eq -> Stack.apply_v128_v128_v128 stack V128.I64x2.eq |> Choice.return
    | Ne -> Stack.apply_v128_v128_v128 stack V128.I64x2.ne |> Choice.return
    | Lt_s -> Stack.apply_v128_v128_v128 stack V128.I64x2.lt_s |> Choice.return
    | Gt_s -> Stack.apply_v128_v128_v128 stack V128.I64x2.gt_s |> Choice.return
    | Le_s -> Stack.apply_v128_v128_v128 stack V128.I64x2.le_s |> Choice.return
    | Ge_s -> Stack.apply_v128_v128_v128 stack V128.I64x2.ge_s |> Choice.return
    | Extend_high_i32x4_s ->
      Stack.apply_v128_v128 stack V128.I64x2.extend_high_i32x4_s
      |> Choice.return
    | Extend_high_i32x4_u ->
      Stack.apply_v128_v128 stack V128.I64x2.extend_high_i32x4_u
      |> Choice.return
    | Extmul_low_i32x4_s ->
      Stack.apply_v128_v128_v128 stack V128.I64x2.extmul_low_i32x4_s
      |> Choice.return
    | Extmul_low_i32x4_u ->
      Stack.apply_v128_v128_v128 stack V128.I64x2.extmul_low_i32x4_u
      |> Choice.return
    | Extmul_high_i32x4_s ->
      Stack.apply_v128_v128_v128 stack V128.I64x2.extmul_high_i32x4_s
      |> Choice.return
    | Extmul_high_i32x4_u ->
      Stack.apply_v128_v128_v128 stack V128.I64x2.extmul_high_i32x4_u
      |> Choice.return
    | Abs -> Stack.apply_v128_v128 stack V128.I64x2.abs |> Choice.return
    | Neg -> Stack.apply_v128_v128 stack V128.I64x2.neg |> Choice.return
    | All_true ->
      Stack.apply_v128_i32 stack V128.I64x2.all_true |> Choice.return
    | Bitmask -> Stack.apply_v128_i32 stack V128.I64x2.bitmask |> Choice.return
    | Shl -> Stack.apply_i32_v128_v128 stack V128.I64x2.shl |> Choice.return
    | Shr_s -> Stack.apply_i32_v128_v128 stack V128.I64x2.shr_s |> Choice.return
    | Shr_u -> Stack.apply_i32_v128_v128 stack V128.I64x2.shr_u |> Choice.return
    | Extract_lane lane ->
      Stack.apply_v128_i64 stack (V128.I64x2.extract_lane lane) |> Choice.return
    | Replace_lane lane ->
      Stack.apply_i64_v128_v128 stack (V128.I64x2.replace_lane lane)
      |> Choice.return

  let exec_f32x4_instr stack : Text.f32x4_instr -> _ = function
    | Abs -> Stack.apply_v128_v128 stack V128.F32x4.abs |> Choice.return
    | Pmin -> Stack.apply_v128_v128_v128 stack V128.F32x4.pmin |> Choice.return
    | Min -> Stack.apply_v128_v128_v128 stack V128.F32x4.min |> Choice.return
    | Eq -> Stack.apply_v128_v128_v128 stack V128.F32x4.eq |> Choice.return
    | Convert_i32x4_s ->
      Stack.apply_v128_v128 stack V128.F32x4.convert_i32x4_s |> Choice.return
    | Convert_i32x4_u ->
      Stack.apply_v128_v128 stack V128.F32x4.convert_i32x4_u |> Choice.return
    | Ceil -> Stack.apply_v128_v128 stack V128.F32x4.ceil |> Choice.return
    | Add -> Stack.apply_v128_v128_v128 stack V128.F32x4.add |> Choice.return
    | Max -> Stack.apply_v128_v128_v128 stack V128.F32x4.max |> Choice.return
    | Floor -> Stack.apply_v128_v128 stack V128.F32x4.floor |> Choice.return
    | Pmax -> Stack.apply_v128_v128_v128 stack V128.F32x4.pmax |> Choice.return
    | Ne -> Stack.apply_v128_v128_v128 stack V128.F32x4.ne |> Choice.return
    | Sub -> Stack.apply_v128_v128_v128 stack V128.F32x4.sub |> Choice.return
    | Trunc -> Stack.apply_v128_v128 stack V128.F32x4.trunc |> Choice.return
    | Lt -> Stack.apply_v128_v128_v128 stack V128.F32x4.lt |> Choice.return
    | Gt -> Stack.apply_v128_v128_v128 stack V128.F32x4.gt |> Choice.return
    | Le -> Stack.apply_v128_v128_v128 stack V128.F32x4.le |> Choice.return
    | Ge -> Stack.apply_v128_v128_v128 stack V128.F32x4.ge |> Choice.return
    | Mul -> Stack.apply_v128_v128_v128 stack V128.F32x4.mul |> Choice.return
    | Convert_low_i32x4_s ->
      Stack.apply_v128_v128 stack V128.F32x4.convert_low_i32x4_s
      |> Choice.return
    | Convert_low_i32x4_u ->
      Stack.apply_v128_v128 stack V128.F32x4.convert_low_i32x4_u
      |> Choice.return
    | Convert_high_i32x4_s ->
      Stack.apply_v128_v128 stack V128.F32x4.convert_high_i32x4_s
      |> Choice.return
    | Convert_high_i32x4_u ->
      Stack.apply_v128_v128 stack V128.F32x4.convert_high_i32x4_u
      |> Choice.return
    | Splat -> Stack.apply_f32_v128 stack V128.F32x4.splat |> Choice.return
    | Nearest -> Stack.apply_v128_v128 stack V128.F32x4.nearest |> Choice.return
    | Div -> Stack.apply_v128_v128_v128 stack V128.F32x4.div |> Choice.return
    | Neg -> Stack.apply_v128_v128 stack V128.F32x4.neg |> Choice.return
    | Sqrt -> Stack.apply_v128_v128 stack V128.F32x4.sqrt |> Choice.return
    | Demote_f64x2_zero ->
      Stack.apply_v128_v128 stack V128.F32x4.demote_f64x2_zero |> Choice.return
    | Extract_lane lane ->
      Stack.apply_v128_f32 stack (V128.F32x4.extract_lane lane) |> Choice.return
    | Replace_lane lane ->
      Stack.apply_f32_v128_v128 stack (V128.F32x4.replace_lane lane)
      |> Choice.return

  let exec_f64x2_instr stack : Text.f64x2_instr -> _ = function
    | Abs -> Stack.apply_v128_v128 stack V128.F64x2.abs |> Choice.return
    | Pmin -> Stack.apply_v128_v128_v128 stack V128.F64x2.pmin |> Choice.return
    | Min -> Stack.apply_v128_v128_v128 stack V128.F64x2.min |> Choice.return
    | Eq -> Stack.apply_v128_v128_v128 stack V128.F64x2.eq |> Choice.return
    | Ceil -> Stack.apply_v128_v128 stack V128.F64x2.ceil |> Choice.return
    | Add -> Stack.apply_v128_v128_v128 stack V128.F64x2.add |> Choice.return
    | Max -> Stack.apply_v128_v128_v128 stack V128.F64x2.max |> Choice.return
    | Floor -> Stack.apply_v128_v128 stack V128.F64x2.floor |> Choice.return
    | Pmax -> Stack.apply_v128_v128_v128 stack V128.F64x2.pmax |> Choice.return
    | Ne -> Stack.apply_v128_v128_v128 stack V128.F64x2.ne |> Choice.return
    | Sub -> Stack.apply_v128_v128_v128 stack V128.F64x2.sub |> Choice.return
    | Trunc -> Stack.apply_v128_v128 stack V128.F64x2.trunc |> Choice.return
    | Lt -> Stack.apply_v128_v128_v128 stack V128.F64x2.lt |> Choice.return
    | Gt -> Stack.apply_v128_v128_v128 stack V128.F64x2.gt |> Choice.return
    | Le -> Stack.apply_v128_v128_v128 stack V128.F64x2.le |> Choice.return
    | Ge -> Stack.apply_v128_v128_v128 stack V128.F64x2.ge |> Choice.return
    | Mul -> Stack.apply_v128_v128_v128 stack V128.F64x2.mul |> Choice.return
    | Convert_low_i32x4_s ->
      Stack.apply_v128_v128 stack V128.F64x2.convert_low_i32x4_s
      |> Choice.return
    | Convert_low_i32x4_u ->
      Stack.apply_v128_v128 stack V128.F64x2.convert_low_i32x4_u
      |> Choice.return
    | Convert_high_i32x4_s ->
      Stack.apply_v128_v128 stack V128.F64x2.convert_high_i32x4_s
      |> Choice.return
    | Convert_high_i32x4_u ->
      Stack.apply_v128_v128 stack V128.F64x2.convert_high_i32x4_u
      |> Choice.return
    | Nearest -> Stack.apply_v128_v128 stack V128.F64x2.nearest |> Choice.return
    | Div -> Stack.apply_v128_v128_v128 stack V128.F64x2.div |> Choice.return
    | Neg -> Stack.apply_v128_v128 stack V128.F64x2.neg |> Choice.return
    | Sqrt -> Stack.apply_v128_v128 stack V128.F64x2.sqrt |> Choice.return
    | Splat -> Stack.apply_f64_v128 stack V128.F64x2.splat |> Choice.return
    | Promote_low_f32x4 ->
      Stack.apply_v128_v128 stack V128.F64x2.promote_low_f32x4 |> Choice.return
    | Extract_lane lane ->
      Stack.apply_v128_f64 stack (V128.F64x2.extract_lane lane) |> Choice.return
    | Replace_lane lane ->
      Stack.apply_f64_v128_v128 stack (V128.F64x2.replace_lane lane)
      |> Choice.return

  let get_func_type (f : Extern_func.t Kind.func) =
    match f with
    | Wasm func -> func.type_f
    | Extern func -> (None, Extern_func.to_func_type func)

  let ref_matches_ref_type ~env (r : Value.t Ref.t) ((nullable, ht) : ref_type)
    : bool =
    let is_null = match nullable with Null -> true | No_null -> false in
    match r with
    | Ref.Func None when is_null -> (
      match ht with Func_ht | NoFunc_ht | TypeUse _ -> true | _ -> false )
    | Ref.Extern None when is_null -> (
      match ht with Extern_ht | NoExtern_ht -> true | _ -> false )
    | Ref.NullRef when is_null -> (
      match ht with
      | Any_ht | None_ht | Eq_ht | I31_ht | Struct_ht | Array_ht | TypeUse _ ->
        true
      | _ -> false )
    | Ref.NullExn when is_null -> (
      match ht with Exn_ht | NoExn_ht -> true | _ -> false )
    | Ref.Func (Some func) -> (
      match ht with
      | Func_ht -> true
      | TypeUse expected -> (
        let func = Env.get_func ~env func in
        let func_type = get_func_type func in
        match func_type with
        | Some got, _ ->
          let types = Env.get_types ~env in
          let type_groups = Env.get_type_groups ~env in
          Binary.is_subtype types type_groups types type_groups ~expected ~got
        | None, _ -> false )
      | _ -> false )
    | Ref.Extern (Some _) -> ( match ht with Extern_ht -> true | _ -> false )
    | Ref.I31 _ -> (
      match ht with I31_ht | Eq_ht | Any_ht -> true | _ -> false )
    | Ref.NullI31 when is_null -> (
      match ht with
      | I31_ht | Eq_ht | Any_ht | None_ht | Struct_ht | Array_ht | TypeUse _ ->
        true
      | _ -> false )
    | Ref.Struct s -> (
      match ht with
      | Struct_ht | Eq_ht | Any_ht -> true
      | TypeUse expected -> (
        match Ref.Struct.get_type s with
        | None -> false
        | Some got ->
          let types = Env.get_types ~env in
          let type_groups = Env.get_type_groups ~env in
          Binary.is_subtype types type_groups types type_groups ~expected ~got )
      | _ -> false )
    | Ref.Array a -> (
      match ht with
      | Array_ht | Eq_ht | Any_ht -> true
      | TypeUse expected -> (
        match Ref.Array.get_type a with
        | None -> false
        | Some got ->
          let types = Env.get_types ~env in
          let type_groups = Env.get_type_groups ~env in
          Binary.is_subtype types type_groups types type_groups ~expected ~got )
      | _ -> false )
    | Ref.ExternAsAny None when is_null -> (
      match ht with
      | Any_ht | None_ht | Eq_ht | I31_ht | Struct_ht | Array_ht | TypeUse _ ->
        true
      | _ -> false )
    | Ref.ExternAsAny (Some _) -> (
      match ht with Any_ht -> true | _ -> false )
    | Ref.AnyAsExtern _ -> ( match ht with Extern_ht -> true | _ -> false )
    | Func None | Extern None | NullExn | NullRef | NullI31 | ExternAsAny _ ->
      false

  let exec_ref_instr ({ stack; env; _ } : State.t) : Binary.ref_instr -> _ =
    function
    | Null t -> Stack.push_ref stack (Ref.null t) |> Choice.return
    | Is_null ->
      let r, stack = Stack.pop_as_ref stack in
      let is_null = Ref.is_null r |> Boolean.of_bool in
      Stack.push_bool stack is_null |> Choice.return
    | As_non_null ->
      let r, stack = Stack.pop_as_ref stack in
      if Ref.is_null r then Choice.trap (`Msg "null reference")
      else Stack.push_ref stack r |> Choice.return
    (* TODO: restrict to non_null refs *)
    | Func i -> Stack.push_ref stack (Ref.func i) |> Choice.return
    | Test rt ->
      let r, stack = Stack.pop_as_ref stack in
      let b = ref_matches_ref_type ~env r rt |> Boolean.of_bool in
      Stack.push_bool stack b |> Choice.return
    | Cast rt ->
      let r, stack = Stack.pop_as_ref stack in
      if ref_matches_ref_type ~env r rt then
        Stack.push_ref stack r |> Choice.return
      else Choice.trap `Cast_failure
    | Eq ->
      let r2, stack = Stack.pop_as_ref stack in
      let r1, stack = Stack.pop_as_ref stack in
      let eq = Ref.ref_eq r1 r2 in
      Stack.push_i32 stack (I32.of_int (if eq then 1 else 0)) |> Choice.return

  let exec_local_instr (state : State.t) locals stack :
    Binary.local_instr -> State.t = function
    | Get i ->
      let stack = Stack.push stack (State.Locals.get locals i) in
      { state with stack }
    | Set i ->
      let v, stack = Stack.pop stack in
      let locals = State.Locals.set locals i v in
      { state with locals; stack }
    | Tee i ->
      let v, stack = Stack.pop stack in
      let locals = State.Locals.set locals i v in
      let stack = Stack.push stack v in
      { state with locals; stack }

  let exec_global_instr ({ stack; env; _ } as state : State.t) :
    Binary.global_instr -> State.t = function
    | Get i ->
      let g = Env.get_global ~env i in
      let stack = Stack.push stack g in
      { state with stack }
    | Set i ->
      let v, stack = Stack.pop stack in
      let env = Env.set_global ~env i v in
      { state with env; stack }

  let exec_table_instr ({ stack; env; _ } as state : State.t) instr_counter :
    Binary.table_instr -> State.t Choice.t = function
    | Get tbl_i ->
      (* TODO: this should be rewritten without `select_i32` ! but it requires to change the type of `Table.get` *)
      let i, stack = Stack.pop_i32 stack in
      let* i = Choice.select_i32 i in
      let i = Int32.to_int i in
      let t = Env.get_table ~env tbl_i in
      let size = Table.size t in
      if i < 0 || i >= size then Choice.trap `Out_of_bounds_table_access
      else
        let v = Table.get t i in
        let stack = Stack.push stack (Ref v) in
        Choice.return { state with stack }
    | Set tbl_indice ->
      let v, stack = Stack.pop_as_ref stack in
      let indice, stack = Stack.pop_i32 stack in
      (* TODO: avoid the select_i32, it requires to change the type of `Table.set` *)
      let* indice = Choice.select_i32 indice in
      let indice = Int32.to_int indice in
      let t = Env.get_table ~env tbl_indice in
      if indice < 0 || indice >= Table.size t then
        Choice.trap `Out_of_bounds_table_access
      else begin
        let t = Table.set t indice v in
        let env = Env.set_table ~env tbl_indice t in
        Choice.return { state with stack; env }
      end
    | Size indice ->
      let t = Env.get_table ~env indice in
      let size = Table.size t in
      let stack = Stack.push_i32_of_int stack size in
      Choice.return { state with stack }
    | Grow indice ->
      let t = Env.get_table ~env indice in
      let size = I32.of_int @@ Table.size t in
      let delta, stack = Stack.pop_i32 stack in
      let new_size = I32.(size + delta) in
      let> allowed =
        Boolean.and_
          ( match Table.max_size t with
          | None -> Boolean.true_
          | Some max -> I32.le_u new_size (I32.of_int max) )
          (I32.le_u size new_size)
      in
      if not allowed then
        let stack = Stack.drop stack in
        let stack = Stack.push_i32_of_int stack (-1) in
        Choice.return { state with stack }
      else
        let new_element, stack = Stack.pop_as_ref stack in
        let+ new_size = Choice.select_i32 new_size in
        let t = Table.grow t new_size new_element in
        let env = Env.set_table ~env indice t in
        let stack = Stack.push_i32 stack size in
        { state with stack; env }
    | Fill indice ->
      let t = Env.get_table ~env indice in
      let len, stack = Stack.pop_i32 stack in
      let x, stack = Stack.pop_as_ref stack in
      let pos, stack = Stack.pop_i32 stack in
      let>! () =
        let pos = I64.extend_i32_u pos in
        let len = I64.extend_i32_u len in
        let size = I64.extend_i32_u (I32.of_int @@ Table.size t) in
        ( I64.lt_u size I64.(add pos len)
        , `Out_of_bounds_table_access
        , Some instr_counter
        , false )
      in
      let* pos = Choice.select_i32 pos in
      let+ len = Choice.select_i32 len in
      let t = Table.fill t pos len x in
      let env = Env.set_table ~env indice t in
      { state with stack; env }
    | Copy (ti_dst, ti_src) ->
      let t_src = Env.get_table ~env ti_src in
      let t_dst = Env.get_table ~env ti_dst in
      let len, stack = Stack.pop_i32 stack in
      let src, stack = Stack.pop_i32 stack in
      let dst, stack = Stack.pop_i32 stack in
      let>! () =
        let src_size = Table.size t_src |> I32.of_int |> I64.extend_i32_u in
        let dst_size = Table.size t_dst |> I32.of_int |> I64.extend_i32_u in
        let src = I64.extend_i32_u src in
        let dst = I64.extend_i32_u dst in
        let len = I64.extend_i32_u len in
        ( Boolean.or_
            (I64.lt_u src_size I64.(add src len))
            (I64.lt_u dst_size I64.(add dst len))
        , `Out_of_bounds_table_access
        , Some instr_counter
        , false )
      in
      let> len_eqz = I32.eqz len in
      if len_eqz then Choice.return { state with stack }
      else begin
        let* src = Choice.select_i32 src in
        let* dst = Choice.select_i32 dst in
        let+ len = Choice.select_i32 len in
        let t_dst = Table.copy ~t_src ~t_dst ~src ~dst ~len in
        let env = Env.set_table ~env ti_dst t_dst in
        { state with stack; env }
      end
    | Init (t_i, e_i) ->
      let t = Env.get_table ~env t_i in
      let elem = Env.get_elem ~env e_i in
      let len, stack = Stack.pop_i32 stack in
      let pos_x, stack = Stack.pop_i32 stack in
      let pos, stack = Stack.pop_i32 stack in
      let>! () =
        let pos = I64.extend_i32_u pos in
        let pos_x = I64.extend_i32_u pos_x in
        let len = I64.extend_i32_u len in
        let tbl_size = Table.size t |> I32.of_int |> I64.extend_i32_u in
        let elem_size = Elem.size elem |> I32.of_int |> I64.extend_i32_u in
        ( Boolean.or_
            I64.(lt_u elem_size (add len pos_x))
            I64.(lt_u tbl_size (add len pos))
        , `Out_of_bounds_table_access
        , Some instr_counter
        , false )
      in
      let* len = Choice.select_i32 len in
      let* pos_x = Choice.select_i32 pos_x in
      let+ pos = Choice.select_i32 pos in
      let len = Int32.to_int len in
      let pos_x = Int32.to_int pos_x in
      let pos = Int32.to_int pos in
      let rec loop i t =
        if i = len then t
        else
          let elt = Elem.get elem (pos_x + i) in
          let t = Table.set t (pos + i) elt in
          loop (i + 1) t
      in
      let t = loop 0 t in
      let env = Env.set_table ~env t_i t in
      { state with stack; env }

  let exec_elem_instr ({ env; _ } as state : State.t) : Binary.elem_instr -> _ =
    function
    | Drop i ->
      let elem = Env.get_elem ~env i in
      let elem = Elem.drop elem in
      let env = Env.set_elem ~env i elem in
      { state with env }

  let exec_memory_instr ~(state : State.t) instr_counter stack :
    Binary.memory_instr -> State.t Choice.t =
    let { State.env; _ } = state in
    function
    | Size memid ->
      let mem = Env.get_memory ~env memid in
      let len = Memory.size_in_pages mem in
      let stack = Stack.push_i32 stack len in
      let env = Env.set_memory ~env memid mem in
      Choice.return { state with stack; env }
    | Grow memid ->
      let mem = Env.get_memory ~env memid in
      let old_size = I64.of_int32 @@ Memory.size mem in
      let max_size = Memory.get_limit_max mem in
      let delta, stack = Stack.pop_i32 stack in
      let delta = I64.(of_int32 delta * page_size) in
      let new_size = I64.(old_size + delta) in
      let> too_big =
        Boolean.or_ I64.(le_u (page_size * page_size) new_size)
        @@
        match max_size with
        | Some max -> I64.(lt_u (of_int max * page_size) new_size)
        | None -> Boolean.false_
      in
      if too_big then
        let stack = Stack.push_i32 stack (I32.of_int ~-1) in
        Choice.return { state with stack }
      else begin
        let mem = Memory.grow mem I64.(to_int32 delta) in
        let res = I64.(to_int32 @@ (old_size / page_size)) in
        let stack = Stack.push_i32 stack res in
        let env = Env.set_memory ~env memid mem in
        Choice.return { state with env; stack }
      end
    | Fill memid ->
      let len, stack = Stack.pop_i32 stack in
      let c, stack = Stack.pop_i32 stack in
      let pos, stack = Stack.pop_i32 stack in
      let mem = Env.get_memory ~env memid in
      let>! () =
        let size = I64.extend_i32_u (Memory.size mem) in
        let len = I64.extend_i32_u len in
        let pos = I64.extend_i32_u pos in
        ( I64.lt_u size I64.(add pos len)
        , `Out_of_bounds_memory_access
        , Some instr_counter
        , false )
      in
      (* TODO: should we have something like select_i8 here? or rather, mask it correctly before calling select_i32? *)
      let* c = Choice.select_i32 c in
      let c =
        let c = Int32.to_int c in
        let c = Int.abs c mod 256 in
        Char.chr c
      in
      let+ mem = Memory.fill mem ~pos ~len c in
      let env = Env.set_memory ~env:state.env memid mem in
      { state with stack; env }
    | Copy (dstmemid, srcmemid) ->
      let len, stack = Stack.pop_i32 stack in
      let src_idx, stack = Stack.pop_i32 stack in
      let dst_idx, stack = Stack.pop_i32 stack in
      let srcmem = Env.get_memory ~env srcmemid in
      let dstmem = Env.get_memory ~env dstmemid in
      let>! () =
        let size1 = I64.extend_i32_u (Memory.size srcmem) in
        let size2 = I64.extend_i32_u (Memory.size dstmem) in
        let len = I64.extend_i32_u len in
        let src_idx = I64.extend_i32_u src_idx in
        let dst_idx = I64.extend_i32_u dst_idx in
        ( Boolean.or_
            (I64.lt_u size1 I64.(add src_idx len))
            (I64.lt_u size2 I64.(add dst_idx len))
        , `Out_of_bounds_memory_access
        , Some instr_counter
        , false )
      in
      let+ mem = Memory.blit ~src:srcmem ~src_idx ~dst:dstmem ~dst_idx ~len in
      let env = Env.set_memory ~env:state.env dstmemid mem in
      { state with stack; env }
    | Init (memid, dataid) ->
      let len, stack = Stack.pop_i32 stack in
      let src, stack = Stack.pop_i32 stack in
      let dst, stack = Stack.pop_i32 stack in
      let data = Env.get_data ~env dataid |> Data.to_string in
      let datasize =
        (* TODO: we can probably remove Data.size now that String.length is used ! *)
        match data with
        | None -> I64.zero
        | Some data -> String.length data |> I64.of_int
      in
      let mem = Env.get_memory ~env memid in
      let>! () =
        let memsize = I64.extend_i32_u (Memory.size mem) in
        let len = I64.extend_i32_u len in
        let src = I64.extend_i32_u src in
        let dst = I64.extend_i32_u dst in
        ( Boolean.or_
            (I64.lt_u memsize I64.(add dst len))
            (I64.lt_u datasize I64.(add src len))
        , `Out_of_bounds_memory_access
        , Some instr_counter
        , false )
      in
      let+ mem =
        Memory.blit_string mem (Option.value data ~default:"") ~src ~dst ~len
      in
      let env = Env.set_memory ~env:state.env memid mem in
      { state with stack; env }

  let exec_data_instr env : Binary.data_instr -> Env.t = function
    | Drop i ->
      let data = Env.get_data ~env i in
      let data = Data.drop data in
      Env.set_data ~env i data

  let init_local (_id, t) : Value.t =
    match t with
    | Binary.Num_type I32 -> I32 I32.zero
    | Num_type I64 -> I64 I64.zero
    | Num_type F32 -> F32 F32.zero
    | Num_type F64 -> F64 F64.zero
    | Num_type V128 -> V128 V128.zero
    | Ref_type (_null, rt) -> Ref (Ref.null rt)

  let rec split_args : type f r.
    Stack.t -> (f, r) Extern_func.atype -> Stack.t * Stack.t =
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
    | Res -> ([], stack)

  let pop_arg (type ty) stack (arg : ty Extern_func.telt) :
    (ty * Stack.t) Choice.t =
    match arg with
    | I32 -> Choice.return @@ Stack.pop_i32 stack
    | I64 -> Choice.return @@ Stack.pop_i64 stack
    | F32 -> Choice.return @@ Stack.pop_f32 stack
    | F64 -> Choice.return @@ Stack.pop_f64 stack
    | V128 -> Choice.return @@ Stack.pop_v128 stack
    | Externref ety -> (
      let v, stack = Stack.pop_as_ref stack in
      match Ref.get_extern v ety with
      | Ref_value v -> Choice.return @@ (v, stack)
      | Type_mismatch -> Choice.trap `Extern_call_arg_type_mismatch
      | Null -> Choice.trap `Extern_call_null_arg )

  let rec apply : type f r.
    Env.t -> Stack.t -> (f, r) Extern_func.atype -> f -> r Choice.t =
   fun env stack ty f ->
    match ty with
    | Mem (memid, args) ->
      let mem = Env.get_memory ~env memid in
      apply env stack args (f mem)
    | Arg (arg, args) ->
      let* v, stack = pop_arg stack arg in
      apply env stack args (f v)
    | UArg args -> apply env stack args (f ())
    | Res -> Choice.return f

  let push_val (type ty) (arg : ty Extern_func.telt) (v : ty) stack =
    match arg with
    | I32 -> Stack.push_i32 stack v
    | I64 -> Stack.push_i64 stack v
    | F32 -> Stack.push_f32 stack v
    | F64 -> Stack.push_f64 stack v
    | V128 -> Stack.push_v128 stack v
    | Externref ty ->
      let r = Ref.extern ty v in
      Stack.push_ref stack r

  let exec_extern_func ~(state : State.t) (f : Extern_func.t) =
    let (Extern_func.Extern_func (Func (atype, rtype), func)) = f in
    let args, stack = split_args state.stack atype in
    let* r = apply state.env (List.rev args) atype func in
    let+ r in
    match (rtype, r) with
    | R0, () -> stack
    | R1 t1, v1 -> push_val t1 v1 stack
    | R2 (t1, t2), (v1, v2) -> push_val t1 v1 stack |> push_val t2 v2
    | R3 (t1, t2, t3), (v1, v2, v3) ->
      push_val t1 v1 stack |> push_val t2 v2 |> push_val t3 v3
    | R4 (t1, t2, t3, t4), (v1, v2, v3, v4) ->
      push_val t1 v1 stack |> push_val t2 v2 |> push_val t3 v3 |> push_val t4 v4

  module Next_instruction : sig
    val with_instr_counter : instr Annotated.t Option.t -> int Option.t

    val exec_block :
      State.t -> is_loop:bool -> expr Annotated.t -> instr Annotated.t Option.t

    val continue : State.t -> instr Annotated.t Option.t

    val branch : State.t -> int -> instr Annotated.t Option.t
  end = struct
    let rec loop (state : State.t) : instr Annotated.t Option.t =
      match state.State.pc.Annotated.raw with
      | i :: _ -> Some i
      | [] -> (
        match state.State.block_stack with
        | [] -> None
        | block :: block_stack ->
          loop { state with block_stack; pc = block.State.continue } )

    let branch (state : State.t) n : instr Annotated.t Option.t =
      let block_stack = Stack.drop_n state.State.block_stack n in
      match block_stack with
      | [] -> None
      | block :: block_stack_tl ->
        let block_stack =
          if block.State.is_loop then block_stack else block_stack_tl
        in
        loop { state with block_stack; pc = block.State.branch; stack = [] }

    let continue (state : State.t) = loop state

    let exec_block state ~is_loop expr : instr Annotated.t Option.t =
      let branch = if is_loop then expr else state.State.pc in
      let block : State.block =
        { branch
        ; branch_rt = []
        ; continue = state.State.pc
        ; continue_rt = []
        ; stack = []
        ; is_loop
        }
      in
      loop { state with pc = expr; block_stack = block :: state.block_stack }

    let with_instr_counter = function
      | None -> None
      | Some i -> Some (Atomic.get i.Annotated.instr_counter)
  end

  let exec_block (state : State.t) ~is_loop (bt : block_type option) expr =
    let pt, rt =
      match bt with
      | None -> ([], [])
      | Some ((None | Some _), (pt, rt)) -> (List.map snd pt, rt)
    in
    let block : State.block =
      let branch_rt, branch = if is_loop then (pt, expr) else (rt, state.pc) in
      { branch
      ; branch_rt
      ; continue = state.pc
      ; continue_rt = rt
      ; stack = Stack.drop_n state.stack (List.length pt)
      ; is_loop
      }
    in
    Choice.return
      (State.Continue
         { state with pc = expr; block_stack = block :: state.block_stack } )

  let exec_func ~return (state : State.t) (func : Func.t) =
    Log.info (fun m ->
      m "calling func  : func %s" (Option.value func.id ~default:"anonymous") );
    let (None | Some _), (param_type, result_type) = func.type_f in
    let args, stack = Stack.pop_n state.stack (List.length param_type) in
    let return_state =
      if return then state.return_state else Some { state with stack }
    in
    let locals =
      State.Locals.of_list @@ List.rev args @ List.map init_local func.locals
    in
    State.
      { stack = []
      ; locals
      ; pc = func.body
      ; block_stack = []
      ; func_rt = result_type
      ; return_state
      ; env = state.env
      }

  (* TODO: remove env and use state.env ... do the same in the whole file *)
  let exec_vfunc ~return (state : State.t) (func : Extern_func.t Kind.func) =
    match func with
    | Wasm func -> Choice.return (State.Continue (exec_func ~return state func))
    | Extern func ->
      let+ stack = exec_extern_func ~state func in
      let state = { state with stack } in
      if return then State.return state else State.Continue state

  let call_ref ~return (state : State.t) _typ_i =
    let fun_ref, stack = Stack.pop_as_ref state.stack in
    let state = { state with stack } in
    match Ref.get_func fun_ref with
    | Null -> Choice.trap `Null_function_reference
    | Type_mismatch -> Choice.trap `Element_type_error
    | Ref_value func ->
      let func = Env.get_func ~env:state.env func in
      exec_vfunc ~return state func

  let call_indirect ~env ~return (state : State.t)
    (tbl_i, ((call_type_idx, typ_i) : block_type)) =
    let fun_i, stack = Stack.pop_i32 state.stack in
    let state = { state with stack } in
    let t = Env.get_table ~env tbl_i in
    let _null, ref_kind = Table.typ t in
    match ref_kind with
    | Func_ht | TypeUse _ ->
      let size = Table.size t in
      let>! () =
        ( I32.(le_u (I32.of_int size) fun_i)
        , `Undefined_element
        , (* TODO: get instr counter *) None
        , false )
      in
      let* fun_i = Choice.select_i32 fun_i in
      let fun_i = Int32.to_int fun_i in
      let f_ref = Table.get t fun_i in
      begin match Ref.get_func f_ref with
      | Null -> Choice.trap (`Uninitialized_element fun_i)
      | Type_mismatch -> Choice.trap `Element_type_error
      | Ref_value func ->
        let func = Env.get_func ~env func in
        let func_type = get_func_type func in
        let type_matches =
          match (call_type_idx, func_type) with
          | Some expected, (Some got, _) ->
            let func_types = Env.get_types ~env:state.env in
            let func_type_groups = Env.get_type_groups ~env:state.env in
            let call_types = Env.get_types ~env:state.env in
            let call_type_groups = Env.get_type_groups ~env:state.env in
            Binary.is_subtype func_types func_type_groups call_types
              call_type_groups ~got ~expected
          | Some _expected, (None, ft) -> Binary.func_type_eq ft typ_i
          | _, _ -> Binary.func_type_eq (snd func_type) typ_i
        in
        if not type_matches then Choice.trap `Indirect_call_type_mismatch
        else exec_vfunc ~return state func
      end
    | _ -> Choice.trap `Indirect_call_type_mismatch

  let array_data_elem_size = function
    | Binary.Pack_type I8 -> 1
    | Pack_type I16 -> 2
    | Val_type (Num_type I32) | Val_type (Num_type F32) -> 4
    | Val_type (Num_type I64) | Val_type (Num_type F64) -> 8
    | Val_type (Num_type V128) -> 16
    | Val_type (Ref_type _) -> assert false

  let get_u8 data i = Char.code (String.get data i)

  let read_le16 data off = get_u8 data off lor (get_u8 data (off + 1) lsl 8)

  let read_le32 data off =
    Int32.of_int (read_le16 data off lor (read_le16 data (off + 2) lsl 16))

  let read_le64 data off =
    (* of_int32 sign-extends, so set the upper bits to zero  *)
    let lo = Int64.logand (Int64.of_int32 (read_le32 data off)) 0xFFFFFFFFL in
    let hi = Int64.shift_left (Int64.of_int32 (read_le32 data (off + 4))) 32 in
    Int64.logor lo hi

  let read_data_gc_val (st : Binary.storage_type) data off : Value.t =
    match st with
    | Pack_type I8 -> I32 (I32.of_int (get_u8 data off))
    | Pack_type I16 -> I32 (I32.of_int (read_le16 data off))
    | Val_type (Num_type I32) -> I32 (I32.of_int32 @@ read_le32 data off)
    | Val_type (Num_type F32) ->
      F32 (F32.of_bits @@ I32.of_int32 (read_le32 data off))
    | Val_type (Num_type I64) -> I64 (I64.of_int64 @@ read_le64 data off)
    | Val_type (Num_type F64) ->
      F64 (F64.of_bits @@ I64.of_int64 (read_le64 data off))
    | Val_type (Num_type V128) ->
      Fmt.failwith "array data instruction: v128 element type not yet supported"
    | Val_type (Ref_type _) -> assert false

  let select_int x =
    let+ x = Choice.select_i32 x in
    Int32.to_int x

  let check_array_oob off n arr =
    (* TODO: all of this could be rewritten in a better way I think *)
    let* length = select_i32 (Ref.Array.length arr) in
    Choice.return (off < 0 || n < 0 || off + n > Concrete_i32.to_int length)

  let get_array_ref r =
    match r with
    | Ref.Array arr -> Choice.return arr
    | r when Ref.is_null r -> Choice.trap (`Msg "null array reference")
    | _ -> Choice.trap `Element_type_error

  let get_array_storage_type ({ env; _ } : State.t) arr_id instr_name =
    let types = Env.get_types ~env in
    match types.(arr_id).ct with
    | Def_array_t (_, st) -> st
    | _ -> Fmt.failwith "%s: type %d is not an array type" instr_name arr_id

  let exec_simple_instruction ({ stack; locals; env; _ } as state : State.t)
    instr_counter ~uuid : Binary.simple_instruction -> State.t Choice.t =
    let ret stack = Choice.return { state with stack } in
    function
    | I32 i -> exec_i32_instr ~state instr_counter stack i ~uuid
    | I64 i -> exec_i64_instr ~state instr_counter stack i ~uuid
    | F32 i -> exec_f32_instr ~state instr_counter stack i
    | F64 i -> exec_f64_instr ~state instr_counter stack i
    | V128 i -> exec_v128_instr ~state instr_counter stack i
    | I8x16 i ->
      let* stack = exec_i8x16_instr stack i in
      ret stack
    | I16x8 i ->
      let* stack = exec_i16x8_instr stack i in
      ret stack
    | I32x4 i ->
      let* stack = exec_i32x4_instr stack i in
      ret stack
    | I64x2 i ->
      let* stack = exec_i64x2_instr stack i in
      ret stack
    | F32x4 i ->
      let* stack = exec_f32x4_instr stack i in
      ret stack
    | F64x2 i ->
      let* stack = exec_f64x2_instr stack i in
      ret stack
    | Ref i ->
      let* stack = exec_ref_instr state i in
      ret stack
    | Local i ->
      let state = exec_local_instr state locals stack i in
      Choice.return state
    | Global i ->
      let state = exec_global_instr state i in
      Choice.return state
    | Table i ->
      let* state = exec_table_instr state instr_counter i in
      Choice.return state
    | Elem i ->
      let state = exec_elem_instr state i in
      Choice.return state
    | Memory i -> exec_memory_instr ~state instr_counter stack i
    | Data i ->
      let env = exec_data_instr env i in
      Choice.return { state with env }
    | Nop -> Choice.return state
    | Unreachable -> Choice.trap `Unreachable
    | Drop -> ret @@ Stack.drop stack
    | Select _t ->
      if Parameters.use_ite_for_select then begin
        let b, stack = Stack.pop_bool stack in
        let o2, stack = Stack.pop stack in
        let o1, stack = Stack.pop stack in
        let* res = Choice.ite b ~if_true:o1 ~if_false:o2 in
        ret @@ Stack.push stack res
      end
      else begin
        let instr_counter_true =
          Next_instruction.continue state |> Next_instruction.with_instr_counter
        in
        let instr_counter_false = instr_counter_true in
        let* b, stack =
          pop_choice stack ~instr_counter_true ~instr_counter_false
        in
        let o2, stack = Stack.pop stack in
        let o1, stack = Stack.pop stack in
        ret @@ Stack.push stack (if b then o1 else o2)
      end
    | I31 Ref ->
      let n, stack = Stack.pop_i32 state.stack in
      let state = { state with stack } in
      ret @@ Stack.push_ref state.stack (Ref.make_i31 n)
    | I31 Get_u ->
      let r, stack = Stack.pop_as_ref state.stack in
      let state = { state with stack } in
      begin match Ref.get_i31 r with
      | Null -> Choice.trap `Null_i31_reference
      | Type_mismatch -> Choice.trap `Element_type_error
      | Ref_value n ->
        let n31bits = I32.logand n (I32.of_int32 0x7FFF_FFFFl) in
        ret @@ Stack.push_i32 state.stack n31bits
      end
    | I31 Get_s ->
      let r, stack = Stack.pop_as_ref state.stack in
      let state = { state with stack } in
      begin match Ref.get_i31 r with
      | Null -> Choice.trap `Null_i31_reference
      | Type_mismatch -> Choice.trap `Element_type_error
      | Ref_value n ->
        (* sign-extend 31-bit value to 32 bits with bit 30 as the sign bit *)
        let n31bits = I32.logand n (I32.of_int32 0x7FFF_FFFFl) in
        let* sign_extended =
          let sign =
            I32.ne (I32.logand n31bits (I32.of_int32 0x4000_0000l)) I32.zero
          in
          Choice.ite sign
            ~if_true:
              (I32
                 (I32.logor n31bits (I32.of_int32 @@ Int32.lognot 0x7FFF_FFFFl))
              )
            ~if_false:(I32 n31bits)
        in
        ret @@ Stack.push state.stack sign_extended
      end
    | Struct (New id) ->
      let types = Env.get_types ~env in
      let fields =
        match types.(id).ct with
        | Def_struct_t fl -> fl
        | _ -> Fmt.failwith "struct.new: type %d is not a struct type" id
      in
      let n = List.length fields in
      let top_n, stack = Stack.pop_n state.stack n in
      let state = { state with stack } in
      let s = Ref.Struct (Ref.Struct.new_with id (Array.of_list top_n)) in
      ret @@ Stack.push_ref state.stack s
    | Struct (New_default id) ->
      let types = Env.get_types ~env in
      let fields =
        match types.(id).ct with
        | Def_struct_t fl -> fl
        | _ ->
          Fmt.failwith "struct.new_default: type %d is not a struct type" id
      in
      let defaults =
        Array.of_list (List.map (fun (_, (_, st)) -> default_gc_val st) fields)
      in
      let s = Ref.Struct (Ref.Struct.new_with id defaults) in
      ret @@ Stack.push_ref state.stack s
    | Struct (Get (_type_id, field_id)) ->
      let r, stack = Stack.pop_as_ref state.stack in
      let state = { state with stack } in
      begin match r with
      | Ref.Struct s ->
        ret @@ Stack.push state.stack (Ref.Struct.get_field s field_id)
      | r when Ref.is_null r -> Choice.trap (`Msg "null structure reference")
      | _ -> Choice.trap `Element_type_error
      end
    | Struct (Get_s (type_id, field_id)) ->
      let r, stack = Stack.pop_as_ref state.stack in
      let state = { state with stack } in
      begin match r with
      | Ref.Struct s ->
        let types = Env.get_types ~env in
        let packed =
          match types.(type_id).ct with
          | Def_struct_t fl ->
            begin match List.nth_opt fl field_id with
            | Some (_, (_, Pack_type I8)) -> Some 8
            | Some (_, (_, Pack_type I16)) -> Some 16
            | _ -> None
            end
          | _ -> None
        in
        let raw = Ref.Struct.get_field s field_id in
        let* v =
          match raw with
          | I32 i -> (
            match packed with
            | Some 8 ->
              let n = I32.logand i (I32.of_int32 0xFFl) in
              Choice.ite
                (I32.ne (I32.logand n (I32.of_int32 0x80l)) I32.zero)
                ~if_true:(I32 (I32.logor n (I32.of_int32 (Int32.lognot 0xFFl))))
                ~if_false:(I32 n)
            | Some 16 ->
              let n = I32.logand i (I32.of_int32 0xFFFFl) in
              Choice.ite
                (I32.ne (I32.logand n (I32.of_int32 0x8000l)) I32.zero)
                ~if_true:
                  (I32 (I32.logor n (I32.of_int32 (Int32.lognot 0xFFFFl))))
                ~if_false:(I32 n)
            | _ -> Choice.return @@ I32 i )
          | _ -> assert false
        in
        ret @@ Stack.push state.stack v
      | r when Ref.is_null r -> Choice.trap (`Msg "null structure reference")
      | _ -> Choice.trap `Element_type_error
      end
    | Struct (Get_u (type_id, field_id)) ->
      let r, stack = Stack.pop_as_ref state.stack in
      let state = { state with stack } in
      begin match r with
      | Ref.Struct s ->
        let types = Env.get_types ~env in
        let mask =
          match types.(type_id).ct with
          | Def_struct_t fl ->
            begin match List.nth_opt fl field_id with
            | Some (_, (_, Pack_type I8)) -> Some 0xFFl
            | Some (_, (_, Pack_type I16)) -> Some 0xFFFFl
            | _ -> None
            end
          | _ -> None
        in
        let raw = Ref.Struct.get_field s field_id in
        let v =
          match raw with
          | I32 i -> (
            match mask with
            | Some m -> I32.logand i (I32.of_int32 m)
            | None -> i )
          | _ -> assert false
        in
        ret @@ Stack.push_i32 state.stack v
      | r when Ref.is_null r -> Choice.trap (`Msg "null structure reference")
      | _ -> Choice.trap `Element_type_error
      end
    | Struct (Set (_type_id, field_id)) ->
      let v, stack = Stack.pop state.stack in
      let r, stack = Stack.pop_as_ref stack in
      let state = { state with stack } in
      begin match r with
      | Ref.Struct s ->
        Ref.Struct.set_field s field_id v;
        ret state.stack
      | r when Ref.is_null r -> Choice.trap (`Msg "null structure reference")
      | _ -> Choice.trap `Element_type_error
      end
    | Array (New id) ->
      let n, stack = Stack.pop_i32 state.stack in
      let v, stack = Stack.pop stack in
      let state = { state with stack } in
      let a = Ref.Array (Ref.Array.new_fill id v n) in
      ret @@ Stack.push_ref state.stack a
    | Array (New_default id) ->
      let n, stack = Stack.pop_i32 state.stack in
      let state = { state with stack } in
      let types = Env.get_types ~env in
      let st =
        match types.(id).ct with
        | Def_array_t (_, st) -> st
        | _ -> Fmt.failwith "array.new_default: type %d is not an array type" id
      in
      let a = Ref.Array (Ref.Array.new_fill id (default_gc_val st) n) in
      ret @@ Stack.push_ref state.stack a
    | Array (New_fixed (id, n)) ->
      let n = Int32.to_int n in
      let top_n, stack = Stack.pop_n state.stack n in
      let state = { state with stack } in
      let elems = Array.of_list (List.rev top_n) in
      let a = Ref.Array (Ref.Array.new_fixed_with id elems) in
      ret @@ Stack.push_ref state.stack a
    | Array (Get _id) ->
      let idx, stack = Stack.pop_i32 state.stack in
      let r, stack = Stack.pop_as_ref stack in
      let state = { state with stack } in
      begin match r with
      | Ref.Array a ->
        (* TODO: rewrite to avoid select_i32 ! *)
        let* idx' = Choice.select_i32 idx in
        let* len = Choice.select_i32 (Ref.Array.length a) in
        let idx = Concrete_i32.to_int idx' in
        let len = Concrete_i32.to_int len in
        if idx < 0 || idx >= len then
          Choice.trap (`Msg "out of bounds array access")
        else
          ret
          @@ Stack.push state.stack (Ref.Array.get_elem a (I32.of_int32 idx'))
      | r when Ref.is_null r -> Choice.trap (`Msg "null array reference")
      | _ -> Choice.trap `Element_type_error
      end
    | Array (Get_s id) ->
      let idx, stack = Stack.pop_i32 state.stack in
      let r, stack = Stack.pop_as_ref stack in
      let state = { state with stack } in
      begin match r with
      | Ref.Array a ->
        (* TODO: rewrite to avoid select_i32 ! *)
        let* len = select_i32 (Ref.Array.length a) in
        let types = Env.get_types ~env in
        let packed =
          match types.(id).ct with
          | Def_array_t (_, st) -> (
            match st with
            | Pack_type I8 -> Some 8
            | Pack_type I16 -> Some 16
            | _ -> None )
          | _ -> None
        in
        (* TODO: rewrite to avoid select_i32 ! *)
        let* idx' = Choice.select_i32 idx in
        let idx' = Int32.to_int idx' in
        if idx' < 0 || idx' >= Int32.to_int len then
          Choice.trap (`Msg "out of bounds array access")
        else
          let raw = Ref.Array.get_elem a idx in
          let* v =
            match raw with
            | I32 i -> (
              match packed with
              | Some 8 ->
                let n = I32.logand i (I32.of_int32 0xFFl) in
                Choice.ite
                  (I32.ne (I32.logand n (I32.of_int32 0x80l)) I32.zero)
                  ~if_true:
                    (I32 (I32.logor n (I32.of_int32 (Int32.lognot 0xFFl))))
                  ~if_false:(I32 n)
              | Some 16 ->
                let n = I32.logand i (I32.of_int32 0xFFFFl) in
                Choice.ite
                  (I32.ne (I32.logand n (I32.of_int32 0x8000l)) I32.zero)
                  ~if_true:
                    (I32 (I32.logor n (I32.of_int32 @@ Int32.lognot 0xFFFFl)))
                  ~if_false:(I32 n)
              | _ -> Choice.return @@ I32 i )
            | _ -> assert false
          in
          ret @@ Stack.push state.stack v
      | r when Ref.is_null r -> Choice.trap (`Msg "null array reference")
      | _ -> Choice.trap `Element_type_error
      end
    | Array (Get_u id) ->
      let idx, stack = Stack.pop_i32 state.stack in
      let r, stack = Stack.pop_as_ref stack in
      let state = { state with stack } in
      begin match r with
      | Ref.Array a ->
        (* TODO: rewrite to avoid select_i32 ! *)
        let* len = select_i32 (Ref.Array.length a) in
        let types = Env.get_types ~env in
        let mask =
          match types.(id).ct with
          | Def_array_t (_, st) -> (
            match st with
            | Pack_type I8 -> Some 0xFFl
            | Pack_type I16 -> Some 0xFFFFl
            | _ -> None )
          | _ -> None
        in
        (* TODO: rewrite to avoid select_i32 ! *)
        let* idx' = Choice.select_i32 idx in
        let idx' = Int32.to_int idx' in
        if idx' < 0 || idx' >= Int32.to_int len then
          Choice.trap (`Msg "out of bounds array access")
        else
          let raw = Ref.Array.get_elem a idx in
          let v =
            match raw with
            | I32 i -> (
              match mask with
              | Some m -> I32.logand i (I32.of_int32 m)
              | None -> i )
            | _ -> assert false
          in
          ret @@ Stack.push_i32 state.stack v
      | r when Ref.is_null r -> Choice.trap (`Msg "null array reference")
      | _ -> Choice.trap `Element_type_error
      end
    | Array (Set _id) ->
      let v, stack = Stack.pop state.stack in
      let idx, stack = Stack.pop_i32 stack in
      let r, stack = Stack.pop_as_ref stack in
      let state = { state with stack } in
      begin match r with
      | Ref.Array a ->
        (* TODO: rewrite to avoid select_i32 ! *)
        let* len = select_i32 (Ref.Array.length a) in
        let* idx' = Choice.select_i32 idx in
        let idx' = Int32.to_int idx' in
        if idx' < 0 || idx' >= Int32.to_int len then
          Choice.trap (`Msg "out of bounds array access")
        else begin
          Ref.Array.set_elem a idx v;
          ret state.stack
        end
      | r when Ref.is_null r -> Choice.trap (`Msg "null array reference")
      | _ -> Choice.trap `Element_type_error
      end
    | Array Len ->
      let r, stack = Stack.pop_as_ref state.stack in
      let state = { state with stack } in
      begin match r with
      | Ref.Array a ->
        let len = Ref.Array.length a in
        ret @@ Stack.push_i32 state.stack len
      | r when Ref.is_null r -> Choice.trap (`Msg "null array reference")
      | _ -> Choice.trap `Element_type_error
      end
    | Any_convert_extern ->
      let r, stack = Stack.pop_as_ref state.stack in
      let state = { state with stack } in
      ret @@ Stack.push_ref state.stack (Ref.any_convert_extern r)
    | Extern_convert_any ->
      let r, stack = Stack.pop_as_ref state.stack in
      let state = { state with stack } in
      ret @@ Stack.push_ref state.stack (Ref.extern_convert_any r)
    | Array (Fill _id) ->
      let n, stack = Stack.pop_i32 state.stack in
      let v, stack = Stack.pop stack in
      let dst_off, stack = Stack.pop_i32 stack in
      let array, stack = Stack.pop_as_ref stack in
      let state = { state with stack } in
      let* arr = get_array_ref array in
      let* n = select_int n in
      let* dst_off = select_int dst_off in
      let* () =
        (* TODO: rewrite this! *)
        let* b = check_array_oob dst_off n arr in
        if b then Choice.trap (`Msg "out of bounds array access")
        else Choice.return ()
      in
      let gv = v in
      for i = 0 to n - 1 do
        Ref.Array.set_elem arr (I32.of_int (dst_off + i)) gv
      done;
      ret state.stack
    | Array (Copy (_dst_id, _src_id)) ->
      let n, stack = Stack.pop_i32 state.stack in
      let s_off, stack = Stack.pop_i32 stack in
      let src, stack = Stack.pop_as_ref stack in
      let d_off, stack = Stack.pop_i32 stack in
      let dst, stack = Stack.pop_as_ref stack in
      let state = { state with stack } in
      let* d_arr = get_array_ref dst in
      let* s_arr = get_array_ref src in
      let* n = select_int n in
      let* d_off = select_int d_off in
      let* s_off = select_int s_off in
      let* () =
        (* TODO: rewrite this! *)
        let* b = check_array_oob d_off n d_arr in
        if b then Choice.trap (`Msg "out of bounds array access")
        else
          let* b = check_array_oob s_off n s_arr in
          if b then Choice.trap (`Msg "out of bounds array access")
          else Choice.return ()
      in
      if d_off <= s_off then
        for i = 0 to n - 1 do
          let elt = Ref.Array.get_elem s_arr (I32.of_int @@ (s_off + i)) in
          Ref.Array.set_elem d_arr (I32.of_int @@ (d_off + i)) elt
        done
      else
        for i = n - 1 downto 0 do
          let elt = Ref.Array.get_elem s_arr (I32.of_int @@ (s_off + i)) in
          Ref.Array.set_elem d_arr (I32.of_int @@ (d_off + i)) elt
        done;
      ret state.stack
    | Array (New_data (arr_id, data_id)) ->
      let n, stack = Stack.pop_i32 state.stack in
      let offset, stack = Stack.pop_i32 stack in
      let state = { state with stack } in
      let* n = select_int n in
      let* offset = select_int offset in
      let st = get_array_storage_type state arr_id "array.new_data" in
      let elem_size = array_data_elem_size st in
      let data = Env.get_data ~env data_id in
      if offset < 0 || n < 0 || offset + (n * elem_size) > Data.size data then
        Choice.trap (`Msg "out of bounds memory access")
      else begin
        let data = Data.value data in
        let elems =
          Array.init n (fun i ->
            read_data_gc_val st data (offset + (i * elem_size)) )
        in
        let a = Ref.Array (Ref.Array.new_fixed_with arr_id elems) in
        ret @@ Stack.push_ref state.stack a
      end
    | Array (New_elem (arr_id, elem_id)) ->
      let n, stack = Stack.pop_i32 state.stack in
      let offset, stack = Stack.pop_i32 stack in
      let state = { state with stack } in
      let* n = select_int n in
      let* offset = select_int offset in
      let elem = Env.get_elem ~env elem_id in
      if offset < 0 || n < 0 || offset + n > Elem.size elem then
        Choice.trap (`Msg "out of bounds table access")
      else begin
        let elems =
          Array.init n (fun i ->
            let e = Elem.get elem (offset + i) in
            Ref e )
        in
        let a = Ref.Array (Ref.Array.new_fixed_with arr_id elems) in
        ret @@ Stack.push_ref state.stack a
      end
    | Array (Init_data (arr_id, data_id)) ->
      let n, stack = Stack.pop_i32 state.stack in
      let s_off, stack = Stack.pop_i32 stack in
      let d_off, stack = Stack.pop_i32 stack in
      let array, stack = Stack.pop_as_ref stack in
      let state = { state with stack } in
      let* arr = get_array_ref array in
      let* n = select_int n in
      let* s_off = select_int s_off in
      let* d_off = select_int d_off in
      let st = get_array_storage_type state arr_id "array.init_data" in
      let elem_size = array_data_elem_size st in
      let data = Env.get_data ~env data_id in
      let data_str = Data.value data in
      let* () =
        (* TODO: rewrite this! *)
        let* b = check_array_oob d_off n arr in
        if b then Choice.trap (`Msg "out of bounds array access")
        else if s_off < 0 || s_off + (n * elem_size) > Data.size data then
          Choice.trap (`Msg "out of bounds memory access")
        else Choice.return ()
      in
      for i = 0 to n - 1 do
        let v = read_data_gc_val st data_str (s_off + (i * elem_size)) in
        Ref.Array.set_elem arr (I32.of_int @@ (d_off + i)) v
      done;
      ret state.stack
    | Array (Init_elem (_arr_id, elem_id)) ->
      let n, stack = Stack.pop_i32 state.stack in
      let s_off, stack = Stack.pop_i32 stack in
      let d_off, stack = Stack.pop_i32 stack in
      let arr_ref, stack = Stack.pop_as_ref stack in
      let state = { state with stack } in
      let* arr = get_array_ref arr_ref in
      let* n = select_int n in
      let* s_off = select_int s_off in
      let* d_off = select_int d_off in
      let elem = Env.get_elem ~env elem_id in
      let* () =
        let* b = check_array_oob d_off n arr in
        if b then Choice.trap (`Msg "out of bounds array access")
        else if s_off < 0 || s_off + n > Elem.size elem then
          Choice.trap (`Msg "out of bounds table access")
        else Choice.return ()
      in
      for i = 0 to n - 1 do
        let v = Ref (Elem.get elem (s_off + i)) in
        Ref.Array.set_elem arr (I32.of_int @@ (d_off + i)) v
      done;
      ret state.stack

  let exec_instr ({ raw; uuid; instr_counter; _ } : _ Annotated.t)
    ({ stack; env; _ } as state : State.t) : State.instr_result Choice.t =
    let instr_counter = Atomic.fetch_and_add instr_counter 1 in
    Log.info (fun m -> m "stack         : [ %a ]" Stack.pp stack);
    Log.info (fun m ->
      m "running instr : %a (executed %d times)" (pp_instr ~short:true) raw
        instr_counter );
    let* () =
      match Logs.Src.level Log.main_src with
      | Some Logs.Debug ->
        let+ pc = Choice.get_pc () in
        if not (Smtml.Expr.Set.is_empty pc) then
          Log.debug (fun m ->
            m "path condition smt query:@\n @[<v>%a@]" Smtml.Expr.pp_smtml
              (Smtml.Expr.Set.to_list pc) )
      | None | Some _ -> return ()
    in
    match raw with
    | Simple i ->
      let* state = exec_simple_instruction state instr_counter ~uuid i in
      Choice.return (State.Continue state)
    | Return -> Choice.return (State.return state)
    | If_else (_id, bt, e1, e2) ->
      let* b, stack =
        let instr_counter_true =
          Next_instruction.exec_block state ~is_loop:false e1
          |> Next_instruction.with_instr_counter
        in
        let instr_counter_false =
          Next_instruction.exec_block state ~is_loop:false e2
          |> Next_instruction.with_instr_counter
        in
        pop_choice stack ~instr_counter_true ~instr_counter_false
      in
      let state = { state with stack } in
      exec_block state ~is_loop:false bt (if b then e1 else e2)
    | Call i -> begin
      let func = Env.get_func ~env i in
      exec_vfunc ~return:false state func
      end
    | Return_call i -> begin
      let func = Env.get_func ~env i in
      exec_vfunc ~return:true state func
      end
    | Br i -> State.branch state i
    | Br_if i ->
      let* b, stack =
        let instr_counter_true =
          Next_instruction.branch state i |> Next_instruction.with_instr_counter
        in
        let instr_counter_false =
          Next_instruction.continue state |> Next_instruction.with_instr_counter
        in
        pop_choice stack ~instr_counter_true ~instr_counter_false
      in
      let state = { state with stack } in
      if b then State.branch state i else Choice.return (State.Continue state)
    | Br_on_null i ->
      let instr_counter_true =
        Next_instruction.branch state i |> Next_instruction.with_instr_counter
      in
      let instr_counter_false =
        Next_instruction.continue state |> Next_instruction.with_instr_counter
      in
      let r, stack = Stack.pop_as_ref stack in
      let is_null = Ref.is_null r |> Boolean.of_bool in
      let* is_null, stack =
        let* is_null =
          select is_null ~instr_counter_false ~instr_counter_true
        in
        return (is_null, stack)
      in
      let state = { state with stack } in
      if is_null then State.branch state i
      else
        (* TODO: restrict the type of r to non-nullable refs *)
        let stack = Stack.push_ref stack r in
        Choice.return (State.Continue { state with stack })
    | Br_on_non_null i ->
      let instr_counter_true =
        Next_instruction.branch state i |> Next_instruction.with_instr_counter
      in
      let instr_counter_false =
        Next_instruction.continue state |> Next_instruction.with_instr_counter
      in
      let r, stack = Stack.pop_as_ref stack in
      let* is_non_null, stack =
        let is_non_null = (not (Ref.is_null r)) |> Boolean.of_bool in
        let* is_non_null =
          select is_non_null ~instr_counter_false ~instr_counter_true
        in
        return (is_non_null, stack)
      in
      let state = { state with stack } in
      if is_non_null then
        let stack = Stack.push_ref stack r in
        State.branch { state with stack } i
      else Choice.return (State.Continue state)
    | Br_on_cast (i, _rt1, rt2) ->
      let instr_counter_true =
        Next_instruction.branch state i |> Next_instruction.with_instr_counter
      in
      let instr_counter_false =
        Next_instruction.continue state |> Next_instruction.with_instr_counter
      in
      let r, stack = Stack.pop_as_ref stack in
      let matches = ref_matches_ref_type ~env r rt2 |> Boolean.of_bool in
      let* matches, stack =
        let* matches =
          select matches ~instr_counter_false ~instr_counter_true
        in
        return (matches, stack)
      in
      let stack = Stack.push_ref stack r in
      let state = { state with stack } in
      if matches then State.branch state i
      else Choice.return (State.Continue state)
    | Br_on_cast_fail (i, _rt1, rt2) ->
      let instr_counter_true =
        Next_instruction.continue state |> Next_instruction.with_instr_counter
      in
      let instr_counter_false =
        (* branch if the condition is false *)
        Next_instruction.branch state i |> Next_instruction.with_instr_counter
      in
      let r, stack = Stack.pop_as_ref stack in
      let matches = ref_matches_ref_type ~env r rt2 |> Boolean.of_bool in
      let* matches, stack =
        let* matches =
          select matches ~instr_counter_true ~instr_counter_false
        in
        return (matches, stack)
      in
      let stack = Stack.push_ref stack r in
      let state = { state with stack } in
      if not matches then State.branch state i
      else Choice.return (State.Continue state)
    | Loop (_id, bt, e) -> exec_block state ~is_loop:true bt e
    | Block (_id, bt, e) -> exec_block state ~is_loop:false bt e
    | Br_table (inds, i) ->
      let target, stack = Stack.pop_i32 stack in
      let> out = I32.(le_u (I32.of_int (Array.length inds)) target) in
      let* target =
        if out then return i
        else
          let+ target = Choice.select_i32 target in
          let target = Int32.to_int target in
          inds.(target)
      in
      let state = { state with stack } in
      State.branch state target
    | Call_indirect (tbl_i, typ_i) ->
      call_indirect ~env ~return:false state (tbl_i, typ_i)
    | Return_call_indirect (tbl_i, typ_i) ->
      call_indirect ~env ~return:true state (tbl_i, typ_i)
    | Call_ref typ_i -> call_ref ~return:false state typ_i
    | Return_call_ref typ_i -> call_ref ~return:true state typ_i

  let rec loop ~heartbeat (state : State.t) : (Env.t * Value.t list) Choice.t =
    let* () =
      match heartbeat with None -> Choice.return () | Some f -> f ()
    in
    match state.pc.raw with
    | instr :: pc -> begin
      let pc = Annotated.dummy pc in
      let* state = exec_instr instr { state with pc } in
      match state with
      | State.Continue state -> loop ~heartbeat state
      | State.Return (state, res) -> Choice.return (state.env, res)
      end
    | [] -> (
      let* next_state = State.end_block state in
      match next_state with
      | State.Continue state -> loop ~heartbeat state
      | State.Return (state, res) -> Choice.return (state.env, res) )

  let exec_expr ~heartbeat env locals stack expr bt :
    (Env.t * Value.t list) Choice.t =
    let state : State.t =
      let func_rt = match bt with None -> [] | Some rt -> rt in
      { stack
      ; locals
      ; env
      ; func_rt
      ; block_stack = []
      ; pc = expr
      ; return_state = None
      }
    in
    loop ~heartbeat state

  let make_heartbeat () =
    match (Parameters.timeout, Parameters.timeout_instr) with
    | None, None -> None
    | Some _, _ | _, Some _ ->
      let fuel =
        Atomic.make
          (match Parameters.timeout_instr with Some i -> i | None -> max_int)
      in
      let after_time =
        let start_time = Unix.gettimeofday () in
        fun timeout_s ->
          Float.compare (Unix.gettimeofday () -. start_time) timeout_s > 0
      in
      Some
        (fun () ->
          let fuel_left = Atomic.fetch_and_add fuel (-1) in
          (* If we only use [timeout_instr], we want to stop all as
             soon as [fuel_left <= 0]. But if we only use [timeout],
             we don't want to run into the slow path below on each
             instruction after [fuel_left] becomes negative. We avoid
             this repeated slow path by bumping [fuel] to [max_int]
             again in this case. *)
          if fuel_left mod 1024 = 0 || fuel_left < 0 then begin
            let stop =
              match (Parameters.timeout, Parameters.timeout_instr) with
              | None, None -> assert false
              | None, Some _instr -> fuel_left <= 0
              | Some s, Some _instr -> after_time s || fuel_left <= 0
              | Some s, None ->
                let stop = after_time s in
                if (not stop) && fuel_left < 0 then Atomic.set fuel max_int;
                stop
            in
            if stop then Choice.trap (`Msg "timeout") else Choice.return ()
          end
          else Choice.return () )

  let modul ~(env : Env.t) ~(modul : Env.modul) : Env.t Choice.t =
    let init_code = Env.get_initialization_code ~modul ~env in
    let heartbeat = make_heartbeat () in
    Log.info (fun m -> m "interpreting ...");
    try
      begin
        let+ env, _end_stack =
          exec_expr ~heartbeat env (State.Locals.of_list []) Stack.empty
            (Annotated.dummy init_code)
            None
        in
        env
      end
    with Stack_overflow -> Choice.trap `Call_stack_exhausted

  let exec_vfunc_from_outside ~env ~locals (func : Extern_func.t Kind.func) :
    (Env.t * Value.t list) Choice.t =
    let state = State.empty ~locals ~env () in
    try
      begin
        let* state =
          match func with
          | Kind.Wasm func ->
            let state = State.{ state with stack = locals } in
            Choice.return (State.Continue (exec_func ~return:true state func))
          | Extern func ->
            let+ stack = exec_extern_func ~state func in
            let state = State.{ state with stack } in
            State.return state
        in
        match state with
        | State.Return (state, res) -> Choice.return (state.env, res)
        | State.Continue state -> loop ~heartbeat:None state
      end
    with Stack_overflow -> Choice.trap `Call_stack_exhausted
end

module Concrete (Parameters : Parameters) =
  Make [@inlined hint] (Concrete_value) (Concrete_data) (Concrete_elem)
    (Concrete_choice)
    (Concrete_table)
    (Concrete_memory)
    (Concrete_extern.Func)
    (Env.Concrete)
    (Parameters)
module Symbolic (Parameters : Parameters) =
  Make [@inlined hint] (Symbolic_value) (Symbolic_data) (Symbolic_elem)
    (Symbolic_choice)
    (Symbolic_table)
    (Symbolic_memory)
    (Symbolic_extern.Func)
    (Env.Symbolic)
    (Parameters)
