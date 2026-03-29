(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Binary

module type Parameters = sig
  val use_ite_for_select : bool

  val throw_away_trap : bool

  val timeout : float option

  val timeout_instr : int option
end

module Default_parameters = struct
  let use_ite_for_select = true

  let throw_away_trap = false

  let timeout = None

  let timeout_instr = None
end

module Make
    (Value : Value_intf.T)
    (Data : Data_intf.T)
    (Elem : Elem_intf.T with type reference := Value.Ref.t)
    (Choice :
      Choice_intf.S
        with type boolean := Value.boolean
         and type i32 := Value.i32
         and type value := Value.t)
    (Table :
      Table_intf.T
        with type reference := Value.Ref.t
         and type 'a choice := 'a Choice.t)
    (Global :
      Global_intf.T with type value := Value.t and type 'a choice := 'a Choice.t)
    (Memory :
      Memory_intf.T
        with type i32 := Value.i32
         and type i64 := Value.i64
         and type 'a choice := 'a Choice.t)
    (Extern_func :
      Extern.Func.T
        with type i32 := Value.i32
         and type i64 := Value.i64
         and type f32 := Value.f32
         and type f64 := Value.f64
         and type v128 := Value.v128
         and type memory := Memory.t
         and type 'a m := 'a Choice.t)
    (Env :
      Env_intf.T
        with type data := Data.t
         and type memory := Memory.t
         and type global := Global.t
         and type table := Table.t
         and type elem := Elem.t
         and type extern_func := Extern_func.extern_func
         and type 'a choice := 'a Choice.t)
    (Parameters : Parameters) =
struct
  open Value
  open Choice
  module Stack = Stack.Make [@inlined hint] (Value)

  module I32 = struct
    include I32

    (* TODO: move all of this to I32_intf *)
    let ( + ) = add

    let ( = ) = eq

    let eqz v = v = zero

    let min_int = I32.of_int32 Int32.min_int
  end

  module I64 = struct
    include I64

    (* TODO: move all of this to I64_intf *)
    let ( + ) = add

    let ( * ) = mul

    let ( / ) = div

    let ( = ) = eq

    let eqz v = v = zero

    let min_int = I64.of_int64 Int64.min_int
  end

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

  (* In case of throw_away_trap, this is only going in the non-trapping branch, to avoid a useless solver call.
     Otherwise, this will properly try both branches (one trapping, one non-trapping).
     I.e. this can be read as `if v then trap else f` (or assume (not v) and f) in the non-trapping mode).
  *)
  let ( let>! ) (v, trap, instr_counter) f =
    if Parameters.throw_away_trap then
      let* () = Choice.assume (Boolean.not v) in
      f ()
    else
      (* TODO: can we do something better here? *)
      let instr_counter_true = instr_counter in
      let instr_counter_false = instr_counter in
      let* v = select v ~instr_counter_false ~instr_counter_true in
      if v then Choice.trap trap else f ()

  module State = struct
    type stack = Stack.t

    type value = Value.t

    module Locals : sig
      type t = value array

      val of_list : value list -> t

      val get : t -> int -> value

      val set : t -> int -> value -> t
    end = struct
      type t = value array

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
      ; stack : stack
      ; is_loop : Prelude.Bool.t
      }

    type block_stack = block list

    type exec_state =
      { return_state : exec_state option
      ; stack : stack
      ; locals : Locals.t
          (* TODO: rename this PC, it stands for program counter but is easily confused with path condition... *)
      ; pc : expr Annotated.t
      ; block_stack : block_stack
      ; func_rt : result_type
      ; env : Env.t
      ; envs : Env.t Dynarray.t
      }

    let empty_exec_state ~locals ~env ~envs =
      { return_state = None
      ; stack = []
      ; locals = Locals.of_list locals
      ; pc = Annotated.dummy []
      ; block_stack = []
      ; func_rt = []
      ; env
      ; envs
      }

    type instr_result =
      | Return of value list
      | Continue of exec_state

    let return (state : exec_state) =
      let args = Stack.keep state.stack (List.length state.func_rt) in
      match state.return_state with
      | None -> Return args
      | Some state ->
        let stack = args @ state.stack in
        Continue { state with stack }

    let branch (state : exec_state) n =
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

    let end_block (state : exec_state) =
      match state.block_stack with
      | [] -> Choice.return (return state)
      | block :: block_stack ->
        let args = Stack.keep state.stack (List.length block.continue_rt) in
        let stack = args @ block.stack in
        Choice.return
          (Continue { state with block_stack; pc = block.continue; stack })
  end

  let mk_addr_check_bounds const env memid ~pos ~offset instr_counter =
    if Int64.(lt_u (sub 0xFFFF_FFFF_FFFF_FFFFL const) offset) then
      Choice.trap `Out_of_bounds_memory_access
    else
      let* mem = Env.get_memory env memid in
      let pos = I64.extend_i32_u pos in
      let>! () =
        let len = I64.of_int64 (Int64.add const offset) in
        let mem_size = Memory.size mem |> I64.extend_i32_u in
        (* mem_size <=u len || mem_size - len <u pos *)
        (* TODO: experiment with splitting the disjunction and doing the
           checks one at a time. *)
        ( Boolean.or_ I64.(le_u mem_size len) I64.(lt_u (sub mem_size len) pos)
        , `Out_of_bounds_memory_access
        , Some instr_counter )
      in
      let addr = I32.wrap_i64 I64.(add pos (of_int64 offset)) in
      Choice.return addr

  let mk_addr_check_bounds_1L = mk_addr_check_bounds 1L

  let mk_addr_check_bounds_2L = mk_addr_check_bounds 2L

  let mk_addr_check_bounds_4L = mk_addr_check_bounds 4L

  let mk_addr_check_bounds_8L = mk_addr_check_bounds 8L

  let exec_i32_instr env instr_counter stack :
    Binary.i32_instr -> Stack.t Choice.t = function
    | Const n -> Stack.push_concrete_i32 stack n |> Choice.return
    | Clz -> Stack.apply_i32_i32 stack I32.clz |> Choice.return
    | Ctz -> Stack.apply_i32_i32 stack I32.ctz |> Choice.return
    | Popcnt -> Stack.apply_i32_i32 stack I32.popcnt |> Choice.return
    | Add -> Stack.apply_i32_i32_i32 stack I32.add |> Choice.return
    | Sub -> Stack.apply_i32_i32_i32 stack I32.sub |> Choice.return
    | Mul -> Stack.apply_i32_i32_i32 stack I32.mul |> Choice.return
    | Div S ->
      let (n1, n2), stack = Stack.pop2_i32 stack in
      let>! () =
        (I32.eqz n2, `Integer_divide_by_zero, (* TODO: get instr counter *) None)
      in
      let>! () =
        ( Boolean.and_ (I32.eq n1 I32.min_int) @@ I32.eq n2 (I32.of_int (-1))
        , `Integer_overflow
        , (* TODO: get instr counter *) None )
      in
      Stack.push_i32 stack (I32.div n1 n2) |> Choice.return
    | Div U ->
      let (n1, n2), stack = Stack.pop2_i32 stack in
      let>! () =
        (I32.eqz n2, `Integer_divide_by_zero, (* TODO: get instr counter *) None)
      in
      Stack.push_i32 stack (I32.unsigned_div n1 n2) |> Choice.return
    | Rem S ->
      let (n1, n2), stack = Stack.pop2_i32 stack in
      let>! () =
        (I32.eqz n2, `Integer_divide_by_zero, (* TODO: get instr counter *) None)
      in
      Stack.push_i32 stack (I32.rem n1 n2) |> Choice.return
    | Rem U ->
      let (n1, n2), stack = Stack.pop2_i32 stack in
      let>! () =
        (I32.eqz n2, `Integer_divide_by_zero, (* TODO: get instr counter *) None)
      in
      Stack.push_i32 stack (I32.unsigned_rem n1 n2) |> Choice.return
    | And -> Stack.apply_i32_i32_i32 stack I32.logand |> Choice.return
    | Or -> Stack.apply_i32_i32_i32 stack I32.logor |> Choice.return
    | Xor -> Stack.apply_i32_i32_i32 stack I32.logxor |> Choice.return
    | Shl -> Stack.apply_i32_i32_i32 stack I32.shl |> Choice.return
    | Shr S -> Stack.apply_i32_i32_i32 stack I32.ashr |> Choice.return
    | Shr U -> Stack.apply_i32_i32_i32 stack I32.lshr |> Choice.return
    | Rotl -> Stack.apply_i32_i32_i32 stack I32.rotate_left |> Choice.return
    | Rotr -> Stack.apply_i32_i32_i32 stack I32.rotate_right |> Choice.return
    | Eqz -> Stack.apply_i32_boolean stack I32.eqz |> Choice.return
    | Eq -> Stack.apply_i32_i32_boolean stack I32.eq |> Choice.return
    | Ne -> Stack.apply_i32_i32_boolean stack I32.ne |> Choice.return
    | Lt S -> Stack.apply_i32_i32_boolean stack I32.lt |> Choice.return
    | Lt U -> Stack.apply_i32_i32_boolean stack I32.lt_u |> Choice.return
    | Gt S ->
      Stack.apply_i32_i32_boolean stack (Fun.flip I32.lt) |> Choice.return
    | Gt U ->
      Stack.apply_i32_i32_boolean stack (Fun.flip I32.lt_u) |> Choice.return
    | Le S -> Stack.apply_i32_i32_boolean stack I32.le |> Choice.return
    | Le U -> Stack.apply_i32_i32_boolean stack I32.le_u |> Choice.return
    | Ge S ->
      Stack.apply_i32_i32_boolean stack (Fun.flip I32.le) |> Choice.return
    | Ge U ->
      Stack.apply_i32_i32_boolean stack (Fun.flip I32.le_u) |> Choice.return
    | Trunc_f (Text.S32, sx) ->
      let f, stack = Stack.pop_f32 stack in
      let res =
        match sx with Text.S -> I32.trunc_f32_s f | U -> I32.trunc_f32_u f
      in
      begin match res with
      | Error t -> Choice.trap t
      | Ok res -> Choice.return @@ Stack.push_i32 stack res
      end
    | Trunc_f (Text.S64, sx) ->
      let f, stack = Stack.pop_f64 stack in
      let res =
        match sx with S -> I32.trunc_f64_s f | U -> I32.trunc_f64_u f
      in
      begin match res with
      | Error t -> Choice.trap t
      | Ok res -> Choice.return @@ Stack.push_i32 stack res
      end
    | Trunc_sat_f (Text.S32, S) ->
      Stack.apply_f32_i32 stack I32.trunc_sat_f32_s |> Choice.return
    | Trunc_sat_f (Text.S32, U) ->
      Stack.apply_f32_i32 stack I32.trunc_sat_f32_u |> Choice.return
    | Trunc_sat_f (Text.S64, S) ->
      Stack.apply_f64_i32 stack I32.trunc_sat_f64_s |> Choice.return
    | Trunc_sat_f (Text.S64, U) ->
      Stack.apply_f64_i32 stack I32.trunc_sat_f64_u |> Choice.return
    | Extend8_s -> Stack.apply_i32_i32 stack (I32.extend_s 8) |> Choice.return
    | Extend16_s -> Stack.apply_i32_i32 stack (I32.extend_s 16) |> Choice.return
    | Wrap_i64 -> Stack.apply_i64_i32 stack I32.wrap_i64 |> Choice.return
    | Reinterpret_f Text.S32 ->
      Stack.apply_f32_i32 stack I32.reinterpret_f32 |> Choice.return
    | Reinterpret_f Text.S64 ->
      Stack.apply_f64_i32 stack (Fun.compose I32.reinterpret_f32 F32.demote_f64)
      |> Choice.return
    | Load8 (memid, S, { offset; _ }) ->
      let pos, stack = Stack.pop_i32 stack in
      let* addr =
        mk_addr_check_bounds_1L env memid ~pos ~offset instr_counter
      in
      let* mem = Env.get_memory env memid in
      let* res = Memory.load_8_s mem addr in
      Stack.push_i32 stack res |> Choice.return
    | Load8 (memid, U, { offset; _ }) ->
      let pos, stack = Stack.pop_i32 stack in
      let* addr =
        mk_addr_check_bounds_1L env memid ~pos ~offset instr_counter
      in
      let* mem = Env.get_memory env memid in
      let* res = Memory.load_8_u mem addr in
      Stack.push_i32 stack res |> Choice.return
    | Load16 (memid, S, { offset; _ }) ->
      let pos, stack = Stack.pop_i32 stack in
      let* addr =
        mk_addr_check_bounds_2L env memid ~pos ~offset instr_counter
      in
      let* mem = Env.get_memory env memid in
      let* res = Memory.load_16_s mem addr in
      Stack.push_i32 stack res |> Choice.return
    | Load16 (memid, U, { offset; _ }) ->
      let pos, stack = Stack.pop_i32 stack in
      let* addr =
        mk_addr_check_bounds_2L env memid ~pos ~offset instr_counter
      in
      let* mem = Env.get_memory env memid in
      let* res = Memory.load_16_u mem addr in
      Stack.push_i32 stack res |> Choice.return
    | Load (memid, { offset; _ }) ->
      let pos, stack = Stack.pop_i32 stack in
      let* addr =
        mk_addr_check_bounds_4L env memid ~pos ~offset instr_counter
      in
      let* mem = Env.get_memory env memid in
      let* res = Memory.load_32 mem addr in
      Stack.push_i32 stack res |> Choice.return
    | Store8 (memid, { offset; _ }) ->
      let n, stack = Stack.pop_i32 stack in
      let pos, stack = Stack.pop_i32 stack in
      let* addr =
        mk_addr_check_bounds_1L env memid ~pos ~offset instr_counter
      in
      let* mem = Env.get_memory env memid in
      let+ () = Memory.store_8 mem ~addr n in
      stack
    | Store16 (memid, { offset; _ }) ->
      let n, stack = Stack.pop_i32 stack in
      let pos, stack = Stack.pop_i32 stack in
      let* addr =
        mk_addr_check_bounds_2L env memid ~pos ~offset instr_counter
      in
      let* mem = Env.get_memory env memid in
      let+ () = Memory.store_16 mem ~addr n in
      stack
    | Store (memid, { offset; _ }) ->
      let n, stack = Stack.pop_i32 stack in
      let pos, stack = Stack.pop_i32 stack in
      let* addr =
        mk_addr_check_bounds_4L env memid ~pos ~offset instr_counter
      in
      let* mem = Env.get_memory env memid in
      let+ () = Memory.store_32 mem ~addr n in
      stack

  let exec_i64_instr env instr_counter stack :
    Binary.i64_instr -> Stack.t Choice.t = function
    | Const n -> Stack.push_concrete_i64 stack n |> Choice.return
    | Clz -> Stack.apply_i64_i64 stack I64.clz |> Choice.return
    | Ctz -> Stack.apply_i64_i64 stack I64.ctz |> Choice.return
    | Popcnt -> Stack.apply_i64_i64 stack I64.popcnt |> Choice.return
    | Add -> Stack.apply_i64_i64_i64 stack I64.add |> Choice.return
    | Sub -> Stack.apply_i64_i64_i64 stack I64.sub |> Choice.return
    | Mul -> Stack.apply_i64_i64_i64 stack I64.mul |> Choice.return
    | Div S ->
      let (n1, n2), stack = Stack.pop2_i64 stack in
      let>! () =
        (I64.eqz n2, `Integer_divide_by_zero, (* TODO: get instr counter *) None)
      in
      let>! () =
        ( Boolean.and_ (I64.eq n1 I64.min_int)
          @@ I64.eq n2 (I64.sub (I64.of_int 0) (I64.of_int 1))
        , `Integer_overflow
        , (* TODO: get instr counter *) None )
      in
      Stack.push_i64 stack (I64.div n1 n2) |> Choice.return
    | Div U ->
      let (n1, n2), stack = Stack.pop2_i64 stack in
      let>! () =
        (I64.eqz n2, `Integer_divide_by_zero, (* TODO: get instr counter *) None)
      in
      Stack.push_i64 stack (I64.unsigned_div n1 n2) |> Choice.return
    | Rem S ->
      let (n1, n2), stack = Stack.pop2_i64 stack in
      let>! () =
        (I64.eqz n2, `Integer_divide_by_zero, (* TODO: get instr counter *) None)
      in
      Stack.push_i64 stack (I64.rem n1 n2) |> Choice.return
    | Rem U ->
      let (n1, n2), stack = Stack.pop2_i64 stack in
      let>! () =
        (I64.eqz n2, `Integer_divide_by_zero, (* TODO: get instr counter *) None)
      in
      Stack.push_i64 stack (I64.unsigned_rem n1 n2) |> Choice.return
    | And -> Stack.apply_i64_i64_i64 stack I64.logand |> Choice.return
    | Or -> Stack.apply_i64_i64_i64 stack I64.logor |> Choice.return
    | Xor -> Stack.apply_i64_i64_i64 stack I64.logxor |> Choice.return
    | Shl -> Stack.apply_i64_i64_i64 stack I64.shl |> Choice.return
    | Shr S -> Stack.apply_i64_i64_i64 stack I64.ashr |> Choice.return
    | Shr U -> Stack.apply_i64_i64_i64 stack I64.lshr |> Choice.return
    | Rotl -> Stack.apply_i64_i64_i64 stack I64.rotate_left |> Choice.return
    | Rotr -> Stack.apply_i64_i64_i64 stack I64.rotate_right |> Choice.return
    | Eqz -> Stack.apply_i64_boolean stack I64.eqz |> Choice.return
    | Eq -> Stack.apply_i64_i64_boolean stack I64.eq |> Choice.return
    | Ne -> Stack.apply_i64_i64_boolean stack I64.ne |> Choice.return
    | Lt S -> Stack.apply_i64_i64_boolean stack I64.lt |> Choice.return
    | Lt U -> Stack.apply_i64_i64_boolean stack I64.lt_u |> Choice.return
    | Gt S ->
      Stack.apply_i64_i64_boolean stack (Fun.flip I64.lt) |> Choice.return
    | Gt U ->
      Stack.apply_i64_i64_boolean stack (Fun.flip I64.lt_u) |> Choice.return
    | Le S -> Stack.apply_i64_i64_boolean stack I64.le |> Choice.return
    | Le U -> Stack.apply_i64_i64_boolean stack I64.le_u |> Choice.return
    | Ge S ->
      Stack.apply_i64_i64_boolean stack (Fun.flip I64.le) |> Choice.return
    | Ge U ->
      Stack.apply_i64_i64_boolean stack (Fun.flip I64.le_u) |> Choice.return
    | Trunc_f (Text.S32, sx) ->
      let f, stack = Stack.pop_f32 stack in
      let res =
        match sx with S -> I64.trunc_f32_s f | U -> I64.trunc_f32_u f
      in
      begin match res with
      | Error t -> Choice.trap t
      | Ok res -> Choice.return @@ Stack.push_i64 stack res
      end
    | Trunc_f (Text.S64, sx) ->
      let f, stack = Stack.pop_f64 stack in
      let res =
        match sx with S -> I64.trunc_f64_s f | U -> I64.trunc_f64_u f
      in
      begin match res with
      | Error t -> Choice.trap t
      | Ok res -> Choice.return @@ Stack.push_i64 stack res
      end
    | Trunc_sat_f (Text.S32, S) ->
      Stack.apply_f32_i64 stack I64.trunc_sat_f32_s |> Choice.return
    | Trunc_sat_f (Text.S32, U) ->
      Stack.apply_f32_i64 stack I64.trunc_sat_f32_u |> Choice.return
    | Trunc_sat_f (Text.S64, S) ->
      Stack.apply_f64_i64 stack I64.trunc_sat_f64_s |> Choice.return
    | Trunc_sat_f (Text.S64, U) ->
      Stack.apply_f64_i64 stack I64.trunc_sat_f64_u |> Choice.return
    | Extend8_s -> Stack.apply_i64_i64 stack (I64.extend_s 8) |> Choice.return
    | Extend16_s -> Stack.apply_i64_i64 stack (I64.extend_s 16) |> Choice.return
    | Extend32_s -> Stack.apply_i64_i64 stack (I64.extend_s 32) |> Choice.return
    | Extend_i32 S ->
      Stack.apply_i32_i64 stack I64.extend_i32_s |> Choice.return
    | Extend_i32 U ->
      Stack.apply_i32_i64 stack I64.extend_i32_u |> Choice.return
    | Reinterpret_f S32 ->
      Stack.apply_f32_i64 stack
        (Fun.compose I64.reinterpret_f64 F64.promote_f32)
      |> Choice.return
    | Reinterpret_f S64 ->
      Stack.apply_f64_i64 stack I64.reinterpret_f64 |> Choice.return
    | Load8 (memid, S, { offset; _ }) ->
      let pos, stack = Stack.pop_i32 stack in
      let* addr =
        mk_addr_check_bounds_1L env memid ~pos ~offset instr_counter
      in
      let* mem = Env.get_memory env memid in
      let* res = Memory.load_8_s mem addr in
      Stack.push_i64 stack (I64.of_int32 res) |> Choice.return
    | Load8 (memid, U, { offset; _ }) ->
      let pos, stack = Stack.pop_i32 stack in
      let* addr =
        mk_addr_check_bounds_1L env memid ~pos ~offset instr_counter
      in
      let* mem = Env.get_memory env memid in
      let* res = Memory.load_8_u mem addr in
      Stack.push_i64 stack (I64.of_int32 res) |> Choice.return
    | Load16 (memid, S, { offset; _ }) ->
      let pos, stack = Stack.pop_i32 stack in
      let* addr =
        mk_addr_check_bounds_2L env memid ~pos ~offset instr_counter
      in
      let* mem = Env.get_memory env memid in
      let* res = Memory.load_16_s mem addr in
      Stack.push_i64 stack (I64.of_int32 res) |> Choice.return
    | Load16 (memid, U, { offset; _ }) ->
      let pos, stack = Stack.pop_i32 stack in
      let* addr =
        mk_addr_check_bounds_2L env memid ~pos ~offset instr_counter
      in
      let* mem = Env.get_memory env memid in
      let* res = Memory.load_16_u mem addr in
      Stack.push_i64 stack (I64.of_int32 res) |> Choice.return
    | Load32 (memid, S, { offset; _ }) ->
      let pos, stack = Stack.pop_i32 stack in
      let* addr =
        mk_addr_check_bounds_4L env memid ~pos ~offset instr_counter
      in
      let* mem = Env.get_memory env memid in
      let* res = Memory.load_32 mem addr in
      Stack.push_i64 stack (I64.of_int32 res) |> Choice.return
    | Load32 (memid, U, { offset; _ }) ->
      let pos, stack = Stack.pop_i32 stack in
      let* addr =
        mk_addr_check_bounds_4L env memid ~pos ~offset instr_counter
      in
      let* mem = Env.get_memory env memid in
      let* res = Memory.load_32 mem addr in
      let res =
        let a = I64.shl (I64.of_int 1) (I64.of_int 32) in
        let b = I64.sub a (I64.of_int 1) in
        I64.logand (I64.of_int32 res) b
      in
      Stack.push_i64 stack res |> Choice.return
    | Load (memid, { offset; _ }) ->
      let pos, stack = Stack.pop_i32 stack in
      let* addr =
        mk_addr_check_bounds_8L env memid ~pos ~offset instr_counter
      in
      let* mem = Env.get_memory env memid in
      let* res = Memory.load_64 mem addr in
      Stack.push_i64 stack res |> Choice.return
    | Store8 (memid, { offset; _ }) ->
      let n, stack = Stack.pop_i64 stack in
      let pos, stack = Stack.pop_i32 stack in
      let* addr =
        mk_addr_check_bounds_1L env memid ~pos ~offset instr_counter
      in
      let* mem = Env.get_memory env memid in
      let+ () =
        let n = I64.to_int32 n in
        Memory.store_8 mem ~addr n
      in
      stack
    | Store16 (memid, { offset; _ }) ->
      let n, stack = Stack.pop_i64 stack in
      let pos, stack = Stack.pop_i32 stack in
      let* addr =
        mk_addr_check_bounds_2L env memid ~pos ~offset instr_counter
      in
      let* mem = Env.get_memory env memid in
      let+ () =
        let n = I64.to_int32 n in
        Memory.store_16 mem ~addr n
      in
      stack
    | Store32 (memid, { offset; _ }) ->
      let n, stack = Stack.pop_i64 stack in
      let pos, stack = Stack.pop_i32 stack in
      let* addr =
        mk_addr_check_bounds_4L env memid ~pos ~offset instr_counter
      in
      let* mem = Env.get_memory env memid in
      let+ () =
        let n = I64.to_int32 n in
        Memory.store_32 mem ~addr n
      in
      stack
    | Store (memid, { offset; _ }) ->
      let n, stack = Stack.pop_i64 stack in
      let pos, stack = Stack.pop_i32 stack in
      let* addr =
        mk_addr_check_bounds_8L env memid ~pos ~offset instr_counter
      in
      let* mem = Env.get_memory env memid in
      let+ () = Memory.store_64 mem ~addr n in
      stack

  let exec_f32_instr env instr_counter stack :
    Binary.f32_instr -> Stack.t Choice.t = function
    | Const n -> Stack.push_concrete_f32 stack n |> Choice.return
    | Abs -> Stack.apply_f32_f32 stack F32.abs |> Choice.return
    | Neg -> Stack.apply_f32_f32 stack F32.neg |> Choice.return
    | Sqrt -> Stack.apply_f32_f32 stack F32.sqrt |> Choice.return
    | Ceil -> Stack.apply_f32_f32 stack F32.ceil |> Choice.return
    | Floor -> Stack.apply_f32_f32 stack F32.floor |> Choice.return
    | Trunc -> Stack.apply_f32_f32 stack F32.trunc |> Choice.return
    | Nearest -> Stack.apply_f32_f32 stack F32.nearest |> Choice.return
    | Add -> Stack.apply_f32_f32_f32 stack F32.add |> Choice.return
    | Sub -> Stack.apply_f32_f32_f32 stack F32.sub |> Choice.return
    | Mul -> Stack.apply_f32_f32_f32 stack F32.mul |> Choice.return
    | Div -> Stack.apply_f32_f32_f32 stack F32.div |> Choice.return
    | Min -> Stack.apply_f32_f32_f32 stack F32.min |> Choice.return
    | Max -> Stack.apply_f32_f32_f32 stack F32.max |> Choice.return
    | Copysign -> Stack.apply_f32_f32_f32 stack F32.copy_sign |> Choice.return
    | Eq -> Stack.apply_f32_f32_boolean stack F32.eq |> Choice.return
    | Ne -> Stack.apply_f32_f32_boolean stack F32.ne |> Choice.return
    | Lt -> Stack.apply_f32_f32_boolean stack F32.lt |> Choice.return
    | Gt -> Stack.apply_f32_f32_boolean stack (Fun.flip F32.lt) |> Choice.return
    | Le -> Stack.apply_f32_f32_boolean stack F32.le |> Choice.return
    | Ge -> Stack.apply_f32_f32_boolean stack (Fun.flip F32.le) |> Choice.return
    | Demote_f64 -> Stack.apply_f64_f32 stack F32.demote_f64 |> Choice.return
    | Convert_i (S32, S) ->
      Stack.apply_i32_f32 stack F32.convert_i32_s |> Choice.return
    | Convert_i (S32, U) ->
      Stack.apply_i32_f32 stack F32.convert_i32_u |> Choice.return
    | Convert_i (S64, S) ->
      Stack.apply_i64_f32 stack F32.convert_i64_s |> Choice.return
    | Convert_i (S64, U) ->
      Stack.apply_i64_f32 stack F32.convert_i64_u |> Choice.return
    | Reinterpret_i S32 ->
      Stack.apply_i32_f32 stack F32.reinterpret_i32 |> Choice.return
    | Reinterpret_i S64 ->
      Stack.apply_i64_f32 stack (Fun.compose F32.reinterpret_i32 I64.to_int32)
      |> Choice.return
    | Load (memid, { offset; _ }) ->
      let pos, stack = Stack.pop_i32 stack in
      let* addr =
        mk_addr_check_bounds_4L env memid ~pos ~offset instr_counter
      in
      let* mem = Env.get_memory env memid in
      let+ res = Memory.load_32 mem addr in
      Stack.push_f32 stack (F32.of_bits res)
    | Store (memid, { offset; _ }) ->
      let n, stack = Stack.pop_f32 stack in
      let pos, stack = Stack.pop_i32 stack in
      let* addr =
        mk_addr_check_bounds_4L env memid ~pos ~offset instr_counter
      in
      let* mem = Env.get_memory env memid in
      let+ () = Memory.store_32 mem ~addr (F32.to_bits n) in
      stack

  let exec_f64_instr env instr_counter stack :
    Binary.f64_instr -> Stack.t Choice.t = function
    | Const n -> Stack.push_concrete_f64 stack n |> Choice.return
    | Abs -> Stack.apply_f64_f64 stack F64.abs |> Choice.return
    | Neg -> Stack.apply_f64_f64 stack F64.neg |> Choice.return
    | Sqrt -> Stack.apply_f64_f64 stack F64.sqrt |> Choice.return
    | Ceil -> Stack.apply_f64_f64 stack F64.ceil |> Choice.return
    | Floor -> Stack.apply_f64_f64 stack F64.floor |> Choice.return
    | Trunc -> Stack.apply_f64_f64 stack F64.trunc |> Choice.return
    | Nearest -> Stack.apply_f64_f64 stack F64.nearest |> Choice.return
    | Add -> Stack.apply_f64_f64_f64 stack F64.add |> Choice.return
    | Sub -> Stack.apply_f64_f64_f64 stack F64.sub |> Choice.return
    | Mul -> Stack.apply_f64_f64_f64 stack F64.mul |> Choice.return
    | Div -> Stack.apply_f64_f64_f64 stack F64.div |> Choice.return
    | Min -> Stack.apply_f64_f64_f64 stack F64.min |> Choice.return
    | Max -> Stack.apply_f64_f64_f64 stack F64.max |> Choice.return
    | Copysign -> Stack.apply_f64_f64_f64 stack F64.copy_sign |> Choice.return
    | Eq -> Stack.apply_f64_f64_boolean stack F64.eq |> Choice.return
    | Ne -> Stack.apply_f64_f64_boolean stack F64.ne |> Choice.return
    | Lt -> Stack.apply_f64_f64_boolean stack F64.lt |> Choice.return
    | Gt -> Stack.apply_f64_f64_boolean stack (Fun.flip F64.lt) |> Choice.return
    | Le -> Stack.apply_f64_f64_boolean stack F64.le |> Choice.return
    | Ge -> Stack.apply_f64_f64_boolean stack (Fun.flip F64.le) |> Choice.return
    | Promote_f32 -> Stack.apply_f32_f64 stack F64.promote_f32 |> Choice.return
    | Convert_i (S32, S) ->
      Stack.apply_i32_f64 stack F64.convert_i32_s |> Choice.return
    | Convert_i (S32, U) ->
      Stack.apply_i32_f64 stack F64.convert_i32_u |> Choice.return
    | Convert_i (S64, S) ->
      Stack.apply_i64_f64 stack F64.convert_i64_s |> Choice.return
    | Convert_i (S64, U) ->
      Stack.apply_i64_f64 stack F64.convert_i64_u |> Choice.return
    | Reinterpret_i S32 ->
      Stack.apply_i32_f64 stack (Fun.compose F64.reinterpret_i64 I64.of_int32)
      |> Choice.return
    | Reinterpret_i S64 ->
      Stack.apply_i64_f64 stack F64.reinterpret_i64 |> Choice.return
    | Load (memid, { offset; _ }) ->
      let pos, stack = Stack.pop_i32 stack in
      (* I32.of_concrete 8l |> I64.extend_i32_u = I64.of_concrete 8L, right?  *)
      let* addr =
        mk_addr_check_bounds_8L env memid ~pos ~offset instr_counter
      in
      let* mem = Env.get_memory env memid in
      let+ res = Memory.load_64 mem addr in
      Stack.push_f64 stack (F64.of_bits res)
    | Store (memid, { offset; _ }) ->
      let n, stack = Stack.pop_f64 stack in
      let pos, stack = Stack.pop_i32 stack in
      let* addr =
        mk_addr_check_bounds_8L env memid ~pos ~offset instr_counter
      in
      let* mem = Env.get_memory env memid in
      let+ () = Memory.store_64 mem ~addr (F64.to_bits n) in
      stack

  let exec_v128_instr stack : Text.v128_instr -> _ = function
    | Const n -> Stack.push_concrete_v128 stack n

  let exec_i8x16_instr _stack : Text.i8x16_instr -> _ = function
    | _ -> (* TODO *) assert false

  let exec_i16x8_instr _stack : Text.i16x8_instr -> _ = function
    | _ -> (* TODO *) assert false

  let exec_i32x4_instr stack : Text.i32x4_instr -> _ = function
    | Add ->
      let (n1, n2), stack = Stack.pop2_v128 stack in
      let a1, b1, c1, d1 = V128.to_i32x4 n1 in
      let a2, b2, c2, d2 = V128.to_i32x4 n2 in
      let a = I32.add a1 a2 in
      let b = I32.add b1 b2 in
      let c = I32.add c1 c2 in
      let d = I32.add d1 d2 in
      Stack.push_v128 stack (V128.of_i32x4 a b c d)
    | Sub ->
      let (n1, n2), stack = Stack.pop2_v128 stack in
      let a1, b1, c1, d1 = V128.to_i32x4 n1 in
      let a2, b2, c2, d2 = V128.to_i32x4 n2 in
      let a = I32.sub a1 a2 in
      let b = I32.sub b1 b2 in
      let c = I32.sub c1 c2 in
      let d = I32.sub d1 d2 in
      Stack.push_v128 stack (V128.of_i32x4 a b c d)

  let exec_i64x2_instr stack : Text.i64x2_instr -> _ = function
    | Add ->
      let (n1, n2), stack = Stack.pop2_v128 stack in
      let a1, b1 = V128.to_i64x2 n1 in
      let a2, b2 = V128.to_i64x2 n2 in
      let a = I64.add a1 a2 in
      let b = I64.add b1 b2 in
      Stack.push_v128 stack (V128.of_i64x2 a b)
    | Sub ->
      let (n1, n2), stack = Stack.pop2_v128 stack in
      let a1, b1 = V128.to_i64x2 n1 in
      let a2, b2 = V128.to_i64x2 n2 in
      let a = I64.sub a1 a2 in
      let b = I64.sub b1 b2 in
      Stack.push_v128 stack (V128.of_i64x2 a b)

  let exec_ref_instr env stack : Binary.ref_instr -> _ = function
    | Null t -> Stack.push_ref stack (Ref.null t) |> Choice.return
    | Is_null ->
      let r, stack = Stack.pop_as_ref stack in
      let is_null = Ref.is_null r |> Boolean.of_bool in
      Stack.push_bool stack is_null |> Choice.return
    | As_non_null ->
      let r, stack = Stack.pop_as_ref stack in
      if Ref.is_null r then
        Choice.trap
          (`Type_mismatch
             "ref.as_non_null expected a non-null reference but got null" )
      else Stack.push_ref stack r |> Choice.return
    (* TODO: restrict to non_null refs *)
    | Func i ->
      let f = Env.get_func env i in
      Stack.push_ref stack (Ref.func f) |> Choice.return

  let exec_local_instr state locals stack : Binary.local_instr -> _ = function
    | Get i ->
      let stack = Stack.push stack (State.Locals.get locals i) in
      Choice.return (State.Continue { state with stack })
    | Set i ->
      let v, stack = Stack.pop stack in
      let locals = State.Locals.set locals i v in
      Choice.return (State.Continue { state with locals; stack })
    | Tee i ->
      let v, stack = Stack.pop stack in
      let locals = State.Locals.set locals i v in
      let stack = Stack.push stack v in
      Choice.return (State.Continue { state with locals; stack })

  let exec_global_instr env stack : Binary.global_instr -> _ = function
    | Get i ->
      let+ g = Env.get_global env i in
      Stack.push stack (Global.value g)
    | Set i ->
      let* global = Env.get_global env i in
      let v, stack = Stack.pop stack in
      let+ () = Global.set_value global v in
      stack

  let exec_table_instr env instr_counter stack : Binary.table_instr -> _ =
    function
    | Get tbl_i ->
      (* TODO: this should be rewritten without `select_i32` ! but it requires to change the type of `Table.get` *)
      let i, stack = Stack.pop_i32 stack in
      let* i = Choice.select_i32 i in
      let i = Int32.to_int i in
      let* t = Env.get_table env tbl_i in
      let size = Table.size t in
      if i < 0 || i >= size then Choice.trap `Out_of_bounds_table_access
      else
        let* t = Env.get_table env tbl_i in
        let v = Table.get t i in
        Stack.push stack (Ref v) |> Choice.return
    | Set tbl_indice ->
      let v, stack = Stack.pop_as_ref stack in
      let indice, stack = Stack.pop_i32 stack in
      (* TODO: avoid the select_i32, it requires to change the type of `Table.set` *)
      let* indice = Choice.select_i32 indice in
      let indice = Int32.to_int indice in
      let* t = Env.get_table env tbl_indice in
      if indice < 0 || indice >= Table.size t then
        Choice.trap `Out_of_bounds_table_access
      else begin
        let* t = Env.get_table env tbl_indice in
        let+ () = Table.set t indice v in
        stack
      end
    | Size indice ->
      let+ t = Env.get_table env indice in
      let size = Table.size t in
      Stack.push_i32_of_int stack size
    | Grow indice ->
      let* t = Env.get_table env indice in
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
        Stack.push_i32_of_int stack (-1) |> Choice.return
      else
        let new_element, stack = Stack.pop_as_ref stack in
        let* new_size = Choice.select_i32 new_size in
        let* t = Env.get_table env indice in
        let+ () = Table.grow t new_size new_element in
        Stack.push_i32 stack size
    | Fill indice ->
      let* t = Env.get_table env indice in
      let len, stack = Stack.pop_i32 stack in
      let x, stack = Stack.pop_as_ref stack in
      let pos, stack = Stack.pop_i32 stack in
      let>! () =
        let pos = I64.extend_i32_u pos in
        let len = I64.extend_i32_u len in
        let size = I64.extend_i32_u (I32.of_int @@ Table.size t) in
        ( I64.lt_u size I64.(add pos len)
        , `Out_of_bounds_table_access
        , Some instr_counter )
      in
      let* pos = Choice.select_i32 pos in
      let* len = Choice.select_i32 len in
      let* t = Env.get_table env indice in
      let+ () = Table.fill t pos len x in
      stack
    | Copy (ti_dst, ti_src) ->
      let* t_src = Env.get_table env ti_src in
      let* t_dst = Env.get_table env ti_dst in
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
        , Some instr_counter )
      in
      let+ () =
        let> len_eqz = I32.eqz len in
        if len_eqz then return ()
        else begin
          let* src = Choice.select_i32 src in
          let* dst = Choice.select_i32 dst in
          let* len = Choice.select_i32 len in
          let* t_src = Env.get_table env ti_src in
          let* t_dst = Env.get_table env ti_dst in
          Table.copy ~t_src ~t_dst ~src ~dst ~len
        end
      in
      stack
    | Init (t_i, e_i) ->
      let* t = Env.get_table env t_i in
      let elem = Env.get_elem env e_i in
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
        , Some instr_counter )
      in
      let* len = Choice.select_i32 len in
      let* pos_x = Choice.select_i32 pos_x in
      let* pos = Choice.select_i32 pos in
      let len = Int32.to_int len in
      let pos_x = Int32.to_int pos_x in
      let pos = Int32.to_int pos in
      let rec loop i () =
        if i = len then return ()
        else
          let elt = Elem.get elem (pos_x + i) in
          let* t = Env.get_table env t_i in
          let* () = Table.set t (pos + i) elt in
          loop (i + 1) ()
      in
      let+ () = loop 0 () in
      stack

  let exec_elem_instr env : Binary.elem_instr -> _ = function
    | Drop i ->
      let elem = Env.get_elem env i in
      Elem.drop elem

  let exec_memory_instr env instr_counter stack : Binary.memory_instr -> _ =
    function
    | Size memid ->
      let* mem = Env.get_memory env memid in
      let len = Memory.size_in_pages mem in
      Stack.push_i32 stack len |> Choice.return
    | Grow memid ->
      let* mem = Env.get_memory env memid in
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
      if too_big then Stack.push_i32 stack (I32.of_int ~-1) |> Choice.return
      else begin
        let* mem = Env.get_memory env memid in
        let* () = Memory.grow mem I64.(to_int32 delta) in
        let res = I64.(to_int32 @@ (old_size / page_size)) in
        Stack.push_i32 stack res |> Choice.return
      end
    | Fill memid ->
      let len, stack = Stack.pop_i32 stack in
      let c, stack = Stack.pop_i32 stack in
      let pos, stack = Stack.pop_i32 stack in
      let* mem = Env.get_memory env memid in
      let>! () =
        let size = I64.extend_i32_u (Memory.size mem) in
        let len = I64.extend_i32_u len in
        let pos = I64.extend_i32_u pos in
        ( I64.lt_u size I64.(add pos len)
        , `Out_of_bounds_memory_access
        , Some instr_counter )
      in
      (* TODO: should we have something like select_i8 here? or rather, mask it correctly before calling select_i32? *)
      let* c = Choice.select_i32 c in
      let c =
        let c = Int32.to_int c in
        let c = Int.abs c mod 256 in
        Char.chr c
      in
      let* mem = Env.get_memory env memid in
      let+ () = Memory.fill mem ~pos ~len c in
      stack
    | Copy (dstmemid, srcmemid) ->
      let len, stack = Stack.pop_i32 stack in
      let src_idx, stack = Stack.pop_i32 stack in
      let dst_idx, stack = Stack.pop_i32 stack in
      let* srcmem = Env.get_memory env srcmemid in
      let* dstmem = Env.get_memory env dstmemid in
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
        , Some instr_counter )
      in
      let* srcmem = Env.get_memory env srcmemid in
      let* dstmem = Env.get_memory env dstmemid in
      let+ () = Memory.blit ~src:srcmem ~src_idx ~dst:dstmem ~dst_idx ~len in
      stack
    | Init (memid, dataid) ->
      let len, stack = Stack.pop_i32 stack in
      let src, stack = Stack.pop_i32 stack in
      let dst, stack = Stack.pop_i32 stack in
      let* data = Env.get_data env dataid in
      let* mem = Env.get_memory env memid in
      let>! () =
        let memsize = I64.extend_i32_u (Memory.size mem) in
        let datasize = I64.of_int (Data.size data) in
        let len = I64.extend_i32_u len in
        let src = I64.extend_i32_u src in
        let dst = I64.extend_i32_u dst in
        ( Boolean.or_
            (I64.lt_u memsize I64.(add dst len))
            (I64.lt_u datasize I64.(add src len))
        , `Out_of_bounds_memory_access
        , Some instr_counter )
      in
      let data = Data.value data in
      let* mem = Env.get_memory env memid in
      let+ () = Memory.blit_string mem data ~src ~dst ~len in
      stack

  let exec_data_instr env : Binary.data_instr -> _ = function
    | Drop i ->
      let+ data = Env.get_data env i in
      Data.drop data

  let init_local (_id, t) : Value.t =
    match t with
    | Binary.Num_type I32 -> I32 I32.zero
    | Num_type I64 -> I64 I64.zero
    | Num_type F32 -> F32 F32.zero
    | Num_type F64 -> F64 F64.zero
    | Num_type V128 -> V128 V128.zero
    | Ref_type (_null, rt) -> Ref (Ref.null rt)

  type extern_func = Extern_func.extern_func

  let exec_extern_func env stack (f : extern_func) =
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
    in
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
      | NArg (_, _, args) -> split_one_arg args
      | Res -> ([], stack)
    in
    let rec apply : type f r.
      Stack.t -> (f, r) Extern_func.atype -> f -> r Choice.t =
     fun stack ty f ->
      match ty with
      | Mem (memid, args) ->
        let* mem = Env.get_memory env memid in
        apply stack args (f mem)
      | Arg (arg, args) ->
        let* v, stack = pop_arg stack arg in
        apply stack args (f v)
      | UArg args -> apply stack args (f ())
      | NArg (_, arg, args) ->
        let* v, stack = pop_arg stack arg in
        apply stack args (f v)
      | Res -> Choice.return f
    in
    let (Extern_func.Extern_func (Func (atype, rtype), func)) = f in
    let args, stack = split_args stack atype in
    let* r = apply (List.rev args) atype func in
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
    in
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
         State.exec_state
      -> is_loop:bool
      -> expr Annotated.t
      -> instr Annotated.t Option.t

    val continue : State.exec_state -> instr Annotated.t Option.t

    val branch : State.exec_state -> int -> instr Annotated.t Option.t
  end = struct
    let rec loop (state : State.exec_state) : instr Annotated.t Option.t =
      match state.State.pc.Annotated.raw with
      | i :: _ -> Some i
      | [] -> (
        match state.State.block_stack with
        | [] -> None
        | block :: block_stack ->
          loop { state with block_stack; pc = block.State.continue } )

    let branch (state : State.exec_state) n : instr Annotated.t Option.t =
      let block_stack = Stack.drop_n state.State.block_stack n in
      match block_stack with
      | [] -> None
      | block :: block_stack_tl ->
        let block_stack =
          if block.State.is_loop then block_stack else block_stack_tl
        in
        loop { state with block_stack; pc = block.State.branch; stack = [] }

    let continue (state : State.exec_state) = loop state

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

  let exec_block (state : State.exec_state) ~is_loop (bt : block_type option)
    expr =
    let pt, rt =
      match bt with
      | None -> ([], [])
      | Some (Bt_raw ((None | Some _), (pt, rt))) -> (List.map snd pt, rt)
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

  let exec_func ~return (state : State.exec_state) env (func : Func.t) =
    Log.info (fun m ->
      m "calling func  : func %s" (Option.value func.id ~default:"anonymous") );
    let (Bt_raw ((None | Some _), (param_type, result_type))) = func.type_f in
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
      ; env
      ; envs = state.envs
      }

  let exec_vfunc ~return (state : State.exec_state) (func : Kind.func) =
    match func with
    | Wasm { func; idx } ->
      let env = Dynarray.get state.envs idx in
      Choice.return (State.Continue (exec_func ~return state env func))
    | Extern { idx } ->
      let f = Env.get_extern_func state.env idx in
      let+ stack = exec_extern_func state.env state.stack f in
      let state = { state with stack } in
      if return then State.return state else State.Continue state

  let func_type (state : State.exec_state) (f : Kind.func) =
    match f with
    | Wasm { func; _ } ->
      let (Bt_raw ((None | Some _), t)) = func.type_f in
      t
    | Extern { idx } ->
      let f = Env.get_extern_func state.env idx in
      Extern_func.extern_type f

  let call_ref ~return:_ (_state : State.exec_state) _typ_i =
    (* TODO *)
    assert false
  (* let fun_ref, stack = Stack.pop_as_ref state.stack in *)
  (* let state = { state with stack } in *)
  (* let func = *)
  (*   match fun_ref with *)
  (*   | exception Invalid_argument _ -> trap "undefined element" *)
  (*   | Funcref (Some f) -> f *)
  (*   | Funcref None -> trap (Printf.sprintf "calling null function reference") *)
  (*   | _ -> trap "element type error" *)
  (* in *)
  (* let pt, rt = Func.typ func in *)
  (* let pt', rt' = typ_i in *)
  (* if not (rt = rt' && List.equal p_type_eq pt pt') then *)
  (*   trap "indirect call type mismatch"; *)
  (* exec_vfunc ~return state func *)

  let call_indirect ~return (state : State.exec_state)
    (tbl_i, (Bt_raw ((None | Some _), typ_i) : block_type)) =
    let fun_i, stack = Stack.pop_i32 state.stack in
    let state = { state with stack } in
    let* t = Env.get_table state.env tbl_i in
    let _null, ref_kind = Table.typ t in
    match ref_kind with
    | Func_ht ->
      let size = Table.size t in
      let>! () =
        ( I32.(le_u (I32.of_int size) fun_i)
        , `Undefined_element
        , (* TODO: get instr counter *) None )
      in
      let* fun_i = Choice.select_i32 fun_i in
      let* t = Env.get_table state.env tbl_i in
      let fun_i = Int32.to_int fun_i in
      let f_ref = Table.get t fun_i in
      begin match Ref.get_func f_ref with
      | Null -> Choice.trap (`Uninitialized_element fun_i)
      | Type_mismatch -> Choice.trap `Element_type_error
      | Ref_value func ->
        let ft = func_type state func in
        let ft' = typ_i in
        if not (Binary.func_type_eq ft ft') then
          Choice.trap `Indirect_call_type_mismatch
        else exec_vfunc ~return state func
      end
    | _ -> Choice.trap `Indirect_call_type_mismatch

  let exec_instr instr (state : State.exec_state) : State.instr_result Choice.t
      =
    let stack = state.stack in
    let env = state.env in
    let locals = state.locals in
    let instr_counter = Atomic.fetch_and_add instr.Annotated.instr_counter 1 in
    let ret stack = Choice.return (State.Continue { state with stack }) in
    Log.info (fun m -> m "stack         : [ %a ]" Stack.pp stack);
    Log.info (fun m ->
      m "running instr : %a (executed %d times)" (pp_instr ~short:true)
        instr.Annotated.raw instr_counter );
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
    match instr.raw with
    | I32 i ->
      (* TODO: pass ret or state directly to avoid the cost of the monad here and do the same for all cases of the match *)
      let* stack = exec_i32_instr env instr_counter stack i in
      ret stack
    | I64 i ->
      let* stack = exec_i64_instr env instr_counter stack i in
      ret stack
    | F32 i ->
      let* stack = exec_f32_instr env instr_counter stack i in
      ret stack
    | F64 i ->
      let* stack = exec_f64_instr env instr_counter stack i in
      ret stack
    | V128 i -> ret @@ exec_v128_instr stack i
    | I8x16 i -> exec_i8x16_instr stack i
    | I16x8 i -> exec_i16x8_instr stack i
    | I32x4 i -> ret @@ exec_i32x4_instr stack i
    | I64x2 i -> ret @@ exec_i64x2_instr stack i
    | Ref i ->
      let* stack = exec_ref_instr env stack i in
      ret stack
    | Local i -> exec_local_instr state locals stack i
    | Global i ->
      let* stack = exec_global_instr env stack i in
      ret stack
    | Table i ->
      let* stack = exec_table_instr env instr_counter stack i in
      ret stack
    | Elem i ->
      exec_elem_instr env i;
      ret stack
    | Memory i ->
      let* stack = exec_memory_instr env instr_counter stack i in
      ret stack
    | Data i ->
      let* () = exec_data_instr env i in
      ret stack
    | Return -> Choice.return (State.return state)
    | Nop -> Choice.return (State.Continue state)
    | Unreachable -> Choice.trap `Unreachable
    | Drop -> ret @@ Stack.drop stack
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
      let func = Env.get_func env i in
      exec_vfunc ~return:false state func
      end
    | Return_call i -> begin
      let func = Env.get_func env i in
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
    | Loop (_id, bt, e) -> exec_block state ~is_loop:true bt e
    | Block (_id, bt, e) -> exec_block state ~is_loop:false bt e
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
      call_indirect ~return:false state (tbl_i, typ_i)
    | Return_call_indirect (tbl_i, typ_i) ->
      call_indirect ~return:true state (tbl_i, typ_i)
    | Call_ref typ_i -> call_ref ~return:false state typ_i
    | Return_call_ref typ_i -> call_ref ~return:true state typ_i
    | ( Ref_i31 | I31_get_s | I31_get_u | Struct_new _ | Struct_new_default _
      | Struct_get (_, _)
      | Struct_get_s (_, _)
      | Struct_get_u (_, _)
      | Struct_set (_, _)
      | Array_new _ | Array_new_default _
      | Array_new_fixed (_, _)
      | Array_new_data (_, _)
      | Array_new_elem (_, _)
      | Array_get _ | Array_get_s _ | Array_get_u _ | Array_set _ | Array_len
      | Array_fill _
      | Array_copy (_, _)
      | Array_init_data (_, _)
      | Array_init_elem (_, _)
      | Any_convert_extern | Extern_convert_any ) as i ->
      Log.err (fun m ->
        m "unimplemented instruction: %a" (pp_instr ~short:false) i );
      assert false

  let rec loop ~heartbeat (state : State.exec_state) =
    let* () =
      match heartbeat with None -> Choice.return () | Some f -> f ()
    in
    match state.pc.raw with
    | instr :: pc -> begin
      let pc = Annotated.dummy pc in
      let* state = exec_instr instr { state with pc } in
      match state with
      | State.Continue state -> loop ~heartbeat state
      | State.Return res -> Choice.return res
      end
    | [] -> (
      let* next_state = State.end_block state in
      match next_state with
      | State.Continue state -> loop ~heartbeat state
      | State.Return res -> Choice.return res )

  let exec_expr ~heartbeat envs env locals stack expr bt =
    let state : State.exec_state =
      let func_rt = match bt with None -> [] | Some rt -> rt in
      { stack
      ; locals
      ; env
      ; envs
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

  let modul (link_state : 'f Link.State.t) (modul : 'extern_func Linked.Module.t)
    : unit Choice.t =
    let envs = Link.State.get_envs link_state in
    let heartbeat = make_heartbeat () in
    Log.info (fun m -> m "interpreting ...");
    try
      begin
        List.fold_left
          (fun (acc : unit Choice.t) to_run ->
            (* WARN: it can be tempting to remove the next line, but you shouldn't! (trust me, I've tried before... )*)
            let* () = acc in
            let+ _end_stack =
              let env = modul.env in
              exec_expr ~heartbeat envs env (State.Locals.of_list [])
                Stack.empty to_run None
            in
            () )
          (Choice.return ()) modul.to_run
      end
    with Stack_overflow -> Choice.trap `Call_stack_exhausted

  let exec_vfunc_from_outside ~locals ~env ~envs (func : Kind.func) :
    _ list Choice.t =
    let env = Dynarray.get envs env in
    let exec_state = State.empty_exec_state ~locals ~env ~envs in
    try
      begin
        let* state =
          match func with
          | Kind.Wasm { func; idx } ->
            let env = Dynarray.get exec_state.State.envs idx in
            let state = State.{ exec_state with stack = locals } in
            Choice.return
              (State.Continue (exec_func ~return:true state env func))
          | Extern { idx } ->
            let f = Env.get_extern_func exec_state.env idx in
            let+ stack = exec_extern_func exec_state.env exec_state.stack f in
            let state = State.{ exec_state with stack } in
            State.return state
        in
        match state with
        | State.Return res -> Choice.return res
        | State.Continue state -> loop ~heartbeat:None state
      end
    with Stack_overflow -> Choice.trap `Call_stack_exhausted
end

module Concrete (Parameters : Parameters) =
  Make [@inlined hint] (Concrete_value) (Concrete_data) (Concrete_elem)
    (Concrete_choice)
    (Concrete_table)
    (Concrete_global)
    (Concrete_memory)
    (Concrete_extern_func)
    (Concrete_env)
    (Parameters)
module Symbolic (Parameters : Parameters) =
  Make [@inlined hint] (Symbolic_value) (Symbolic_data) (Symbolic_elem)
    (Symbolic_choice)
    (Symbolic_table)
    (Symbolic_global)
    (Symbolic_memory)
    (Symbolic_extern_func)
    (Symbolic_env)
    (Parameters)
