(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module Stack = Abstract_stack

type t = Abstract_state.t option * Binary.instr Annotated.t option

let i32_can_be_zero ctx v =
  Option.is_some @@ Abstract_domain.assume ctx (Abstract_i32.eqz ctx v)

let eval_i32 ({ stack; ctx; invariant; _ } as state : Abstract_state.t) uuid :
  Binary.i32_instr -> _ = function
  | Const i ->
    let stack = Stack.push_i32 stack (Abstract_i32.of_int32 ctx i) in
    { state with stack }
  | Add ->
    let stack = Stack.apply_i32_i32_i32 stack (Abstract_i32.add ctx) in
    { state with stack }
  | Sub ->
    let stack = Stack.apply_i32_i32_i32 stack (Abstract_i32.sub ctx) in
    { state with stack }
  | Mul ->
    let stack = Stack.apply_i32_i32_i32 stack (Abstract_i32.mul ctx) in
    { state with stack }
  | Div S ->
    let (hd1, hd2), stack = Stack.pop2_i32 stack in
    let () =
      let possible = i32_can_be_zero ctx hd2 in
      Abstract_invariant.add_divide_by_zero_invariant invariant ~uuid ~possible
    in
    let stack = Stack.push_i32 stack (Abstract_i32.div_s ctx hd1 hd2) in
    { state with stack }
  | Div U ->
    let (hd1, hd2), stack = Stack.pop2_i32 stack in
    let () =
      let possible = i32_can_be_zero ctx hd2 in
      Abstract_invariant.add_divide_by_zero_invariant invariant ~uuid ~possible
    in
    let stack = Stack.push_i32 stack (Abstract_i32.div_u ctx hd1 hd2) in
    { state with stack }
  | Rem S ->
    let (hd1, hd2), stack = Stack.pop2_i32 stack in
    let () =
      let possible = i32_can_be_zero ctx hd2 in
      Abstract_invariant.add_divide_by_zero_invariant invariant ~uuid ~possible
    in
    let stack = Stack.push_i32 stack (Abstract_i32.rem_s ctx hd1 hd2) in
    { state with stack }
  | Rem U ->
    let (hd1, hd2), stack = Stack.pop2_i32 stack in
    let () =
      let possible = i32_can_be_zero ctx hd2 in
      Abstract_invariant.add_divide_by_zero_invariant invariant ~uuid ~possible
    in
    let stack = Stack.push_i32 stack (Abstract_i32.rem_u ctx hd1 hd2) in
    { state with stack }
  | And ->
    let stack = Stack.apply_i32_i32_i32 stack (Abstract_i32.and_ ctx) in
    { state with stack }
  | Or ->
    let stack = Stack.apply_i32_i32_i32 stack (Abstract_i32.or_ ctx) in
    { state with stack }
  | Shl ->
    let stack = Stack.apply_i32_i32_i32 stack (Abstract_i32.shl ctx) in
    { state with stack }
  | Lt S ->
    let stack = Stack.apply_i32_i32_boolean stack ctx (Abstract_i32.lt_s ctx) in
    { state with stack }
  | Gt S ->
    let stack = Stack.apply_i32_i32_boolean stack ctx (Abstract_i32.gt_s ctx) in
    { state with stack }
  | Lt U ->
    let stack = Stack.apply_i32_i32_boolean stack ctx (Abstract_i32.lt_u ctx) in
    { state with stack }
  | Gt U ->
    let stack = Stack.apply_i32_i32_boolean stack ctx (Abstract_i32.gt_u ctx) in
    { state with stack }
  | Le S ->
    let stack = Stack.apply_i32_i32_boolean stack ctx (Abstract_i32.le_s ctx) in
    { state with stack }
  | Ge S ->
    let stack = Stack.apply_i32_i32_boolean stack ctx (Abstract_i32.ge_s ctx) in
    { state with stack }
  | Le U ->
    let stack = Stack.apply_i32_i32_boolean stack ctx (Abstract_i32.le_u ctx) in
    { state with stack }
  | Ge U ->
    let stack = Stack.apply_i32_i32_boolean stack ctx (Abstract_i32.ge_u ctx) in
    { state with stack }
  | Ne ->
    let stack = Stack.apply_i32_i32_boolean stack ctx (Abstract_i32.ne ctx) in
    { state with stack }
  | Eqz ->
    let stack = Stack.apply_i32_boolean stack ctx (Abstract_i32.eqz ctx) in
    { state with stack }
  | Eq ->
    let stack = Stack.apply_i32_i32_boolean stack ctx (Abstract_i32.eq ctx) in
    { state with stack }
  | Store _ ->
    (* TODO: handle this correctly *)
    let _, stack = Stack.pop2_i32 stack in
    let stack = Stack.push_i32 stack (Abstract_i32.unknown ctx) in
    { state with stack }
  | Load _ ->
    (* TODO: handle this correctly *)
    let _, stack = Stack.pop_i32 stack in
    let stack = Stack.push_i32 stack (Abstract_i32.unknown ctx) in
    { state with stack }
  | instr ->
    Fmt.epr "not implemented yet: %a" Binary.pp_i32_instr instr;
    assert false

let i64_can_be_zero ctx v =
  Option.is_some @@ Abstract_domain.assume ctx (Abstract_i64.eqz ctx v)

let eval_i64 ({ stack; ctx; invariant; _ } as state : Abstract_state.t) uuid :
  Binary.i64_instr -> _ = function
  | Const i ->
    let stack = Stack.push_i64 stack (Abstract_i64.of_int64 ctx i) in
    { state with stack }
  | Add ->
    let stack = Stack.apply_i64_i64_i64 stack (Abstract_i64.add ctx) in
    { state with stack }
  | Sub ->
    let stack = Stack.apply_i64_i64_i64 stack (Abstract_i64.sub ctx) in
    { state with stack }
  | Mul ->
    let stack = Stack.apply_i64_i64_i64 stack (Abstract_i64.mul ctx) in
    { state with stack }
  | Div S ->
    let (hd1, hd2), stack = Stack.pop2_i64 stack in
    let () =
      let possible = i64_can_be_zero ctx hd2 in
      Abstract_invariant.add_divide_by_zero_invariant invariant ~uuid ~possible
    in
    let stack = Stack.push_i64 stack (Abstract_i64.div_s ctx hd1 hd2) in
    { state with stack }
  | Div U ->
    let (hd1, hd2), stack = Stack.pop2_i64 stack in
    let () =
      let possible = i64_can_be_zero ctx hd2 in
      Abstract_invariant.add_divide_by_zero_invariant invariant ~uuid ~possible
    in
    let stack = Stack.push_i64 stack (Abstract_i64.div_u ctx hd1 hd2) in
    { state with stack }
  | Rem S ->
    let (hd1, hd2), stack = Stack.pop2_i64 stack in
    let () =
      let possible = i64_can_be_zero ctx hd2 in
      Abstract_invariant.add_divide_by_zero_invariant invariant ~uuid ~possible
    in
    let stack = Stack.push_i64 stack (Abstract_i64.rem_s ctx hd1 hd2) in
    { state with stack }
  | Rem U ->
    let (hd1, hd2), stack = Stack.pop2_i64 stack in
    let () =
      let possible = i64_can_be_zero ctx hd2 in
      Abstract_invariant.add_divide_by_zero_invariant invariant ~uuid ~possible
    in
    let stack = Stack.push_i64 stack (Abstract_i64.rem_u ctx hd1 hd2) in
    { state with stack }
  | And ->
    let stack = Stack.apply_i64_i64_i64 stack (Abstract_i64.and_ ctx) in
    { state with stack }
  | Or ->
    let stack = Stack.apply_i64_i64_i64 stack (Abstract_i64.or_ ctx) in
    { state with stack }
  | Lt S ->
    let stack = Stack.apply_i64_i64_boolean stack ctx (Abstract_i64.lt_s ctx) in
    { state with stack }
  | Lt U ->
    let stack = Stack.apply_i64_i64_boolean stack ctx (Abstract_i64.lt_u ctx) in
    { state with stack }
  | Le S ->
    let stack = Stack.apply_i64_i64_boolean stack ctx (Abstract_i64.le_s ctx) in
    { state with stack }
  | Le U ->
    let stack = Stack.apply_i64_i64_boolean stack ctx (Abstract_i64.le_u ctx) in
    { state with stack }
  | Store _ ->
    (* TODO: handle this correctly *)
    let _, stack = Stack.pop2_i64 stack in
    let stack = Stack.push_i64 stack (Abstract_i64.unknown ctx) in
    { state with stack }
  | Load _ ->
    (* TODO: handle this correctly *)
    let _, stack = Stack.pop_i64 stack in
    let stack = Stack.push_i64 stack (Abstract_i64.unknown ctx) in
    { state with stack }
  | _ -> assert false

(* TODO: handle this correctly *)
let eval_f32 ({ stack; ctx; _ } as state : Abstract_state.t) _uuid :
  Binary.f32_instr -> _ = function
  | Const f ->
    let stack = Stack.push_f32 stack (Abstract_f32.of_float32 ctx f) in
    { state with stack }
  | Add | Sub | Mul | Div | Min | Max | Copysign ->
    let stack =
      Stack.apply_f32_f32_f32 stack (fun _ _ -> Abstract_f32.unknown ctx)
    in
    { state with stack }
  | Lt | Le | Gt | Ge | Eq | Ne ->
    let stack =
      Stack.apply_f32_f32_boolean stack ctx (fun _ _ ->
        Abstract_boolean.unknown ctx )
    in
    { state with stack }
  | Convert_i (nn, _sx) ->
    let stack =
      match nn with
      | S32 -> Stack.apply_i32_f32 stack (fun _ -> Abstract_f32.unknown ctx)
      | S64 -> Stack.apply_i64_f32 stack (fun _ -> Abstract_f32.unknown ctx)
    in
    { state with stack }
  | Abs | Neg | Ceil | Floor | Trunc | Nearest | Sqrt ->
    let stack = Stack.apply_f32_f32 stack (fun _ -> Abstract_f32.unknown ctx) in
    { state with stack }
  | Demote_f64 ->
    let stack = Stack.apply_f64_f32 stack (fun _ -> Abstract_f32.unknown ctx) in
    { state with stack }
  | Reinterpret_i S32 ->
    let stack = Stack.apply_i32_f32 stack (fun _ -> Abstract_f32.unknown ctx) in
    { state with stack }
  | Reinterpret_i S64 ->
    let stack = Stack.apply_i64_f32 stack (fun _ -> Abstract_f32.unknown ctx) in
    { state with stack }
  | Load (_i, _m) ->
    let stack = Stack.apply_i32_f32 stack (fun _ -> Abstract_f32.unknown ctx) in
    { state with stack }
  | Store (_i, _m) ->
    let _, stack = Stack.pop_f32 stack in
    let _, stack = Stack.pop_i32 stack in
    { state with stack }

(* TODO: handle this correctly *)
let eval_f64 ({ stack; ctx; _ } as state : Abstract_state.t) _uuid :
  Binary.f64_instr -> _ = function
  | Const _ ->
    let stack = Stack.push_f64 stack (Abstract_f64.unknown ctx) in
    { state with stack }
  | Add | Sub | Mul | Div | Min | Max | Copysign ->
    let stack =
      Stack.apply_f64_f64_f64 stack (fun _ _ -> Abstract_f64.unknown ctx)
    in
    { state with stack }
  | Lt | Le | Gt | Ge | Eq | Ne ->
    let stack =
      Stack.apply_f64_f64_boolean stack ctx (fun _ _ ->
        Abstract_boolean.unknown ctx )
    in
    { state with stack }
  | Convert_i (nn, _sx) ->
    let stack =
      match nn with
      | S32 -> Stack.apply_i32_f64 stack (fun _ -> Abstract_f64.unknown ctx)
      | S64 -> Stack.apply_i64_f64 stack (fun _ -> Abstract_f64.unknown ctx)
    in
    { state with stack }
  | Abs | Neg | Ceil | Floor | Trunc | Nearest | Sqrt ->
    let stack = Stack.apply_f64_f64 stack (fun _ -> Abstract_f64.unknown ctx) in
    { state with stack }
  | Promote_f32 ->
    let stack = Stack.apply_f32_f64 stack (fun _ -> Abstract_f64.unknown ctx) in
    { state with stack }
  | Reinterpret_i S32 ->
    let stack = Stack.apply_i32_f64 stack (fun _ -> Abstract_f64.unknown ctx) in
    { state with stack }
  | Reinterpret_i S64 ->
    let stack = Stack.apply_i64_f64 stack (fun _ -> Abstract_f64.unknown ctx) in
    { state with stack }
  | Load (_i, _m) ->
    let stack = Stack.apply_i32_f64 stack (fun _ -> Abstract_f64.unknown ctx) in
    { state with stack }
  | Store (_i, _m) ->
    let _, stack = Stack.pop_f64 stack in
    let _, stack = Stack.pop_i32 stack in
    { state with stack }

let eval_local ({ stack; locals; _ } as state : Abstract_state.t) :
  Binary.local_instr -> _ = function
  | Get i ->
    let v = Abstract_locals.find i locals in
    let stack = Stack.push stack v in
    { state with stack }
  | Set i ->
    let e, stack = Stack.pop stack in
    let locals = Abstract_locals.add i e locals in
    { state with stack; locals }
  | Tee i ->
    let e, stack = Stack.pop stack in
    let stack = Stack.push stack e in
    let locals = Abstract_locals.add i e locals in
    { state with stack; locals }

let eval_global ({ stack; globals; _ } as state : Abstract_state.t) :
  Binary.global_instr -> _ = function
  | Set i ->
    let e, stack = Stack.pop stack in
    let globals = Abstract_globals.add i e globals in
    { state with stack; globals }
  | Get i ->
    let v = Abstract_globals.find i globals in
    let stack = Stack.push stack v in
    { state with stack; globals }

(* TODO: handle this correctly *)
let eval_memory (state : Abstract_state.t) : Binary.memory_instr -> _ = function
  | Size _i | Grow _i | Fill _i -> state
  | Init (_i1, _i2) | Copy (_i1, _i2) -> state

(* TODO: handle this correctly *)
let eval_data (state : Abstract_state.t) : Binary.data_instr -> _ = function
  | Drop _i -> state

let eval_instr ({ stack; _ } as state : Abstract_state.t) :
  Binary.instr Annotated.t -> t =
 fun ({ raw; uuid; _ } as instr) ->
  match raw with
  | I32 instr ->
    let r = eval_i32 state uuid instr in
    (Some r, None)
  | I64 instr ->
    let r = eval_i64 state uuid instr in
    (Some r, None)
  | F32 instr ->
    let r = eval_f32 state uuid instr in
    (Some r, None)
  | F64 instr ->
    let r = eval_f64 state uuid instr in
    (Some r, None)
  | Unreachable ->
    (*TODO à gèrer proprement*)
    (None, None)
  | Local instr ->
    let state = eval_local state instr in
    (Some state, None)
  | Global instr ->
    let state = eval_global state instr in
    (Some state, None)
  | Drop ->
    let _, stack = Stack.pop stack in
    (Some { state with stack }, None)
  | Memory instr ->
    let state = eval_memory state instr in
    (Some state, None)
  | Data instr ->
    let state = eval_data state instr in
    (Some state, None)
  | Nop -> (Some state, None)
  | If_else _ | Call _ | Block _ | Loop _ | Br _ | Br_if _ | Br_table _
  | Br_on_non_null _ | Br_on_null _ ->
    (None, Some instr)
  | instr ->
    Fmt.failwith "DataAbstract_state.eval_instr not implemented for %a"
      (Binary.pp_instr ~short:true)
      instr
