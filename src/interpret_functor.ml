[@@@ocaml.warning "-32-33"]

open Types
open Simplified
module Env = Link.Env

module Intf = Interpret_functor_intf

module Make (P : Intf.P) : Intf.S = struct

module Int32 = P.Value.I32
module Int64 = P.Value.I64
module Float32 = P.Value.F32
module Float64 = P.Value.F64
module Stack = Stack_functor.Make(P.Value)

module Log : sig
  [@@@ocaml.warning "-32"]

  type do_not_use

  include module type of Log

  val debug : do_not_use
end = struct
  type do_not_use = unit

  include Log

  let debug = ()
end

let page_size = 65_536

let p_type_eq (_id1, t1) (_id2, t2) = t1 = t2

let trap msg = raise (Trap msg)

let exec_iunop stack nn op =
  match nn with
  | S32 ->
    let n, stack = Stack.pop_i32 stack in
    let res =
      let open Int32 in
      match op with Clz -> clz n | Ctz -> ctz n | Popcnt -> popcnt n
    in
    Stack.push_i32 stack res
  | S64 ->
    let n, stack = Stack.pop_i64 stack in
    let res =
      let open Int64 in
      match op with Clz -> clz n | Ctz -> ctz n | Popcnt -> popcnt n
    in
    Stack.push_i64 stack res

let exec_funop stack nn op =
  match nn with
  | S32 ->
    let open Float32 in
    let f, stack = Stack.pop_f32 stack in
    let res =
      match op with
      | Abs -> abs f
      | Neg -> neg f
      | Sqrt -> sqrt f
      | Ceil -> ceil f
      | Floor -> floor f
      | Trunc -> trunc f
      | Nearest -> nearest f
    in
    Stack.push_f32 stack res
  | S64 ->
    let open Float64 in
    let f, stack = Stack.pop_f64 stack in
    let res =
      match op with
      | Abs -> abs f
      | Neg -> neg f
      | Sqrt -> sqrt f
      | Ceil -> ceil f
      | Floor -> floor f
      | Trunc -> trunc f
      | Nearest -> nearest f
    in
    Stack.push_f64 stack res

let exec_ibinop stack nn (op : ibinop) =
  match nn with
  | S32 ->
    let (n1, n2), stack = Stack.pop2_i32 stack in
    Stack.push_i32 stack
      (let open Int32 in
       match op with
       | Add -> add n1 n2
       | Sub -> sub n1 n2
       | Mul -> mul n1 n2
       | Div s -> begin
         try
           match s with
           | S ->
             failwith "TODO div_s_32"
             (* if n1 = Int32.min_int && n2 = -1l then trap "integer overflow"; *)
             (* div n1 n2 *)
           | U -> unsigned_div n1 n2
         with Division_by_zero -> trap "integer divide by zero"
       end
       | Rem s -> begin
         try match s with S -> rem n1 n2 | U -> unsigned_rem n1 n2
         with Division_by_zero -> trap "integer divide by zero"
       end
       | And -> logand n1 n2
       | Or -> logor n1 n2
       | Xor -> logxor n1 n2
       | Shl -> shl n1 n2
       | Shr S -> shr_s n1 n2
       | Shr U -> shr_u n1 n2
       | Rotl -> rotl n1 n2
       | Rotr -> rotr n1 n2 )
  | S64 ->
    let (n1, n2), stack = Stack.pop2_i64 stack in
    Stack.push_i64 stack
      (let open Int64 in
       match op with
       | Add -> add n1 n2
       | Sub -> sub n1 n2
       | Mul -> mul n1 n2
       | Div s -> begin
         try
           match s with
           | S ->
             failwith "TODO div_s_64"
             (* if n1 = Int64.min_int && n2 = -1L then trap "integer overflow"; *)
             (* div n1 n2 *)
           | U -> unsigned_div n1 n2
         with Division_by_zero -> trap "integer divide by zero"
       end
       | Rem s -> begin
         try match s with S -> rem n1 n2 | U -> unsigned_rem n1 n2
         with Division_by_zero -> trap "integer divide by zero"
       end
       | And -> logand n1 n2
       | Or -> logor n1 n2
       | Xor -> logxor n1 n2
       | Shl -> shl n1 n2
       | Shr S -> shr_s n1 n2
       | Shr U -> shr_u n1 n2
       | Rotl -> rotl n1 n2
       | Rotr -> rotr n1 n2 )

let exec_fbinop stack nn (op : fbinop) =
  match nn with
  | S32 ->
    let (f1, f2), stack = Stack.pop2_f32 stack in
    Stack.push_f32 stack
      (let open Float32 in
       match op with
       | Add -> add f1 f2
       | Sub -> sub f1 f2
       | Mul -> mul f1 f2
       | Div -> div f1 f2
       | Min -> min f1 f2
       | Max -> max f1 f2
       | Copysign -> copy_sign f1 f2 )
  | S64 ->
    let (f1, f2), stack = Stack.pop2_f64 stack in
    Stack.push_f64 stack
      (let open Float64 in
       match op with
       | Add -> add f1 f2
       | Sub -> sub f1 f2
       | Mul -> mul f1 f2
       | Div -> div f1 f2
       | Min -> min f1 f2
       | Max -> max f1 f2
       | Copysign -> copy_sign f1 f2 )

let exec_itestop stack nn op =
  match nn with
  | S32 ->
    let n, stack = Stack.pop_i32 stack in
    let res = match op with Eqz -> Int32.eq_const n 0l in
    Stack.push_bool stack res
  | S64 ->
    let n, stack = Stack.pop_i64 stack in
    let res = match op with Eqz -> Int64.eq_const n 0L in
    Stack.push_bool stack res

let exec_irelop stack nn (op : irelop) =
  match nn with
  | S32 ->
    let (n1, n2), stack = Stack.pop2_i32 stack in
    let res =
      let open Int32 in
      match op with
      | Eq -> eq n1 n2
      | Ne -> ne n1 n2
      | Lt S -> lt n1 n2
      | Lt U -> lt_u n1 n2
      | Gt S -> gt n1 n2
      | Gt U -> gt_u n1 n2
      | Le S -> le n1 n2
      | Le U -> le_u n1 n2
      | Ge S -> ge n1 n2
      | Ge U -> ge_u n1 n2
    in
    Stack.push_bool stack res
  | S64 ->
    let (n1, n2), stack = Stack.pop2_i64 stack in
    let res =
      let open Int64 in
      match op with
      | Eq -> eq n1 n2
      | Ne -> ne n1 n2
      | Lt S -> lt n1 n2
      | Lt U -> lt_u n1 n2
      | Gt S -> gt n1 n2
      | Gt U -> gt_u n1 n2
      | Le S -> le n1 n2
      | Le U -> le_u n1 n2
      | Ge S -> ge n1 n2
      | Ge U -> ge_u n1 n2
    in
    Stack.push_bool stack res

let exec_frelop stack nn (op : frelop) =
  match nn with
  | S32 ->
    let (n1, n2), stack = Stack.pop2_f32 stack in
    let res =
      let open Float32 in
      match op with
      | Eq -> eq n1 n2
      | Ne -> ne n1 n2
      | Lt -> lt n1 n2
      | Gt -> gt n1 n2
      | Le -> le n1 n2
      | Ge -> ge n1 n2
    in
    Stack.push_bool stack res
  | S64 ->
    let (n1, n2), stack = Stack.pop2_f64 stack in
    let res =
      let open Float64 in
      match op with
      | Eq -> eq n1 n2
      | Ne -> ne n1 n2
      | Lt -> lt n1 n2
      | Gt -> gt n1 n2
      | Le -> le n1 n2
      | Ge -> ge n1 n2
    in
    Stack.push_bool stack res

(* let exec_itruncf stack nn nn' sx = *)
(*   match (nn, nn') with *)
(*   | S32, S32 -> ( *)
(*     let f, stack = Stack.pop_f32 stack in *)
(*     Stack.push_i32 stack *)
(*     @@ *)
(*     match sx with *)
(*     | S -> Convert.Int32.trunc_f32_s f *)
(*     | U -> Convert.Int32.trunc_f32_u f ) *)
(*   | S32, S64 -> ( *)
(*     let f, stack = Stack.pop_f64 stack in *)
(*     Stack.push_i32 stack *)
(*     @@ *)
(*     match sx with *)
(*     | S -> Convert.Int32.trunc_f64_s f *)
(*     | U -> Convert.Int32.trunc_f64_u f ) *)
(*   | S64, S32 -> ( *)
(*     let f, stack = Stack.pop_f32 stack in *)
(*     Stack.push_i64 stack *)
(*     @@ *)
(*     match sx with *)
(*     | S -> Convert.Int64.trunc_f32_s f *)
(*     | U -> Convert.Int64.trunc_f32_u f ) *)
(*   | S64, S64 -> ( *)
(*     let f, stack = Stack.pop_f64 stack in *)
(*     Stack.push_i64 stack *)
(*     @@ *)
(*     match sx with *)
(*     | S -> Convert.Int64.trunc_f64_s f *)
(*     | U -> Convert.Int64.trunc_f64_u f ) *)

(* let exec_itruncsatf stack nn nn' sx = *)
(*   match nn with *)
(*   | S32 -> begin *)
(*     match nn' with *)
(*     | S32 -> *)
(*       let n, stack = Stack.pop_f32 stack in *)
(*       let n = *)
(*         match sx with *)
(*         | S -> Convert.Int32.trunc_sat_f32_s n *)
(*         | U -> Convert.Int32.trunc_sat_f32_u n *)
(*       in *)
(*       Stack.push_i32 stack n *)
(*     | S64 -> *)
(*       let n, stack = Stack.pop_f64 stack in *)
(*       let n = *)
(*         match sx with *)
(*         | S -> Convert.Int32.trunc_sat_f64_s n *)
(*         | U -> Convert.Int32.trunc_sat_f64_u n *)
(*       in *)
(*       Stack.push_i32 stack n *)
(*   end *)
(*   | S64 -> begin *)
(*     match nn' with *)
(*     | S32 -> *)
(*       let n, stack = Stack.pop_f32 stack in *)
(*       let n = *)
(*         match sx with *)
(*         | S -> Convert.Int64.trunc_sat_f32_s n *)
(*         | U -> Convert.Int64.trunc_sat_f32_u n *)
(*       in *)
(*       Stack.push_i64 stack n *)
(*     | S64 -> *)
(*       let n, stack = Stack.pop_f64 stack in *)
(*       let n = *)
(*         match sx with *)
(*         | S -> Convert.Int64.trunc_sat_f64_s n *)
(*         | U -> Convert.Int64.trunc_sat_f64_u n *)
(*       in *)
(*       Stack.push_i64 stack n *)
(*   end *)

(* let exec_fconverti stack nn nn' sx = *)
(*   match nn with *)
(*   | S32 -> ( *)
(*     let open Convert.Float32 in *)
(*     match nn' with *)
(*     | S32 -> *)
(*       let n, stack = Stack.pop_i32 stack in *)
(*       let n = if sx = S then convert_i32_s n else convert_i32_u n in *)
(*       Stack.push_f32 stack n *)
(*     | S64 -> *)
(*       let n, stack = Stack.pop_i64 stack in *)
(*       let n = if sx = S then convert_i64_s n else convert_i64_u n in *)
(*       Stack.push_f32 stack n ) *)
(*   | S64 -> ( *)
(*     let open Convert.Float64 in *)
(*     match nn' with *)
(*     | S32 -> *)
(*       let n, stack = Stack.pop_i32 stack in *)
(*       let n = if sx = S then convert_i32_s n else convert_i32_u n in *)
(*       Stack.push_f64 stack n *)
(*     | S64 -> *)
(*       let n, stack = Stack.pop_i64 stack in *)
(*       let n = if sx = S then convert_i64_s n else convert_i64_u n in *)
(*       Stack.push_f64 stack n ) *)

(* let exec_ireinterpretf stack nn nn' = *)
(*   match nn with *)
(*   | S32 -> begin *)
(*     match nn' with *)
(*     | S32 -> *)
(*       let n, stack = Stack.pop_f32 stack in *)
(*       let n = Convert.Int32.reinterpret_f32 n in *)
(*       Stack.push_i32 stack n *)
(*     | S64 -> *)
(*       let n, stack = Stack.pop_f64 stack in *)
(*       let n = Convert.Int32.reinterpret_f32 (Convert.Float32.demote_f64 n) in *)
(*       Stack.push_i32 stack n *)
(*   end *)
(*   | S64 -> begin *)
(*     match nn' with *)
(*     | S32 -> *)
(*       let n, stack = Stack.pop_f32 stack in *)
(*       let n = Convert.Int64.reinterpret_f64 (Convert.Float64.promote_f32 n) in *)
(*       Stack.push_i64 stack n *)
(*     | S64 -> *)
(*       let n, stack = Stack.pop_f64 stack in *)
(*       let n = Convert.Int64.reinterpret_f64 n in *)
(*       Stack.push_i64 stack n *)
(*   end *)

(* let exec_freinterpreti stack nn nn' = *)
(*   match nn with *)
(*   | S32 -> begin *)
(*     match nn' with *)
(*     | S32 -> *)
(*       let n, stack = Stack.pop_i32 stack in *)
(*       let n = Convert.Float32.reinterpret_i32 n in *)
(*       Stack.push_f32 stack n *)
(*     | S64 -> *)
(*       let n, stack = Stack.pop_i64 stack in *)
(*       let n = Convert.Float32.reinterpret_i32 (Int64.to_int32 n) in *)
(*       Stack.push_f32 stack n *)
(*   end *)
(*   | S64 -> begin *)
(*     match nn' with *)
(*     | S32 -> *)
(*       let n, stack = Stack.pop_i32 stack in *)
(*       let n = Convert.Float64.reinterpret_i64 (Int64.of_int32 n) in *)
(*       Stack.push_f64 stack n *)
(*     | S64 -> *)
(*       let n, stack = Stack.pop_i64 stack in *)
(*       let n = Convert.Float64.reinterpret_i64 n in *)
(*       Stack.push_f64 stack n *)
(*   end *)

(* let init_local (_id, t) : Env.t' Value.t = *)
(*   match t with *)
(*   | Num_type I32 -> I32 Int32.zero *)
(*   | Num_type I64 -> I64 Int64.zero *)
(*   | Num_type F32 -> F32 Float32.zero *)
(*   | Num_type F64 -> F64 Float64.zero *)
(*   | Ref_type (_null, rt) -> Value.ref_null rt *)

(* (\* TODO move to module Env *\) *)
(* let mem_0 = 0 *)

(* let ( let* ) o f = Result.fold ~ok:f ~error:trap o *)

(* let get_memory (env : Env.t) idx = *)
(*   let* mem = Env.get_memory env idx in *)
(*   Ok Memory.(get_data mem, get_limit_max mem) *)

(* let get_memory_raw env idx = Env.get_memory env idx *)

(* let exec_extern_func stack (f : Value.Func.extern_func) = *)
(*   let pop_arg (type ty) stack (arg : ty Value.Func.telt) : ty * Env.t' Stack.t = *)
(*     match arg with *)
(*     | I32 -> Stack.pop_i32 stack *)
(*     | I64 -> Stack.pop_i64 stack *)
(*     | F32 -> Stack.pop_f32 stack *)
(*     | F64 -> Stack.pop_f64 stack *)
(*     | Externref ety -> Stack.pop_as_externref ety stack *)
(*   in *)
(*   let rec split_args : *)
(*     type f r. *)
(*     Env.t' Stack.t -> (f, r) Value.Func.atype -> Env.t' Stack.t * Env.t' Stack.t *)
(*       = *)
(*    fun stack ty -> *)
(*     let[@local] split_one_arg args = *)
(*       let elt, stack = Stack.pop stack in *)
(*       let elts, stack = split_args stack args in *)
(*       (elt :: elts, stack) *)
(*     in *)
(*     match ty with *)
(*     | Value.Func.Arg (_, args) -> split_one_arg args *)
(*     | NArg (_, _, args) -> split_one_arg args *)
(*     | Res -> ([], stack) *)
(*   in *)
(*   let rec apply : type f r. Env.t' Stack.t -> (f, r) Value.Func.atype -> f -> r *)
(*       = *)
(*    fun stack ty f -> *)
(*     match ty with *)
(*     | Value.Func.Arg (arg, args) -> *)
(*       let v, stack = pop_arg stack arg in *)
(*       apply stack args (f v) *)
(*     | NArg (_, arg, args) -> *)
(*       let v, stack = pop_arg stack arg in *)
(*       apply stack args (f v) *)
(*     | Res -> f *)
(*   in *)
(*   let (Extern_func (Func (atype, rtype), func)) = f in *)
(*   let args, stack = split_args stack atype in *)
(*   let r = apply (List.rev args) atype func in *)
(*   let push_val (type ty) (arg : ty Value.Func.telt) (v : ty) stack = *)
(*     match arg with *)
(*     | I32 -> Stack.push_i32 stack v *)
(*     | I64 -> Stack.push_i64 stack v *)
(*     | F32 -> Stack.push_f32 stack v *)
(*     | F64 -> Stack.push_f64 stack v *)
(*     | Externref ty -> Stack.push_as_externref stack ty v *)
(*   in *)
(*   match (rtype, r) with *)
(*   | R0, () -> stack *)
(*   | R1 t1, v1 -> push_val t1 v1 stack *)
(*   | R2 (t1, t2), (v1, v2) -> push_val t1 v1 stack |> push_val t2 v2 *)
(*   | R3 (t1, t2, t3), (v1, v2, v3) -> *)
(*     push_val t1 v1 stack |> push_val t2 v2 |> push_val t3 v3 *)
(*   | R4 (t1, t2, t3, t4), (v1, v2, v3, v4) -> *)
(*     push_val t1 v1 stack |> push_val t2 v2 |> push_val t3 v3 |> push_val t4 v4 *)

(* module State = struct *)
(*   type stack = Env.t' Stack.t *)

(*   type value = Env.t' Value.t *)

(*   type locals = value array *)

(*   type pc = instr list *)

(*   type block = *)
(*     { branch : pc *)
(*     ; branch_rt : result_type *)
(*     ; continue : pc *)
(*     ; continue_rt : result_type *)
(*     ; stack : stack *)
(*     ; is_loop : bool *)
(*     } *)

(*   type block_stack = block list *)

(*   type count = *)
(*     { name : string option *)
(*     ; mutable enter : int *)
(*     ; mutable instructions : int *)
(*     ; calls : (indice, count) Hashtbl.t *)
(*     } *)

(*   type exec_state = *)
(*     { return_state : exec_state option *)
(*     ; stack : stack *)
(*     ; locals : locals *)
(*     ; pc : pc *)
(*     ; block_stack : block_stack *)
(*     ; func_rt : result_type *)
(*     ; env : Env.t *)
(*     ; count : count *)
(*     } *)

(*   let rec print_count ppf count = *)
(*     let calls ppf tbl = *)
(*       let l = *)
(*         List.sort (fun (id1, _) (id2, _) -> compare id1 id2) *)
(*         @@ List.of_seq @@ Hashtbl.to_seq tbl *)
(*       in *)
(*       match l with *)
(*       | [] -> () *)
(*       | _ :: _ -> *)
(*         Format.fprintf ppf "@ @[<v 2>calls@ %a@]" *)
(*           (Format.pp_print_list *)
(*              ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ ") *)
(*              (fun ppf (id, count) -> *)
(*                let name ppf = function *)
(*                  | None -> () *)
(*                  | Some name -> Format.fprintf ppf " %s" name *)
(*                in *)
(*                Format.fprintf ppf "@[<v 2>id %i%a@ %a@]" id name count.name *)
(*                  print_count count ) ) *)
(*           l *)
(*     in *)
(*     Format.fprintf ppf "@[<v>enter %i@ intrs %i%a@]" count.enter *)
(*       count.instructions calls count.calls *)

(*   let empty_count name = *)
(*     { name; enter = 0; instructions = 0; calls = Hashtbl.create 0 } *)

(*   let count_instruction state = *)
(*     state.count.instructions <- state.count.instructions + 1 *)

(*   let enter_function_count count func_name func = *)
(*     let c = *)
(*       match Hashtbl.find_opt count.calls func with *)
(*       | None -> *)
(*         let c = empty_count func_name in *)
(*         Hashtbl.add count.calls func c; *)
(*         c *)
(*       | Some c -> c *)
(*     in *)
(*     c.enter <- c.enter + 1; *)
(*     c *)

(*   exception Result of value list *)

(*   let return (state : exec_state) = *)
(*     let args = Stack.keep state.stack (List.length state.func_rt) in *)
(*     match state.return_state with *)
(*     | None -> raise (Result args) *)
(*     | Some state -> *)
(*       let stack = args @ state.stack in *)
(*       { state with stack } *)

(*   let branch (state : exec_state) n = *)
(*     let block_stack = Stack.drop_n state.block_stack n in *)
(*     match block_stack with *)
(*     | [] -> return state *)
(*     | block :: block_stack_tl -> *)
(*       let block_stack = if block.is_loop then block_stack else block_stack_tl in *)
(*       let args = Stack.keep state.stack (List.length block.branch_rt) in *)
(*       let stack = args @ block.stack in *)
(*       { state with block_stack; pc = block.branch; stack } *)

(*   let end_block (state : exec_state) = *)
(*     match state.block_stack with *)
(*     | [] -> return state *)
(*     | block :: block_stack -> *)
(*       let args = Stack.keep state.stack (List.length block.continue_rt) in *)
(*       let stack = args @ block.stack in *)
(*       { state with block_stack; pc = block.continue; stack } *)
(* end *)

(* let exec_block (state : State.exec_state) ~is_loop bt expr = *)
(*   let pt, rt = *)
(*     match bt with None -> ([], []) | Some (pt, rt) -> (List.map snd pt, rt) *)
(*   in *)
(*   let block : State.block = *)
(*     let branch_rt, branch = if is_loop then (pt, expr) else (rt, state.pc) in *)
(*     { branch *)
(*     ; branch_rt *)
(*     ; continue = state.pc *)
(*     ; continue_rt = rt *)
(*     ; stack = Stack.drop_n state.stack (List.length pt) *)
(*     ; is_loop *)
(*     } *)
(*   in *)
(*   { state with pc = expr; block_stack = block :: state.block_stack } *)

(* type wasm_func = Simplified.func *)

(* let exec_func ~return ~id (state : State.exec_state) env (func : wasm_func) = *)
(*   Log.debug1 "calling func : func %s@." *)
(*     (Option.value func.id ~default:"anonymous"); *)
(*   let param_type, result_type = func.type_f in *)
(*   let args, stack = Stack.pop_n state.stack (List.length param_type) in *)
(*   let return_state = *)
(*     if return then state.return_state else Some { state with stack } *)
(*   in *)
(*   let env = Lazy.force env in *)
(*   let locals = *)
(*     Array.of_list @@ List.rev args @ List.map init_local func.locals *)
(*   in *)
(*   State. *)
(*     { stack = [] *)
(*     ; locals *)
(*     ; pc = func.body *)
(*     ; block_stack = [] *)
(*     ; func_rt = result_type *)
(*     ; return_state *)
(*     ; env *)
(*     ; count = enter_function_count state.count func.id id *)
(*     } *)

(* let exec_vfunc ~return (state : State.exec_state) (func : Env.t' Value.Func.t) = *)
(*   match func with *)
(*   | WASM (id, func, env) -> exec_func ~return ~id state env func *)
(*   | Extern f -> *)
(*     let stack = exec_extern_func state.stack f in *)
(*     let state = { state with stack } in *)
(*     if return then State.return state else state *)

(* let call_ref ~return (state : State.exec_state) typ_i = *)
(*   let fun_ref, stack = Stack.pop_as_ref state.stack in *)
(*   let state = { state with stack } in *)
(*   let func = *)
(*     match fun_ref with *)
(*     | exception Invalid_argument _ -> trap "undefined element" *)
(*     | Funcref (Some f) -> f *)
(*     | Funcref None -> trap (Printf.sprintf "calling null function reference") *)
(*     | _ -> trap "element type error" *)
(*   in *)
(*   let pt, rt = Value.Func.typ func in *)
(*   let pt', rt' = typ_i in *)
(*   if not (rt = rt' && List.equal p_type_eq pt pt') then *)
(*     trap "indirect call type mismatch"; *)
(*   exec_vfunc ~return state func *)

(* let call_indirect ~return (state : State.exec_state) (tbl_i, typ_i) = *)
(*   let fun_i, stack = Stack.pop_i32_to_int state.stack in *)
(*   let state = { state with stack } in *)
(*   let* t = Env.get_table state.env tbl_i in *)
(*   let _null, ref_kind = t.typ in *)
(*   if ref_kind <> Func_ht then trap "indirect call type mismatch"; *)
(*   let func = *)
(*     match t.data.(fun_i) with *)
(*     | exception Invalid_argument _ -> trap "undefined element" (\* fails here *\) *)
(*     | Funcref (Some f) -> f *)
(*     | Funcref None -> trap (Printf.sprintf "uninitialized element %i" fun_i) *)
(*     | _ -> trap "element type error" *)
(*   in *)
(*   let pt, rt = Value.Func.typ func in *)
(*   let pt', rt' = typ_i in *)
(*   if not (rt = rt' && List.equal p_type_eq pt pt') then *)
(*     trap "indirect call type mismatch"; *)
(*   exec_vfunc ~return state func *)

(* let exec_instr instr (state : State.exec_state) = *)
(*   State.count_instruction state; *)
(*   let stack = state.stack in *)
(*   let env = state.env in *)
(*   let locals = state.locals in *)
(*   let st stack = { state with stack } in *)
(*   Log.debug2 "stack        : [ %a ]@." Stack.pp stack; *)
(*   Log.debug2 "running instr: %a@." Simplified.Pp.instr instr; *)
(*   match instr with *)
(*   | Return -> State.return state *)
(*   | Nop -> state *)
(*   | Unreachable -> trap "unreachable" *)
(*   | I32_const n -> st @@ Stack.push_i32 stack n *)
(*   | I64_const n -> st @@ Stack.push_i64 stack n *)
(*   | F32_const f -> st @@ Stack.push_f32 stack f *)
(*   | F64_const f -> st @@ Stack.push_f64 stack f *)
(*   | I_unop (nn, op) -> st @@ exec_iunop stack nn op *)
(*   | F_unop (nn, op) -> st @@ exec_funop stack nn op *)
(*   | I_binop (nn, op) -> st @@ exec_ibinop stack nn op *)
(*   | F_binop (nn, op) -> st @@ exec_fbinop stack nn op *)
(*   | I_testop (nn, op) -> st @@ exec_itestop stack nn op *)
(*   | I_relop (nn, op) -> st @@ exec_irelop stack nn op *)
(*   | F_relop (nn, op) -> st @@ exec_frelop stack nn op *)
(*   | I_extend8_s nn -> begin *)
(*     match nn with *)
(*     | S32 -> *)
(*       let n, stack = Stack.pop_i32 stack in *)
(*       let n = Int32.extend_s 8 n in *)
(*       st @@ Stack.push_i32 stack n *)
(*     | S64 -> *)
(*       let n, stack = Stack.pop_i64 stack in *)
(*       let n = Int64.extend_s 8 n in *)
(*       st @@ Stack.push_i64 stack n *)
(*   end *)
(*   | I_extend16_s nn -> begin *)
(*     match nn with *)
(*     | S32 -> *)
(*       let n, stack = Stack.pop_i32 stack in *)
(*       let n = Int32.extend_s 16 n in *)
(*       st @@ Stack.push_i32 stack n *)
(*     | S64 -> *)
(*       let n, stack = Stack.pop_i64 stack in *)
(*       let n = Int64.extend_s 16 n in *)
(*       st @@ Stack.push_i64 stack n *)
(*   end *)
(*   | I64_extend32_s -> *)
(*     let n, stack = Stack.pop_i64 stack in *)
(*     let n = Int64.extend_s 32 n in *)
(*     st @@ Stack.push_i64 stack n *)
(*   | I32_wrap_i64 -> *)
(*     let n, stack = Stack.pop_i64 stack in *)
(*     let n = Convert.Int32.wrap_i64 n in *)
(*     st @@ Stack.push_i32 stack n *)
(*   | I64_extend_i32 s -> *)
(*     let n, stack = Stack.pop_i32 stack in *)
(*     let n = *)
(*       match s with *)
(*       | S -> Convert.Int64.extend_i32_s n *)
(*       | U -> Convert.Int64.extend_i32_u n *)
(*     in *)
(*     st @@ Stack.push_i64 stack n *)
(*   | I_trunc_f (nn, nn', s) -> st @@ exec_itruncf stack nn nn' s *)
(*   | I_trunc_sat_f (nn, nn', s) -> st @@ exec_itruncsatf stack nn nn' s *)
(*   | F32_demote_f64 -> *)
(*     let n, stack = Stack.pop_f64 stack in *)
(*     let n = Convert.Float32.demote_f64 n in *)
(*     st @@ Stack.push_f32 stack n *)
(*   | F64_promote_f32 -> *)
(*     let n, stack = Stack.pop_f32 stack in *)
(*     let n = Convert.Float64.promote_f32 n in *)
(*     st @@ Stack.push_f64 stack n *)
(*   | F_convert_i (nn, nn', s) -> st @@ exec_fconverti stack nn nn' s *)
(*   | I_reinterpret_f (nn, nn') -> st @@ exec_ireinterpretf stack nn nn' *)
(*   | F_reinterpret_i (nn, nn') -> st @@ exec_freinterpreti stack nn nn' *)
(*   | Ref_null t -> st @@ Stack.push stack (Value.ref_null t) *)
(*   | Ref_is_null -> *)
(*     let b, stack = Stack.pop_is_null stack in *)
(*     st @@ Stack.push_bool stack b *)
(*   | Ref_func i -> *)
(*     let* f = Env.get_func env i in *)
(*     st @@ Stack.push stack (Value.ref_func f) *)
(*   | Drop -> st @@ Stack.drop stack *)
(*   | Local_get i -> st @@ Stack.push stack locals.(i) *)
(*   | Local_set i -> *)
(*     let v, stack = Stack.pop stack in *)
(*     locals.(i) <- v; *)
(*     st @@ stack *)
(*   | If_else (_id, bt, e1, e2) -> *)
(*     let b, stack = Stack.pop_bool stack in *)
(*     let state = { state with stack } in *)
(*     exec_block state ~is_loop:false bt (if b then e1 else e2) *)
(*   | Call i -> begin *)
(*     let* func = Env.get_func env i in *)
(*     exec_vfunc ~return:false state func *)
(*   end *)
(*   | Return_call i -> begin *)
(*     let* func = Env.get_func env i in *)
(*     exec_vfunc ~return:true state func *)
(*   end *)
(*   | Br i -> State.branch state i *)
(*   | Br_if i -> *)
(*     let b, stack = Stack.pop_bool stack in *)
(*     let state = { state with stack } in *)
(*     if b then State.branch state i else state *)
(*   | Loop (_id, bt, e) -> exec_block state ~is_loop:true bt e *)
(*   | Block (_id, bt, e) -> exec_block state ~is_loop:false bt e *)
(*   | Memory_size -> *)
(*     let* mem, _max = get_memory env mem_0 in *)
(*     let len = Bytes.length mem / page_size in *)
(*     st @@ Stack.push_i32_of_int stack len *)
(*   | Memory_grow -> begin *)
(*     let* mem = get_memory_raw env mem_0 in *)
(*     let data = Memory.get_data mem in *)
(*     let max_size = Memory.get_limit_max mem in *)
(*     let delta, stack = Stack.pop_ui32_to_int stack in *)
(*     let delta = delta * page_size in *)
(*     let old_size = Bytes.length data in *)
(*     let new_size = old_size + delta in *)
(*     let too_big = *)
(*       if Sys.word_size = 64 then new_size >= page_size * page_size *)
(*       else if Sys.word_size = 32 then *)
(*         let page_size = Int64.of_int page_size in *)
(*         let limit = Int64.mul page_size page_size in *)
(*         let new_size = Int64.of_int new_size in *)
(*         new_size >= limit *)
(*       else Log.err "unsupported word size" *)
(*     in *)
(*     st *)
(*     @@ *)
(*     if too_big then Stack.push_i32 stack (-1l) *)
(*     else begin *)
(*       match max_size with *)
(*       | Some max when new_size > max * page_size -> Stack.push_i32 stack (-1l) *)
(*       | None | Some _ -> *)
(*         let new_mem = Bytes.extend data 0 delta in *)
(*         Bytes.fill new_mem old_size delta (Char.chr 0); *)
(*         Memory.update_memory mem new_mem; *)
(*         Stack.push_i32_of_int stack (old_size / page_size) *)
(*     end *)
(*   end *)
(*   | Memory_fill -> *)
(*     let len, stack = Stack.pop_i32_to_int stack in *)
(*     let c, stack = Stack.pop_i32_to_char stack in *)
(*     let pos, stack = Stack.pop_i32_to_int stack in *)
(*     let* mem, _max = get_memory env mem_0 in *)
(*     begin *)
(*       try Bytes.fill mem pos len c *)
(*       with Invalid_argument _ -> trap "out of bounds memory access" *)
(*     end; *)
(*     st @@ stack *)
(*   | Memory_copy -> *)
(*     let* mem, _max = get_memory env mem_0 in *)
(*     let len, stack = Stack.pop_i32_to_int stack in *)
(*     let src_pos, stack = Stack.pop_i32_to_int stack in *)
(*     let dst_pos, stack = Stack.pop_i32_to_int stack in *)
(*     begin *)
(*       try Bytes.blit mem src_pos mem dst_pos len *)
(*       with Invalid_argument _ -> trap "out of bounds memory access" *)
(*     end; *)
(*     st @@ stack *)
(*   | Memory_init i -> *)
(*     let* mem, _max = get_memory env mem_0 in *)
(*     let len, stack = Stack.pop_i32_to_int stack in *)
(*     let src_pos, stack = Stack.pop_i32_to_int stack in *)
(*     let dst_pos, stack = Stack.pop_i32_to_int stack in *)
(*     let* data = Env.get_data env i in *)
(*     ( try Bytes.blit_string data.value src_pos mem dst_pos len *)
(*       with Invalid_argument _ -> trap "out of bounds memory access" ); *)
(*     st @@ stack *)
(*   | Select _t -> *)
(*     let b, stack = Stack.pop_bool stack in *)
(*     let o2, stack = Stack.pop stack in *)
(*     let o1, stack = Stack.pop stack in *)
(*     st @@ Stack.push stack (if b then o1 else o2) *)
(*   | Local_tee i -> *)
(*     let v, stack = Stack.pop stack in *)
(*     locals.(i) <- v; *)
(*     st @@ Stack.push stack v *)
(*   | Global_get i -> *)
(*     let* g = Env.get_global env i in *)
(*     st @@ Stack.push stack g.value *)
(*   | Global_set i -> *)
(*     let* global = Env.get_global env i in *)
(*     if global.mut = Const then Log.err "Can't set const global"; *)
(*     let v, stack = *)
(*       match global.typ with *)
(*       | Ref_type _rt -> Stack.pop_ref stack *)
(*       | Num_type nt -> ( *)
(*         match nt with *)
(*         | I32 -> *)
(*           let v, stack = Stack.pop_i32 stack in *)
(*           (I32 v, stack) *)
(*         | I64 -> *)
(*           let v, stack = Stack.pop_i64 stack in *)
(*           (I64 v, stack) *)
(*         | F32 -> *)
(*           let v, stack = Stack.pop_f32 stack in *)
(*           (F32 v, stack) *)
(*         | F64 -> *)
(*           let v, stack = Stack.pop_f64 stack in *)
(*           (F64 v, stack) ) *)
(*     in *)
(*     global.value <- v; *)
(*     st @@ stack *)
(*   | Table_get indice -> *)
(*     let* t = Env.get_table env indice in *)
(*     let indice, stack = Stack.pop_i32_to_int stack in *)
(*     let v = *)
(*       match t.data.(indice) with *)
(*       | exception Invalid_argument _ -> trap "out of bounds table access" *)
(*       | v -> v *)
(*     in *)
(*     st @@ Stack.push stack (Ref v) *)
(*   | Table_set indice -> *)
(*     let* t = Env.get_table env indice in *)
(*     let v, stack = Stack.pop_as_ref stack in *)
(*     let indice, stack = Stack.pop_i32_to_int stack in *)
(*     begin *)
(*       try t.data.(indice) <- v *)
(*       with Invalid_argument _ -> trap "out of bounds table access" *)
(*     end; *)
(*     st @@ stack *)
(*   | Table_size indice -> *)
(*     let* t = Env.get_table env indice in *)
(*     st @@ Stack.push_i32_of_int stack (Array.length t.data) *)
(*   | Table_grow indice -> *)
(*     let* t = Env.get_table env indice in *)
(*     let size = Array.length t.data in *)
(*     let delta, stack = Stack.pop_i32_to_int stack in *)
(*     let new_size = size + delta in *)
(*     let allowed = *)
(*       Option.value t.limits.max ~default:Int.max_int >= new_size *)
(*       && new_size >= 0 && new_size >= size *)
(*     in *)
(*     st *)
(*     @@ *)
(*     if not allowed then *)
(*       let stack = Stack.drop stack in *)
(*       Stack.push_i32_of_int stack (-1) *)
(*     else *)
(*       let new_element, stack = Stack.pop_as_ref stack in *)
(*       let new_table = Array.make new_size new_element in *)
(*       Array.blit t.data 0 new_table 0 (Array.length t.data); *)
(*       Table.update t new_table; *)
(*       Stack.push_i32_of_int stack size *)
(*   | Table_fill indice -> *)
(*     let* t = Env.get_table env indice in *)
(*     let len, stack = Stack.pop_i32_to_int stack in *)
(*     let x, stack = Stack.pop_as_ref stack in *)
(*     let pos, stack = Stack.pop_i32_to_int stack in *)
(*     begin *)
(*       try Array.fill t.data pos len x *)
(*       with Invalid_argument _ -> trap "out of bounds table access" *)
(*     end; *)
(*     st @@ stack *)
(*   | Table_copy (ti_dst, ti_src) -> begin *)
(*     let* t_src = Env.get_table env ti_src in *)
(*     let* t_dst = Env.get_table env ti_dst in *)
(*     let len, stack = Stack.pop_i32_to_int stack in *)
(*     let src, stack = Stack.pop_i32_to_int stack in *)
(*     let dst, stack = Stack.pop_i32_to_int stack in *)
(*     if *)
(*       src + len > Array.length t_src.data || dst + len > Array.length t_dst.data *)
(*     then trap "out of bounds table access"; *)
(*     st *)
(*     @@ *)
(*     if len = 0 then stack *)
(*     else *)
(*       try *)
(*         Array.blit t_src.data src t_dst.data dst len; *)
(*         stack *)
(*       with Invalid_argument _ -> trap "out of bounds table access" *)
(*   end *)
(*   | Table_init (t_i, e_i) -> *)
(*     let* t = Env.get_table env t_i in *)
(*     let* elem = Env.get_elem env e_i in *)
(*     let len, stack = Stack.pop_i32_to_int stack in *)
(*     let pos_x, stack = Stack.pop_i32_to_int stack in *)
(*     let pos, stack = Stack.pop_i32_to_int stack in *)
(*     (\* TODO: this is dumb, why do we have to fail even when len = 0 ? *)
(*      * I don't remember where exactly but somewhere else it's the opposite: *)
(*      * if len is 0 then we do not fail... *)
(*      * if it wasn't needed, the following check would be useless *)
(*      * as the next one would take care of it *)
(*      * (or maybe not because we don't want to fail *)
(*      * in the middle of the loop but still...)*\) *)
(*     if *)
(*       pos_x + len > Array.length elem.value *)
(*       || pos + len > Array.length t.data *)
(*       || 0 > len *)
(*     then trap "out of bounds table access"; *)
(*     begin *)
(*       try *)
(*         for i = 0 to len - 1 do *)
(*           let idx = pos_x + i in *)
(*           if idx < 0 || idx >= Array.length elem.value then *)
(*             trap "out of bounds table access"; *)
(*           let x = elem.value.(idx) in *)
(*           let idx = pos + i in *)
(*           if idx < 0 || idx >= Array.length t.data then *)
(*             trap "out of bounds table access"; *)
(*           Array.set t.data idx x *)
(*         done *)
(*       with Invalid_argument _ -> trap "out of bounds table access" *)
(*     end; *)
(*     st @@ stack *)
(*   | Elem_drop i -> *)
(*     let* elem = Env.get_elem env i in *)
(*     Env.drop_elem elem; *)
(*     st @@ stack *)
(*   | I_load16 (nn, sx, { offset; _ }) -> ( *)
(*     let* mem, _max = get_memory env mem_0 in *)
(*     let pos, stack = Stack.pop_i32_to_int stack in *)
(*     if Bytes.length mem < pos + offset + 2 || offset < 0 || pos < 0 then *)
(*       trap "out of bounds memory access"; *)
(*     let res = *)
(*       (if sx = S then Bytes.get_int16_le else Bytes.get_uint16_le) *)
(*         mem (pos + offset) *)
(*     in *)
(*     st *)
(*     @@ *)
(*     match nn with *)
(*     | S32 -> Stack.push_i32_of_int stack res *)
(*     | S64 -> Stack.push_i64_of_int stack res ) *)
(*   | I_load (nn, { offset; _ }) -> ( *)
(*     let* mem, _max = get_memory env mem_0 in *)
(*     let pos, stack = Stack.pop_i32_to_int stack in *)
(*     st *)
(*     @@ *)
(*     match nn with *)
(*     | S32 -> *)
(*       if Bytes.length mem < pos + offset + 4 || offset < 0 || pos < 0 then *)
(*         trap "out of bounds memory access"; *)
(*       let res = Bytes.get_int32_le mem (pos + offset) in *)
(*       Stack.push_i32 stack res *)
(*     | S64 -> *)
(*       if Bytes.length mem < pos + offset + 8 || offset < 0 || pos < 0 then *)
(*         trap "out of bounds memory access"; *)
(*       let res = Bytes.get_int64_le mem (pos + offset) in *)
(*       Stack.push_i64 stack res ) *)
(*   | F_load (nn, { offset; _ }) -> ( *)
(*     let* mem, _max = get_memory env mem_0 in *)
(*     let pos, stack = Stack.pop_i32_to_int stack in *)
(*     st *)
(*     @@ *)
(*     match nn with *)
(*     | S32 -> *)
(*       if Bytes.length mem < pos + offset + 4 || offset < 0 || pos < 0 then *)
(*         trap "out of bounds memory access"; *)
(*       let res = Bytes.get_int32_le mem (pos + offset) in *)
(*       let res = Float32.of_bits res in *)
(*       Stack.push_f32 stack res *)
(*     | S64 -> *)
(*       if Bytes.length mem < pos + offset + 8 || offset < 0 || pos < 0 then *)
(*         trap "out of bounds memory access"; *)
(*       let res = Bytes.get_int64_le mem (pos + offset) in *)
(*       let res = Float64.of_bits res in *)
(*       Stack.push_f64 stack res ) *)
(*   | I_store (nn, { offset; _ }) -> ( *)
(*     let* mem, _max = get_memory env mem_0 in *)
(*     st *)
(*     @@ *)
(*     match nn with *)
(*     | S32 -> *)
(*       let n, stack = Stack.pop_i32 stack in *)
(*       let pos, stack = Stack.pop_i32_to_int stack in *)
(*       let offset = offset + pos in *)
(*       if Bytes.length mem < offset + 4 || pos < 0 then *)
(*         trap "out of bounds memory access"; *)
(*       Bytes.set_int32_le mem offset n; *)
(*       stack *)
(*     | S64 -> *)
(*       let n, stack = Stack.pop_i64 stack in *)
(*       let pos, stack = Stack.pop_i32_to_int stack in *)
(*       let offset = offset + pos in *)
(*       if Bytes.length mem < offset + 8 || pos < 0 then *)
(*         trap "out of bounds memory access"; *)
(*       Bytes.set_int64_le mem offset n; *)
(*       stack ) *)
(*   | F_store (nn, { offset; _ }) -> ( *)
(*     let* mem, _max = get_memory env mem_0 in *)
(*     st *)
(*     @@ *)
(*     match nn with *)
(*     | S32 -> *)
(*       let n, stack = Stack.pop_f32 stack in *)
(*       let pos, stack = Stack.pop_i32_to_int stack in *)
(*       let offset = offset + pos in *)
(*       if Bytes.length mem < offset + 4 || pos < 0 then *)
(*         trap "out of bounds memory access"; *)
(*       Bytes.set_int32_le mem offset (Float32.to_bits n); *)
(*       stack *)
(*     | S64 -> *)
(*       let n, stack = Stack.pop_f64 stack in *)
(*       let pos, stack = Stack.pop_i32_to_int stack in *)
(*       let offset = offset + pos in *)
(*       if Bytes.length mem < offset + 8 || pos < 0 then *)
(*         trap "out of bounds memory access"; *)
(*       Bytes.set_int64_le mem offset (Float64.to_bits n); *)
(*       stack ) *)
(*   | I_load8 (nn, sx, { offset; _ }) -> ( *)
(*     let* mem, _max = get_memory env mem_0 in *)
(*     let pos, stack = Stack.pop_i32_to_int stack in *)
(*     if Bytes.length mem < pos + offset + 1 || offset < 0 || pos < 0 then *)
(*       trap "out of bounds memory access"; *)
(*     let res = *)
(*       (if sx = S then Bytes.get_int8 else Bytes.get_uint8) mem (pos + offset) *)
(*     in *)
(*     st *)
(*     @@ *)
(*     match nn with *)
(*     | S32 -> Stack.push_i32_of_int stack res *)
(*     | S64 -> Stack.push_i64_of_int stack res ) *)
(*   | I64_load32 (sx, { offset; _ }) -> *)
(*     let* mem, _max = get_memory env mem_0 in *)
(*     let pos, stack = Stack.pop_i32_to_int stack in *)
(*     if Bytes.length mem < pos + offset + 4 || offset < 0 || pos < 0 then *)
(*       trap "out of bounds memory access"; *)
(*     let res = Bytes.get_int32_le mem (pos + offset) in *)
(*     if sx = S || Sys.word_size = 32 then *)
(*       let res = Int64.of_int32 res in *)
(*       st @@ Stack.push_i64 stack res *)
(*     else if Sys.word_size = 64 then *)
(*       let res = Int32.to_int res in *)
(*       let res = Int.(logand res (sub (shift_left 1 32) 1)) in *)
(*       st @@ Stack.push_i64_of_int stack res *)
(*     else Log.err "unsupported word size" *)
(*   | I_store8 (nn, { offset; _ }) -> *)
(*     let* mem, _max = get_memory env mem_0 in *)
(*     let n, stack = *)
(*       match nn with *)
(*       | S32 -> *)
(*         let n, stack = Stack.pop_i32 stack in *)
(*         (Int32.to_int n, stack) *)
(*       | S64 -> *)
(*         let n, stack = Stack.pop_i64 stack in *)
(*         (Int64.to_int n, stack) *)
(*     in *)
(*     let pos, stack = Stack.pop_i32_to_int stack in *)
(*     let offset = offset + pos in *)
(*     if Bytes.length mem < offset + 1 || pos < 0 then *)
(*       trap "out of bounds memory access"; *)
(*     Bytes.set_int8 mem offset n; *)
(*     st @@ stack *)
(*   | I_store16 (nn, { offset; _ }) -> *)
(*     let* mem, _max = get_memory env mem_0 in *)
(*     let n, stack = *)
(*       match nn with *)
(*       | S32 -> *)
(*         let n, stack = Stack.pop_i32 stack in *)
(*         (Int32.to_int n, stack) *)
(*       | S64 -> *)
(*         let n, stack = Stack.pop_i64 stack in *)
(*         (Int64.to_int n, stack) *)
(*     in *)
(*     let pos, stack = Stack.pop_i32_to_int stack in *)
(*     let offset = offset + pos in *)
(*     if Bytes.length mem < offset + 2 || pos < 0 then *)
(*       trap "out of bounds memory access"; *)
(*     Bytes.set_int16_le mem offset n; *)
(*     st @@ stack *)
(*   | I64_store32 { offset; _ } -> *)
(*     let* mem, _max = get_memory env mem_0 in *)
(*     let n, stack = Stack.pop_i64 stack in *)
(*     let n = Int64.to_int32 n in *)
(*     let pos, stack = Stack.pop_i32_to_int stack in *)
(*     let offset = offset + pos in *)
(*     if Bytes.length mem < offset + 4 || pos < 0 then *)
(*       trap "out of bounds memory access"; *)
(*     Bytes.set_int32_le mem offset n; *)
(*     st @@ stack *)
(*   | Data_drop i -> *)
(*     let* data = Env.get_data env i in *)
(*     Env.drop_data data; *)
(*     st @@ stack *)
(*   | Br_table (inds, i) -> *)
(*     let target, stack = Stack.pop_i32_to_int stack in *)
(*     let target = *)
(*       if target < 0 || target >= Array.length inds then i else inds.(target) *)
(*     in *)
(*     let state = { state with stack } in *)
(*     State.branch state target *)
(*   | Call_indirect (tbl_i, typ_i) -> *)
(*     call_indirect ~return:false state (tbl_i, typ_i) *)
(*   | Return_call_indirect (tbl_i, typ_i) -> *)
(*     call_indirect ~return:true state (tbl_i, typ_i) *)
(*   | Call_ref typ_i -> call_ref ~return:false state typ_i *)
(*   | Return_call_ref typ_i -> call_ref ~return:true state typ_i *)
(*   | Array_new _t -> *)
(*     let len, stack = Stack.pop_i32_to_int stack in *)
(*     let _default, stack = Stack.pop stack in *)
(*     let a = Array.init len (fun _i -> (\* TODO: use default *\) ()) in *)
(*     st @@ Stack.push_array stack a *)
(*   | Array_new_default _t -> *)
(*     let len, stack = Stack.pop_i32_to_int stack in *)
(*     let default = (\* TODO: get it from t *\) () in *)
(*     let a = Array.init len (fun _i -> default) in *)
(*     st @@ Stack.push_array stack a *)
(*   | ( Array_new_data _ | Array_new_elem _ | Array_new_fixed _ | Array_get _ *)
(*     | Array_get_u _ | Array_set _ | Array_len | I31_new | I31_get_s | I31_get_u *)
(*     | Struct_get _ | Struct_get_s _ | Struct_set _ | Struct_new _ *)
(*     | Struct_new_default _ | Extern_externalize | Extern_internalize *)
(*     | Ref_as_non_null | Ref_cast _ | Ref_test _ | Ref_eq | Br_on_cast _ *)
(*     | Br_on_cast_fail _ | Br_on_non_null _ | Br_on_null _ ) as i -> *)
(*     Log.debug2 "TODO (Interpret.exec_instr) %a@\n" Simplified.Pp.instr i; *)
(*     st @@ stack *)

(* let rec loop (state : State.exec_state) = *)
(*   let state = *)
(*     match state.pc with *)
(*     | instr :: pc -> exec_instr instr { state with pc } *)
(*     | [] -> *)
(*       Log.debug2 "stack        : [ %a ]@." Stack.pp state.stack; *)
(*       State.end_block state *)
(*   in *)
(*   loop state *)

(* let exec_expr env locals stack expr bt = *)
(*   let count = State.empty_count (Some "start") in *)
(*   count.enter <- count.enter + 1; *)
(*   let state : State.exec_state = *)
(*     let func_rt = match bt with None -> [] | Some rt -> rt in *)
(*     { stack *)
(*     ; locals *)
(*     ; env *)
(*     ; func_rt *)
(*     ; block_stack = [] *)
(*     ; pc = expr *)
(*     ; return_state = None *)
(*     ; count *)
(*     } *)
(*   in *)
(*   try loop state with State.Result args -> (args, count) *)

(* let exec_func env (func : wasm_func) args = *)
(*   Log.debug1 "calling func : func %s@." *)
(*     (Option.value func.id ~default:"anonymous"); *)
(*   let locals = *)
(*     Array.of_list @@ List.rev args @ List.map init_local func.locals *)
(*   in *)
(*   let res, count = exec_expr env locals [] func.body (Some (snd func.type_f)) in *)
(*   Log.profile "Exec func %s@.Instruction count: %i@." *)
(*     (Option.value func.id ~default:"anonymous") *)
(*     count.instructions; *)
(*   res *)

(* let exec_vfunc stack (func : Env.t' Value.Func.t) = *)
(*   match *)
(*     match func with *)
(*     | WASM (_, func, env) -> exec_func (Lazy.force env) func stack *)
(*     | Extern f -> exec_extern_func stack f *)
(*   with *)
(*   | result -> Ok result *)
(*   | exception Trap msg -> Error msg *)
(*   | exception Stack_overflow -> Error "call stack exhausted" *)

(* let modul (modul : Link.module_to_run) = *)
(*   Log.debug0 "interpreting ...@\n"; *)
(*   try *)
(*     List.iter *)
(*       (fun to_run -> *)
(*         let end_stack, count = *)
(*           exec_expr modul.env [||] Stack.empty to_run None *)
(*         in *)
(*         Log.profile "Exec module %s@.%a@." *)
(*           (Option.value modul.modul.id ~default:"anonymous") *)
(*           State.print_count count; *)
(*         match end_stack with *)
(*         | [] -> () *)
(*         | _ :: _ -> Format.eprintf "non empty stack@\n%a@." Stack.pp end_stack *)
(*         ) *)
(*       modul.to_run; *)
(*     Ok () *)
(*   with *)
(*   | Trap msg -> Error msg *)
(*   | Stack_overflow -> Error "call stack exhausted" *)

end
