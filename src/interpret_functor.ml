[@@@ocaml.warning "-32-33"]

open Types
open Simplified
module Env = Link.Env
module Intf = Interpret_functor_intf

module Make (P : Intf.P) :
  Intf.S
    with type 'a choice := 'a P.Choice.t
     and type module_to_run := P.Module_to_run.t
     and type thread := P.thread
     and type env := P.env = struct
  module Int32 = P.Value.I32
  module Int64 = P.Value.I64
  module Float32 = P.Value.F32
  module Float64 = P.Value.F64
  module Extern_func = P.Extern_func
  module Stack = Stack_functor.Make (P.Value) [@@inlined hint]
  module Choice = P.Choice
  module Global = P.Global
  module Memory = P.Memory
  module Env = P.Env

  module Int32_infix = struct
    let ( < ) = Int32.lt

    let ( + ) = Int32.add

    let ( - ) = Int32.sub

    let ( ~- ) x = P.Value.const_i32 0l - x

    let const = P.Value.const_i32
  end

  let ( let/ ) = Choice.bind

  let pop_choice stack =
    let b, stack = Stack.pop_bool stack in
    Choice.bind (Choice.select b) (fun b -> Choice.return (b, stack))

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

  let exec_ibinop (stack : Stack.t) nn (op : ibinop) : Stack.t Choice.t =
    match nn with
    | S32 ->
      let (n1, n2), stack = Stack.pop2_i32 stack in
      let/ res =
        let open Int32 in
        match op with
        | Add -> Choice.return @@ add n1 n2
        | Sub -> Choice.return @@ sub n1 n2
        | Mul -> Choice.return @@ mul n1 n2
        | Div s -> begin
          try
            match s with
            | S ->
              let/ overflow =
                Choice.select
                @@ Int32_infix.(
                     P.Value.Bool.and_
                       (eq n1 (P.Value.const_i32 Stdlib.Int32.min_int))
                       (eq n2 ~-(P.Value.const_i32 1l)) )
              in
              if overflow then Choice.trap Integer_overflow
              else Choice.return @@ div n1 n2
            | U -> Choice.return @@ unsigned_div n1 n2
          with Division_by_zero -> Choice.trap Integer_divide_by_zero
        end
        | Rem s -> begin
          try
            match s with
            | S -> Choice.return @@ rem n1 n2
            | U -> Choice.return @@ unsigned_rem n1 n2
          with Division_by_zero -> Choice.trap Integer_divide_by_zero
        end
        | And -> Choice.return @@ logand n1 n2
        | Or -> Choice.return @@ logor n1 n2
        | Xor -> Choice.return @@ logxor n1 n2
        | Shl -> Choice.return @@ shl n1 n2
        | Shr S -> Choice.return @@ shr_s n1 n2
        | Shr U -> Choice.return @@ shr_u n1 n2
        | Rotl -> Choice.return @@ rotl n1 n2
        | Rotr -> Choice.return @@ rotr n1 n2
      in
      Choice.return @@ Stack.push_i32 stack res
    | S64 ->
      let (n1, n2), stack = Stack.pop2_i64 stack in
      let/ res =
        let open Int64 in
        match op with
        | Add -> Choice.return @@ add n1 n2
        | Sub -> Choice.return @@ sub n1 n2
        | Mul -> Choice.return @@ mul n1 n2
        | Div s -> begin
          try
            match s with
            | S ->
              let/ overflow =
                Choice.select
                @@ P.Value.Bool.and_
                     (eq n1 (P.Value.const_i64 Stdlib.Int64.min_int))
                     (eq n2 (sub (P.Value.const_i64 0L) (P.Value.const_i64 1L)))
              in
              if overflow then Choice.trap Integer_overflow
              else Choice.return @@ div n1 n2
            | U -> Choice.return @@ unsigned_div n1 n2
          with Division_by_zero -> Choice.trap Integer_divide_by_zero
        end
        | Rem s -> begin
          try
            match s with
            | S -> Choice.return @@ rem n1 n2
            | U -> Choice.return @@ unsigned_rem n1 n2
          with Division_by_zero -> Choice.trap Integer_divide_by_zero
        end
        | And -> Choice.return @@ logand n1 n2
        | Or -> Choice.return @@ logor n1 n2
        | Xor -> Choice.return @@ logxor n1 n2
        | Shl -> Choice.return @@ shl n1 n2
        | Shr S -> Choice.return @@ shr_s n1 n2
        | Shr U -> Choice.return @@ shr_u n1 n2
        | Rotl -> Choice.return @@ rotl n1 n2
        | Rotr -> Choice.return @@ rotr n1 n2
      in
      Choice.return @@ Stack.push_i64 stack res

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

  let exec_itruncf stack nn nn' sx =
    match (nn, nn') with
    | S32, S32 ->
      let f, stack = Stack.pop_f32 stack in
      let res =
        match sx with S -> Int32.trunc_f32_s f | U -> Int32.trunc_f32_u f
      in
      Stack.push_i32 stack res
    | S32, S64 ->
      let f, stack = Stack.pop_f64 stack in
      let res =
        match sx with S -> Int32.trunc_f64_s f | U -> Int32.trunc_f64_u f
      in
      Stack.push_i32 stack res
    | S64, S32 ->
      let f, stack = Stack.pop_f32 stack in
      let res =
        match sx with S -> Int64.trunc_f32_s f | U -> Int64.trunc_f32_u f
      in
      Stack.push_i64 stack res
    | S64, S64 ->
      let f, stack = Stack.pop_f64 stack in
      let res =
        match sx with S -> Int64.trunc_f64_s f | U -> Int64.trunc_f64_u f
      in
      Stack.push_i64 stack res

  let exec_itruncsatf stack nn nn' sx =
    match nn with
    | S32 -> begin
      match nn' with
      | S32 ->
        let n, stack = Stack.pop_f32 stack in
        let n =
          match sx with
          | S -> Int32.trunc_sat_f32_s n
          | U -> Int32.trunc_sat_f32_u n
        in
        Stack.push_i32 stack n
      | S64 ->
        let n, stack = Stack.pop_f64 stack in
        let n =
          match sx with
          | S -> Int32.trunc_sat_f64_s n
          | U -> Int32.trunc_sat_f64_u n
        in
        Stack.push_i32 stack n
    end
    | S64 -> begin
      match nn' with
      | S32 ->
        let n, stack = Stack.pop_f32 stack in
        let n =
          match sx with
          | S -> Int64.trunc_sat_f32_s n
          | U -> Int64.trunc_sat_f32_u n
        in
        Stack.push_i64 stack n
      | S64 ->
        let n, stack = Stack.pop_f64 stack in
        let n =
          match sx with
          | S -> Int64.trunc_sat_f64_s n
          | U -> Int64.trunc_sat_f64_u n
        in
        Stack.push_i64 stack n
    end

  let exec_fconverti stack nn nn' sx =
    match nn with
    | S32 -> (
      let open Float32 in
      match nn' with
      | S32 ->
        let n, stack = Stack.pop_i32 stack in
        let n = if sx = S then convert_i32_s n else convert_i32_u n in
        Stack.push_f32 stack n
      | S64 ->
        let n, stack = Stack.pop_i64 stack in
        let n = if sx = S then convert_i64_s n else convert_i64_u n in
        Stack.push_f32 stack n )
    | S64 -> (
      let open Float64 in
      match nn' with
      | S32 ->
        let n, stack = Stack.pop_i32 stack in
        let n = if sx = S then convert_i32_s n else convert_i32_u n in
        Stack.push_f64 stack n
      | S64 ->
        let n, stack = Stack.pop_i64 stack in
        let n = if sx = S then convert_i64_s n else convert_i64_u n in
        Stack.push_f64 stack n )

  let exec_ireinterpretf stack nn nn' =
    match nn with
    | S32 -> begin
      match nn' with
      | S32 ->
        let n, stack = Stack.pop_f32 stack in
        let n = Int32.reinterpret_f32 n in
        Stack.push_i32 stack n
      | S64 ->
        let n, stack = Stack.pop_f64 stack in
        let n = Int32.reinterpret_f32 (Float32.demote_f64 n) in
        Stack.push_i32 stack n
    end
    | S64 -> begin
      match nn' with
      | S32 ->
        let n, stack = Stack.pop_f32 stack in
        let n = Int64.reinterpret_f64 (Float64.promote_f32 n) in
        Stack.push_i64 stack n
      | S64 ->
        let n, stack = Stack.pop_f64 stack in
        let n = Int64.reinterpret_f64 n in
        Stack.push_i64 stack n
    end

  let exec_freinterpreti stack nn nn' =
    match nn with
    | S32 -> begin
      match nn' with
      | S32 ->
        let n, stack = Stack.pop_i32 stack in
        let n = Float32.reinterpret_i32 n in
        Stack.push_f32 stack n
      | S64 ->
        let n, stack = Stack.pop_i64 stack in
        let n = Float32.reinterpret_i32 (Int64.to_int32 n) in
        Stack.push_f32 stack n
    end
    | S64 -> begin
      match nn' with
      | S32 ->
        let n, stack = Stack.pop_i32 stack in
        let n = Float64.reinterpret_i64 (Int64.of_int32 n) in
        Stack.push_f64 stack n
      | S64 ->
        let n, stack = Stack.pop_i64 stack in
        let n = Float64.reinterpret_i64 n in
        Stack.push_f64 stack n
    end

  let init_local (_id, t) : P.Value.t =
    match t with
    | Num_type I32 -> I32 Int32.zero
    | Num_type I64 -> I64 Int64.zero
    | Num_type F32 -> F32 Float32.zero
    | Num_type F64 -> F64 Float64.zero
    | Ref_type (_null, rt) -> P.Value.ref_null rt

  (* TODO move to module Env *)
  let mem_0 = 0

  let ( let* ) o f = Result.fold ~ok:f ~error:trap o

  type extern_func = Extern_func.extern_func

  let exec_extern_func stack (f : extern_func) =
    let pop_arg (type ty) stack (arg : ty Extern_func.telt) : ty * Stack.t =
      match arg with
      | I32 -> Stack.pop_i32 stack
      | I64 -> Stack.pop_i64 stack
      | F32 -> Stack.pop_f32 stack
      | F64 -> Stack.pop_f64 stack
      | Externref _ety -> failwith "TODO"
      (* Stack.pop_as_externref ety stack *)
    in
    let rec split_args :
      type f r. Stack.t -> (f, r) Extern_func.atype -> Stack.t * Stack.t =
     fun stack ty ->
      let[@local] split_one_arg args =
        let elt, stack = Stack.pop stack in
        let elts, stack = split_args stack args in
        (elt :: elts, stack)
      in
      match ty with
      | Extern_func.Arg (_, args) -> split_one_arg args
      | NArg (_, _, args) -> split_one_arg args
      | Res -> ([], stack)
    in
    let rec apply : type f r. Stack.t -> (f, r) Extern_func.atype -> f -> r =
     fun stack ty f ->
      match ty with
      | Extern_func.Arg (arg, args) ->
        let v, stack = pop_arg stack arg in
        apply stack args (f v)
      | NArg (_, arg, args) ->
        let v, stack = pop_arg stack arg in
        apply stack args (f v)
      | Res -> f
    in
    let (Extern_func.Extern_func (Func (atype, rtype), func)) = f in
    let args, stack = split_args stack atype in
    let r = apply (List.rev args) atype func in
    let push_val (type ty) (arg : ty Extern_func.telt) (v : ty) stack =
      match arg with
      | I32 -> Stack.push_i32 stack v
      | I64 -> Stack.push_i64 stack v
      | F32 -> Stack.push_f32 stack v
      | F64 -> Stack.push_f64 stack v
      | Externref ty -> Stack.push_as_externref stack ty v
    in
    match (rtype, r) with
    | R0, () -> stack
    | R1 t1, v1 -> push_val t1 v1 stack
    | R2 (t1, t2), (v1, v2) -> push_val t1 v1 stack |> push_val t2 v2
    | R3 (t1, t2, t3), (v1, v2, v3) ->
      push_val t1 v1 stack |> push_val t2 v2 |> push_val t3 v3
    | R4 (t1, t2, t3, t4), (v1, v2, v3, v4) ->
      push_val t1 v1 stack |> push_val t2 v2 |> push_val t3 v3 |> push_val t4 v4

  module State = struct
    type stack = Stack.t

    type value = P.Value.t

    module Locals : sig
      type t

      val of_list : value list -> t

      val get : t -> int -> value

      val set : t -> int -> value -> t
    end = struct
      type t = value array

      let of_list = Array.of_list

      let get t i = t.(i)

      let set t i v =
        let locals = Array.copy t in
        locals.(i) <- v;
        locals
    end

    type locals = Locals.t

    type pc = instr list

    type block =
      { branch : pc
      ; branch_rt : result_type
      ; continue : pc
      ; continue_rt : result_type
      ; stack : stack
      ; is_loop : bool
      }

    type block_stack = block list

    type count =
      { name : string option
      ; mutable enter : int
      ; mutable instructions : int
      ; calls : (indice, count) Hashtbl.t
      }

    type exec_state =
      { return_state : exec_state option
      ; stack : stack
      ; locals : locals
      ; pc : pc
      ; block_stack : block_stack
      ; func_rt : result_type
      ; env : P.Env.t
      ; count : count
      ; envs : P.Env.t Env_id.collection
      }

    let rec print_count ppf count =
      let calls ppf tbl =
        let l =
          List.sort (fun (id1, _) (id2, _) -> compare id1 id2)
          @@ List.of_seq @@ Hashtbl.to_seq tbl
        in
        match l with
        | [] -> ()
        | _ :: _ ->
          Format.fprintf ppf "@ @[<v 2>calls@ %a@]"
            (Format.pp_print_list
               ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ ")
               (fun ppf (id, count) ->
                 let name ppf = function
                   | None -> ()
                   | Some name -> Format.fprintf ppf " %s" name
                 in
                 Format.fprintf ppf "@[<v 2>id %i%a@ %a@]" id name count.name
                   print_count count ) )
            l
      in
      Format.fprintf ppf "@[<v>enter %i@ intrs %i%a@]" count.enter
        count.instructions calls count.calls

    let empty_count name =
      { name; enter = 0; instructions = 0; calls = Hashtbl.create 0 }

    let count_instruction state =
      state.count.instructions <- state.count.instructions + 1

    let enter_function_count count func_name func =
      let c =
        match Hashtbl.find_opt count.calls func with
        | None ->
          let c = empty_count func_name in
          Hashtbl.add count.calls func c;
          c
        | Some c -> c
      in
      c.enter <- c.enter + 1;
      c

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

  let exec_block (state : State.exec_state) ~is_loop bt expr =
    let pt, rt =
      match bt with None -> ([], []) | Some (pt, rt) -> (List.map snd pt, rt)
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

  type wasm_func = Simplified.func

  let exec_func ~return ~id (state : State.exec_state) env (func : wasm_func) =
    Log.debug1 "calling func : func %s@."
      (Option.value func.id ~default:"anonymous");
    let param_type, result_type = func.type_f in
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
      ; count = enter_function_count state.count func.id id
      }

  let exec_vfunc ~return (state : State.exec_state) (func : Func_intf.t) =
    match func with
    | WASM (id, func, env_id) ->
      let env = Env_id.get env_id state.envs in
      State.Continue (exec_func ~return ~id state env func)
    | Extern f ->
      let f = Env.get_extern_func state.env f in
      let stack = exec_extern_func state.stack f in
      let state = { state with stack } in
      if return then State.return state else State.Continue state

  (* let stack = exec_extern_func state.stack f in *)
  (* let state = { state with stack } in *)
  (* if return then State.return state else state *)

  let call_ref ~return (state : State.exec_state) typ_i =
    ignore (return, state, typ_i);
    failwith "TODO call_ref"
  (* let fun_ref, stack = Stack.pop_as_ref state.stack in *)
  (* let state = { state with stack } in *)
  (* let func = *)
  (*   match fun_ref with *)
  (*   | exception Invalid_argument _ -> trap "undefined element" *)
  (*   | Funcref (Some f) -> f *)
  (*   | Funcref None -> trap (Printf.sprintf "calling null function reference") *)
  (*   | _ -> trap "element type error" *)
  (* in *)
  (* let pt, rt = Value.Func.typ func in *)
  (* let pt', rt' = typ_i in *)
  (* if not (rt = rt' && List.equal p_type_eq pt pt') then *)
  (*   trap "indirect call type mismatch"; *)
  (* exec_vfunc ~return state func *)

  let call_indirect ~return:_ (_state : State.exec_state) (_tbl_i, _typ_i) =
    failwith "tout DUR"
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

  let exec_instr instr (state : State.exec_state) : State.instr_result Choice.t
      =
    State.count_instruction state;
    let stack = state.stack in
    let env = state.env in
    let locals = state.locals in
    let st stack = Choice.return (State.Continue { state with stack }) in
    Log.debug2 "stack        : [ %a ]@." Stack.pp stack;
    Log.debug2 "running instr: %a@." Simplified.Pp.instr instr;
    match instr with
    | Return -> Choice.return (State.return state)
    | Nop -> Choice.return (State.Continue state)
    | Unreachable -> Choice.trap Unreachable
    | I32_const n -> st @@ Stack.push_const_i32 stack n
    | I64_const n -> st @@ Stack.push_const_i64 stack n
    | F32_const f -> st @@ Stack.push_const_f32 stack f
    | F64_const f -> st @@ Stack.push_const_f64 stack f
    | I_unop (nn, op) -> st @@ exec_iunop stack nn op
    | F_unop (nn, op) -> st @@ exec_funop stack nn op
    | I_binop (nn, op) ->
      let/ stack = exec_ibinop stack nn op in
      st stack
    | F_binop (nn, op) -> st @@ exec_fbinop stack nn op
    | I_testop (nn, op) -> st @@ exec_itestop stack nn op
    | I_relop (nn, op) -> st @@ exec_irelop stack nn op
    | F_relop (nn, op) -> st @@ exec_frelop stack nn op
    | I_extend8_s nn -> begin
      match nn with
      | S32 ->
        let n, stack = Stack.pop_i32 stack in
        let n = Int32.extend_s 8 n in
        st @@ Stack.push_i32 stack n
      | S64 ->
        let n, stack = Stack.pop_i64 stack in
        let n = Int64.extend_s 8 n in
        st @@ Stack.push_i64 stack n
    end
    | I_extend16_s nn -> begin
      match nn with
      | S32 ->
        let n, stack = Stack.pop_i32 stack in
        let n = Int32.extend_s 16 n in
        st @@ Stack.push_i32 stack n
      | S64 ->
        let n, stack = Stack.pop_i64 stack in
        let n = Int64.extend_s 16 n in
        st @@ Stack.push_i64 stack n
    end
    | I64_extend32_s ->
      let n, stack = Stack.pop_i64 stack in
      let n = Int64.extend_s 32 n in
      st @@ Stack.push_i64 stack n
    | I32_wrap_i64 ->
      let n, stack = Stack.pop_i64 stack in
      let n = Int32.wrap_i64 n in
      st @@ Stack.push_i32 stack n
    | I64_extend_i32 s ->
      let n, stack = Stack.pop_i32 stack in
      let n =
        match s with S -> Int64.extend_i32_s n | U -> Int64.extend_i32_u n
      in
      st @@ Stack.push_i64 stack n
    | I_trunc_f (nn, nn', s) -> st @@ exec_itruncf stack nn nn' s
    | I_trunc_sat_f (nn, nn', s) -> st @@ exec_itruncsatf stack nn nn' s
    | F32_demote_f64 ->
      let n, stack = Stack.pop_f64 stack in
      let n = Float32.demote_f64 n in
      st @@ Stack.push_f32 stack n
    | F64_promote_f32 ->
      let n, stack = Stack.pop_f32 stack in
      let n = Float64.promote_f32 n in
      st @@ Stack.push_f64 stack n
    | F_convert_i (nn, nn', s) -> st @@ exec_fconverti stack nn nn' s
    | I_reinterpret_f (nn, nn') -> st @@ exec_ireinterpretf stack nn nn'
    | F_reinterpret_i (nn, nn') -> st @@ exec_freinterpreti stack nn nn'
    | Ref_null t -> st @@ Stack.push stack (P.Value.ref_null t)
    | Ref_is_null ->
      let r, stack = Stack.pop_as_ref stack in
      let is_null = P.Value.ref_is_null r in
      st @@ Stack.push_bool stack is_null
    | Ref_func _i -> assert false
    (*     let* f = Env.get_func env i in *)
    (*     st @@ Stack.push stack (Value.ref_func f) *)
    | Drop -> st @@ Stack.drop stack
    | Local_get i -> st @@ Stack.push stack (State.Locals.get locals i)
    | Local_set i ->
      let v, stack = Stack.pop stack in
      let locals = State.Locals.set locals i v in
      Choice.return (State.Continue { state with locals; stack })
    | If_else (_id, bt, e1, e2) ->
      let/ b, stack = pop_choice stack in
      let state = { state with stack } in
      exec_block state ~is_loop:false bt (if b then e1 else e2)
    | Call i -> begin
      let* func = P.Env.get_func env i in
      Choice.return @@ exec_vfunc ~return:false state func
    end
    | Return_call i -> begin
      let* func = P.Env.get_func env i in
      Choice.return @@ exec_vfunc ~return:true state func
    end
    | Br i -> State.branch state i
    | Br_if i ->
      let/ b, stack = pop_choice stack in
      let state = { state with stack } in
      if b then State.branch state i else Choice.return (State.Continue state)
    | Loop (_id, bt, e) -> exec_block state ~is_loop:true bt e
    | Block (_id, bt, e) -> exec_block state ~is_loop:false bt e
    | Memory_size ->
      let* mem = P.Env.get_memory env mem_0 in
      let len = P.Memory.size_in_pages mem in
      st @@ Stack.push_i32 stack len
    | Memory_grow -> assert false
    (* begin *)
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
    | Memory_fill -> assert false
    (*     let len, stack = Stack.pop_i32_to_int stack in *)
    (*     let c, stack = Stack.pop_i32_to_char stack in *)
    (*     let pos, stack = Stack.pop_i32_to_int stack in *)
    (*     let* mem, _max = get_memory env mem_0 in *)
    (*     begin *)
    (*       try Bytes.fill mem pos len c *)
    (*       with Invalid_argument _ -> trap "out of bounds memory access" *)
    (*     end; *)
    (*     st stack *)
    | Memory_copy -> assert false
    (*     let* mem, _max = get_memory env mem_0 in *)
    (*     let len, stack = Stack.pop_i32_to_int stack in *)
    (*     let src_pos, stack = Stack.pop_i32_to_int stack in *)
    (*     let dst_pos, stack = Stack.pop_i32_to_int stack in *)
    (*     begin *)
    (*       try Bytes.blit mem src_pos mem dst_pos len *)
    (*       with Invalid_argument _ -> trap "out of bounds memory access" *)
    (*     end; *)
    (*     st stack *)
    | Memory_init _i -> assert false
    (*     let* mem, _max = get_memory env mem_0 in *)
    (*     let len, stack = Stack.pop_i32_to_int stack in *)
    (*     let src_pos, stack = Stack.pop_i32_to_int stack in *)
    (*     let dst_pos, stack = Stack.pop_i32_to_int stack in *)
    (*     let* data = Env.get_data env i in *)
    (*     ( try Bytes.blit_string data.value src_pos mem dst_pos len *)
    (*       with Invalid_argument _ -> trap "out of bounds memory access" ); *)
    (*     st stack *)
    | Select _t ->
      let/ b, stack = pop_choice stack in
      let o2, stack = Stack.pop stack in
      let o1, stack = Stack.pop stack in
      st @@ Stack.push stack (if b then o1 else o2)
    | Local_tee i ->
      let v, stack = Stack.pop stack in
      let locals = State.Locals.set locals i v in
      let stack = Stack.push stack v in
      Choice.return (State.Continue { state with locals; stack })
    | Global_get i ->
      let* g = P.Env.get_global env i in
      st @@ Stack.push stack (P.Global.value g)
    | Global_set i ->
      let* global = P.Env.get_global env i in
      if P.Global.mut global = Const then Log.err "Can't set const global";
      let v, stack =
        match P.Global.typ global with
        | Ref_type _rt -> Stack.pop_ref stack
        | Num_type nt -> (
          match nt with
          | I32 ->
            let v, stack = Stack.pop_i32 stack in
            (I32 v, stack)
          | I64 ->
            let v, stack = Stack.pop_i64 stack in
            (I64 v, stack)
          | F32 ->
            let v, stack = Stack.pop_f32 stack in
            (F32 v, stack)
          | F64 ->
            let v, stack = Stack.pop_f64 stack in
            (F64 v, stack) )
      in
      P.Global.set_value global v;
      st stack
    | Table_get _indice -> assert false
    (*   let* t = P.Env.get_table env indice in *)
    (*   let indice, stack = Stack.pop_i32 stack in *)
    (*   let size = Table.size t in *)
    (*   let v = Table.get t indice in *)
    (*   (\* TODO bound check *\) *)
    (*   st @@ Stack.push stack (P.Value.Ref v) *)

    (* let/ indice = Choice.select_i32 indice in *)
    (*     let indice = Stdlib.Int32.to_int indice in *)
    (*     let v = *)
    (*       match t.data.(indice) with *)
    (*       | exception Invalid_argument _ -> trap "out of bounds table access" *)
    (*       | v -> v *)
    (*     in *)
    (*     st @@ Stack.push stack (P.Value.Ref v) *)
    | Table_set _indice -> assert false
    (*     let* t = Env.get_table env indice in *)
    (*     let v, stack = Stack.pop_as_ref stack in *)
    (*     let indice, stack = Stack.pop_i32_to_int stack in *)
    (*     begin *)
    (*       try t.data.(indice) <- v *)
    (*       with Invalid_argument _ -> trap "out of bounds table access" *)
    (*     end; *)
    (*     st stack *)
    | Table_size _indice -> assert false
    (*     let* t = Env.get_table env indice in *)
    (*     st @@ Stack.push_i32_of_int stack (Array.length t.data) *)
    | Table_grow _indice -> assert false
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
    | Table_fill _indice -> assert false
    (*     let* t = Env.get_table env indice in *)
    (*     let len, stack = Stack.pop_i32_to_int stack in *)
    (*     let x, stack = Stack.pop_as_ref stack in *)
    (*     let pos, stack = Stack.pop_i32_to_int stack in *)
    (*     begin *)
    (*       try Array.fill t.data pos len x *)
    (*       with Invalid_argument _ -> trap "out of bounds table access" *)
    (*     end; *)
    (*     st stack *)
    | Table_copy (_ti_dst, _ti_src) -> assert false
    (* begin *)
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
    | Table_init (_t_i, _e_i) -> assert false
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
    (*     st stack *)
    | Elem_drop i ->
      let* elem = P.Env.get_elem env i in
      P.Env.drop_elem elem;
      st stack
    | I_load16 (nn, sx, { offset; _ }) -> (
      let* mem = P.Env.get_memory env mem_0 in
      let pos, stack = Stack.pop_i32 stack in
      if offset < 0 then Choice.trap Out_of_bounds_memory_access
      else
        let offset = P.Value.const_i32 @@ Stdlib.Int32.of_int offset in
        let out_of_bounds =
          P.Value.Bool.or_
            Int32_infix.(Memory.size mem < pos + offset + const 2l)
            Int32_infix.(pos < const 0l)
        in
        let/ out_of_bounds = Choice.select out_of_bounds in
        if out_of_bounds then Choice.trap Out_of_bounds_memory_access
        else
          let addr = P.Value.I32.add pos offset in
          let res =
            (if sx = S then P.Memory.load_16_s else P.Memory.load_16_u) mem addr
          in
          st
          @@
          match nn with
          | S32 -> Stack.push_i32 stack res
          | S64 -> Stack.push_i64 stack (P.Value.I64.of_int32 res) )
    | I_load8 (nn, sx, { offset; _ }) -> (
      let* mem = P.Env.get_memory env mem_0 in
      let pos, stack = Stack.pop_i32 stack in
      if offset < 0 then Choice.trap Out_of_bounds_memory_access
      else
        let offset = P.Value.const_i32 @@ Stdlib.Int32.of_int offset in
        let addr = P.Value.I32.add pos offset in
        let out_of_bounds =
          P.Value.Bool.or_
            Int32_infix.(Memory.size mem < addr + const 1l)
            Int32_infix.(pos < const 0l)
        in
        let/ out_of_bounds = Choice.select out_of_bounds in
        if out_of_bounds then Choice.trap Out_of_bounds_memory_access
        else
          let res =
            (if sx = S then P.Memory.load_8_s else P.Memory.load_8_u) mem addr
          in
          st
          @@
          match nn with
          | S32 -> Stack.push_i32 stack res
          | S64 -> Stack.push_i64 stack (P.Value.I64.of_int32 res) )
    | I_store8 (nn, { offset; _ }) ->
      let* mem = P.Env.get_memory env mem_0 in
      let n, stack =
        match nn with
        | S32 ->
          let n, stack = Stack.pop_i32 stack in
          (n, stack)
        | S64 ->
          let n, stack = Stack.pop_i64 stack in
          (Int64.to_int32 n, stack)
      in
      let pos, stack = Stack.pop_i32 stack in
      let offset = P.Value.const_i32 (Stdlib.Int32.of_int offset) in
      let addr = P.Value.I32.add pos offset in
      let out_of_bounds =
        P.Value.Bool.or_
          Int32_infix.(Memory.size mem < addr + const 1l)
          Int32_infix.(pos < const 0l)
      in
      let/ out_of_bounds = Choice.select out_of_bounds in
      if out_of_bounds then Choice.trap Out_of_bounds_memory_access
      else begin
        P.Memory.store_8 mem ~addr n;
        (* Thread memory ? *)
        st stack
      end
    | I_load (nn, { offset; _ }) ->
      let* mem = P.Env.get_memory env mem_0 in
      let pos, stack = Stack.pop_i32 stack in
      let memory_length = Memory.size mem in
      let offset = P.Value.const_i32 (Stdlib.Int32.of_int offset) in
      let addr = P.Value.I32.add pos offset in
      let out_of_bounds =
        P.Value.Bool.or_
          Int32_infix.(offset < const 0l)
          Int32_infix.(pos < const 0l)
      in
      let/ out_of_bounds = Choice.select out_of_bounds in
      if out_of_bounds then Choice.trap Out_of_bounds_memory_access
      else begin
        match nn with
        | S32 ->
          let out_of_bounds =
            P.Value.I32.(lt_u memory_length (add addr (P.Value.const_i32 4l)))
          in
          let/ out_of_bounds = Choice.select out_of_bounds in
          if out_of_bounds then Choice.trap Out_of_bounds_memory_access
          else
            let res = Memory.load_32 mem (P.Value.I32.add pos offset) in
            st @@ Stack.push_i32 stack res
        | S64 ->
          let out_of_bounds =
            P.Value.I32.(lt_u memory_length (add addr (P.Value.const_i32 8l)))
          in
          let/ out_of_bounds = Choice.select out_of_bounds in
          if out_of_bounds then Choice.trap Out_of_bounds_memory_access
          else
            let res = Memory.load_64 mem (P.Value.I32.add pos offset) in
            st @@ Stack.push_i64 stack res
      end
    | F_load (nn, { offset; _ }) ->
      let* mem = P.Env.get_memory env mem_0 in
      let pos, stack = Stack.pop_i32 stack in
      let memory_length = Memory.size mem in
      let offset = P.Value.const_i32 (Stdlib.Int32.of_int offset) in
      let addr = P.Value.I32.add pos offset in
      let out_of_bounds =
        P.Value.Bool.or_
          Int32_infix.(offset < const 0l)
          Int32_infix.(pos < const 0l)
      in
      let/ out_of_bounds = Choice.select out_of_bounds in
      if out_of_bounds then Choice.trap Out_of_bounds_memory_access
      else begin
        match nn with
        | S32 ->
          let out_of_bounds =
            P.Value.I32.(lt_u memory_length (add addr (P.Value.const_i32 4l)))
          in
          let/ out_of_bounds = Choice.select out_of_bounds in
          if out_of_bounds then Choice.trap Out_of_bounds_memory_access
          else
            let res = Memory.load_32 mem addr in
            let res = P.Value.F32.of_bits res in
            st @@ Stack.push_f32 stack res
        | S64 ->
          let out_of_bounds =
            P.Value.I32.(lt_u memory_length (add addr (P.Value.const_i32 8l)))
          in
          let/ out_of_bounds = Choice.select out_of_bounds in
          if out_of_bounds then Choice.trap Out_of_bounds_memory_access
          else
            let res = Memory.load_64 mem addr in
            let res = P.Value.F64.of_bits res in
            st @@ Stack.push_f64 stack res
      end
    | I_store (nn, { offset; _ }) -> (
      let* mem = P.Env.get_memory env mem_0 in
      let memory_length = Memory.size mem in
      let offset = P.Value.const_i32 (Stdlib.Int32.of_int offset) in
      match nn with
      | S32 ->
        let n, stack = Stack.pop_i32 stack in
        let pos, stack = Stack.pop_i32 stack in
        let addr = P.Value.I32.add pos offset in
        let out_of_bounds =
          P.Value.Bool.or_
            P.Value.I32.(lt_u memory_length (add addr (P.Value.const_i32 4l)))
            P.Value.I32.(lt pos (P.Value.const_i32 0l))
        in
        let/ out_of_bounds = Choice.select out_of_bounds in
        if out_of_bounds then Choice.trap Out_of_bounds_memory_access
        else begin
          Memory.store_32 mem ~addr n;
          st stack
        end
      | S64 ->
        let n, stack = Stack.pop_i64 stack in
        let pos, stack = Stack.pop_i32 stack in
        let addr = P.Value.I32.add pos offset in
        let out_of_bounds =
          P.Value.Bool.or_
            P.Value.I32.(lt_u memory_length (add addr (P.Value.const_i32 8l)))
            P.Value.I32.(lt pos (P.Value.const_i32 0l))
        in
        let/ out_of_bounds = Choice.select out_of_bounds in
        if out_of_bounds then Choice.trap Out_of_bounds_memory_access
        else begin
          Memory.store_64 mem ~addr n;
          st stack
        end )
    | F_store (nn, { offset; _ }) -> (
      let* mem = P.Env.get_memory env mem_0 in
      let memory_length = Memory.size mem in
      let offset = P.Value.const_i32 (Stdlib.Int32.of_int offset) in
      match nn with
      | S32 ->
        let n, stack = Stack.pop_f32 stack in
        let pos, stack = Stack.pop_i32 stack in
        let addr = P.Value.I32.add pos offset in
        let out_of_bounds =
          P.Value.Bool.or_
            P.Value.I32.(lt_u memory_length (add addr (P.Value.const_i32 4l)))
            P.Value.I32.(lt pos (P.Value.const_i32 0l))
        in
        let/ out_of_bounds = Choice.select out_of_bounds in
        if out_of_bounds then Choice.trap Out_of_bounds_memory_access
        else begin
          Memory.store_32 mem ~addr (Float32.to_bits n);
          st stack
        end
      | S64 ->
        let n, stack = Stack.pop_f64 stack in
        let pos, stack = Stack.pop_i32 stack in
        let addr = P.Value.I32.add pos offset in
        let out_of_bounds =
          P.Value.Bool.or_
            P.Value.I32.(lt_u memory_length (add addr (P.Value.const_i32 8l)))
            P.Value.I32.(lt pos (P.Value.const_i32 0l))
        in
        let/ out_of_bounds = Choice.select out_of_bounds in
        if out_of_bounds then Choice.trap Out_of_bounds_memory_access
        else begin
          Memory.store_64 mem ~addr (Float64.to_bits n);
          st stack
        end )
    | I64_load32 (sx, { offset; _ }) ->
      let* mem = P.Env.get_memory env mem_0 in
      let offset = P.Value.const_i32 (Stdlib.Int32.of_int offset) in
      let memory_length = Memory.size mem in
      let pos, stack = Stack.pop_i32 stack in
      let addr = P.Value.I32.add pos offset in
      let out_of_bounds =
        P.Value.Bool.or_
          Int32_infix.(offset < const 0l)
          (P.Value.Bool.or_
             Int32_infix.(pos < const 0l)
             P.Value.I32.(lt_u memory_length (add addr (P.Value.const_i32 4l))) )
      in
      let/ out_of_bounds = Choice.select out_of_bounds in
      if out_of_bounds then Choice.trap Out_of_bounds_memory_access
      else begin
        let res = Memory.load_32 mem addr in
        if sx = S || Sys.word_size = 32 then
          let res = Int64.of_int32 res in
          st @@ Stack.push_i64 stack res
        else if Sys.word_size = 64 then
          let res =
            P.Value.I32.(
              logand res
                (sub
                   (shl (P.Value.const_i32 1l) (P.Value.const_i32 32l))
                   (P.Value.const_i32 1l) ) )
          in
          st @@ Stack.push_i64 stack (P.Value.I64.of_int32 res)
        else Log.err "unsupported word size"
      end
    | I_store16 (nn, { offset; _ }) ->
      let* mem = P.Env.get_memory env mem_0 in
      let offset = P.Value.const_i32 (Stdlib.Int32.of_int offset) in
      let memory_length = Memory.size mem in
      let n, stack =
        match nn with
        | S32 ->
          let n, stack = Stack.pop_i32 stack in
          (n, stack)
        | S64 ->
          let n, stack = Stack.pop_i64 stack in
          (P.Value.I64.to_int32 n, stack)
      in
      let pos, stack = Stack.pop_i32 stack in
      let addr = P.Value.I32.add pos offset in
      let out_of_bounds =
        P.Value.Bool.or_
          Int32_infix.(pos < const 0l)
          P.Value.I32.(lt_u memory_length (add addr (P.Value.const_i32 2l)))
      in
      let/ out_of_bounds = Choice.select out_of_bounds in
      if out_of_bounds then Choice.trap Out_of_bounds_memory_access
      else begin
        Memory.store_16 mem ~addr n;
        st stack
      end
    | I64_store32 { offset; _ } ->
      let* mem = P.Env.get_memory env mem_0 in
      let offset = P.Value.const_i32 (Stdlib.Int32.of_int offset) in
      let memory_length = Memory.size mem in
      let n, stack = Stack.pop_i64 stack in
      let n = Int64.to_int32 n in
      let pos, stack = Stack.pop_i32 stack in
      let addr = P.Value.I32.add pos offset in
      let out_of_bounds =
        P.Value.Bool.or_
          Int32_infix.(pos < const 0l)
          P.Value.I32.(lt_u memory_length (add addr (P.Value.const_i32 4l)))
      in
      let/ out_of_bounds = Choice.select out_of_bounds in
      if out_of_bounds then Choice.trap Out_of_bounds_memory_access
      else begin
        Memory.store_32 mem ~addr n;
        st stack
      end
    | Data_drop i ->
      let* data = P.Env.get_data env i in
      P.Env.drop_data data;
      st stack
    | Br_table (inds, i) ->
      let target, stack = Stack.pop_i32 stack in
      let/ target = Choice.select_i32 target in
      let target = Stdlib.Int32.to_int target in
      let target =
        if target < 0 || target >= Array.length inds then i else inds.(target)
      in
      let state = { state with stack } in
      State.branch state target
    | Call_indirect (tbl_i, typ_i) ->
      call_indirect ~return:false state (tbl_i, typ_i)
    | Return_call_indirect (tbl_i, typ_i) ->
      call_indirect ~return:true state (tbl_i, typ_i)
    | Call_ref typ_i -> call_ref ~return:false state typ_i
    | Return_call_ref typ_i -> call_ref ~return:true state typ_i
    | Array_new _t ->
      let len, stack = Stack.pop_i32 stack in
      let/ len = Choice.select_i32 len in
      let _default, stack = Stack.pop stack in
      let a =
        Array.init (Stdlib.Int32.to_int len) (fun _i ->
          (* TODO: use default *) () )
      in
      st @@ Stack.push_array stack a
    | Array_new_default _t ->
      let len, stack = Stack.pop_i32 stack in
      let/ len = Choice.select_i32 len in
      let default = (* TODO: get it from t *) () in
      let a = Array.init (Stdlib.Int32.to_int len) (fun _i -> default) in
      st @@ Stack.push_array stack a
    | ( Array_new_data _ | Array_new_elem _ | Array_new_fixed _ | Array_get _
      | Array_get_u _ | Array_set _ | Array_len | I31_new | I31_get_s
      | I31_get_u | Struct_get _ | Struct_get_s _ | Struct_set _ | Struct_new _
      | Struct_new_default _ | Extern_externalize | Extern_internalize
      | Ref_as_non_null | Ref_cast _ | Ref_test _ | Ref_eq | Br_on_cast _
      | Br_on_cast_fail _ | Br_on_non_null _ | Br_on_null _ ) as i ->
      Log.debug2 "TODO (Interpret.exec_instr) %a@\n" Simplified.Pp.instr i;
      st stack

  let rec loop (state : State.exec_state) =
    match state.pc with
    | instr :: pc -> begin
      let/ state = exec_instr instr { state with pc } in
      match state with
      | State.Continue state -> loop state
      | State.Return res -> Choice.return res
    end
    | [] -> (
      Log.debug2 "stack        : [ %a ]@." Stack.pp state.stack;
      let/ state = State.end_block state in
      match state with
      | State.Continue state -> loop state
      | State.Return res -> Choice.return res )

  let exec_expr envs env locals stack expr bt =
    let count = State.empty_count (Some "start") in
    count.enter <- count.enter + 1;
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
      ; count
      }
    in
    let/ state = loop state in
    Choice.return (state, count)

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

  let modul envs (modul : P.Module_to_run.t) =
    Log.debug0 "interpreting ...@\n";
    let/ () =
      List.fold_left
        (fun u to_run ->
          let/ () = u in
          let/ end_stack, count =
            let env = P.Module_to_run.env modul in
            exec_expr envs env (State.Locals.of_list []) Stack.empty to_run None
          in
          Log.profile "Exec module %s@.%a@."
            (Option.value (P.Module_to_run.modul modul).id ~default:"anonymous")
            State.print_count count;
          match end_stack with
          | [] -> Choice.return ()
          | _ :: _ ->
            Format.eprintf "non empty stack@\n%a@." Stack.pp end_stack;
            assert false )
        (Choice.return ())
        (P.Module_to_run.to_run modul)
    in
    Choice.return (Ok ())
  (* TODO error handling *)
  (* with *)
  (* | Trap msg -> Error msg *)
  (* | Stack_overflow -> Error "call stack exhausted" *)
end
