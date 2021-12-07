open Types
open Simplify

type env =
  { modules : module_ Array.t
  ; last_module : int option
  ; seen_modules : (string, int) Hashtbl.t
  ; registered_modules : (string, int) Hashtbl.t
  }

let empty_env =
  { modules = [||]
  ; last_module = None
  ; seen_modules = Hashtbl.create 64
  ; registered_modules = Hashtbl.create 64
  }

exception Return

let exec_iunop stack nn op =
  match nn with
  | S32 ->
    let n = Stack.pop_i32 stack in
    let res =
      match op with
      | Clz -> Op.i32_clz n
      | Ctz -> Op.i32_ctz n
      | Popcnt -> Op.i32_popcnt n
    in
    Stack.push stack (Const_I32 (Int32.of_int res))
  | S64 ->
    let n = Stack.pop_i64 stack in
    let res =
      match op with
      | Clz -> Op.i64_clz n
      | Ctz -> Op.i64_ctz n
      | Popcnt -> Op.i64_popcnt n
    in
    Stack.push stack (Const_I64 (Int64.of_int res))

let exec_funop stack nn op =
  match nn with
  | S32 ->
    let f = Stack.pop_f32 stack in
    let res =
      match op with
      | Abs -> Float.abs f
      | Neg -> Float.neg f
      | Sqrt -> Float.sqrt f
      | Ceil -> Float.ceil f
      | Floor -> Float.floor f
      | Trunc -> Float.trunc f
      | Nearest -> failwith "TODO: exec_funop Nearest"
    in
    Stack.push stack (Const_F32 res)
  | S64 ->
    let f = Stack.pop_f64 stack in
    let res =
      match op with
      | Abs -> Float.abs f
      | Neg -> Float.neg f
      | Sqrt -> Float.sqrt f
      | Ceil -> Float.ceil f
      | Floor -> Float.floor f
      | Trunc -> Float.trunc f
      | Nearest -> failwith "TODO: exec_funop Nearest"
    in
    Stack.push stack (Const_F64 res)

let exec_ibinop stack nn (op : Types.ibinop) =
  Stack.push stack
  @@
  match nn with
  | S32 ->
    let n1, n2 = Stack.pop2_i32 stack in
    Const_I32
      ( match op with
      | Add -> Int32.add n1 n2
      | Sub -> Int32.sub n1 n2
      | Mul -> Int32.mul n1 n2
      | Div _s -> Int32.div n1 n2
      | Rem _s -> failwith "TODO Rem"
      | And -> failwith "TODO And"
      | Or -> failwith "TODO Or"
      | Xor -> failwith "TODO Xor"
      | Shl -> failwith "TODO Shl"
      | Shr _s -> failwith "TODO Shr"
      | Rotl -> failwith "TODO Rotl"
      | Rotr -> failwith "TODO Rotr" )
  | S64 ->
    let n1, n2 = Stack.pop2_i64 stack in
    Const_I64
      ( match op with
      | Add -> Int64.add n1 n2
      | Sub -> Int64.sub n1 n2
      | Mul -> Int64.mul n1 n2
      | Div _s -> Int64.div n1 n2
      | Rem _s -> failwith "TODO Rem"
      | And -> failwith "TODO And"
      | Or -> failwith "TODO Or"
      | Xor -> failwith "TODO Xor"
      | Shl -> failwith "TODO Shl"
      | Shr _s -> failwith "TODO Shr"
      | Rotl -> failwith "TODO Rotl"
      | Rotr -> failwith "TODO Rotr" )

let exec_fbinop stack nn (op : Types.fbinop) =
  Stack.push stack
  @@
  match nn with
  | S32 ->
    let f1, f2 = Stack.pop2_f32 stack in
    Const_F32
      ( match op with
      | Add -> Float.add f1 f2
      | Sub -> Float.sub f1 f2
      | Mul -> Float.mul f1 f2
      | Div -> Float.div f1 f2
      | Min -> Float.min f1 f2
      | Max -> Float.max f1 f2
      | Copysign -> Float.copy_sign f1 f2 )
  | S64 ->
    let f1, f2 = Stack.pop2_f64 stack in
    Const_F64
      ( match op with
      | Add -> Float.add f1 f2
      | Sub -> Float.sub f1 f2
      | Mul -> Float.mul f1 f2
      | Div -> Float.div f1 f2
      | Min -> Float.min f1 f2
      | Max -> Float.max f1 f2
      | Copysign -> Float.copy_sign f1 f2 )

let i32_of_bool b =
  if b then
    1l
  else
    0l

let exec_itestop stack nn op =
  Stack.push stack
    (Const_I32
       ( match nn with
       | S32 -> (
         let n = Stack.pop_i32 stack in
         match op with
         | Eqz -> i32_of_bool (n = 0l) )
       | S64 -> (
         let n = Stack.pop_i64 stack in
         match op with
         | Eqz -> i32_of_bool (n = 0L) ) ) )

let exec_irelop stack nn (op : Types.irelop) =
  Stack.push stack
    (Const_I32
       ( match nn with
       | S32 -> (
         let n1, n2 = Stack.pop2_i32 stack in
         match op with
         | Eq -> i32_of_bool (n1 = n2)
         | Ne -> i32_of_bool (n1 <> n2)
         | Lt _sx -> i32_of_bool (n1 < n2)
         | Gt _sx -> i32_of_bool (n1 > n2)
         | Le _sx -> i32_of_bool (n1 <= n2)
         | Ge _sx -> i32_of_bool (n1 >= n2) )
       | S64 -> (
         let n1, n2 = Stack.pop2_i64 stack in
         match op with
         | Eq -> i32_of_bool (n1 = n2)
         | Ne -> i32_of_bool (n1 <> n2)
         | Lt _sx -> i32_of_bool (n1 < n2)
         | Gt _sx -> i32_of_bool (n1 > n2)
         | Le _sx -> i32_of_bool (n1 <= n2)
         | Ge _sx -> i32_of_bool (n1 >= n2) ) ) )

let exec_frelop stack nn (op : Types.frelop) =
  Stack.push stack
  @@ Const_I32
       ( match nn with
       | S32 -> (
         let n1, n2 = Stack.pop2_i32 stack in
         match op with
         | Eq -> i32_of_bool (n1 = n2)
         | Ne -> i32_of_bool (n1 <> n2)
         | Lt -> i32_of_bool (n1 < n2)
         | Gt -> i32_of_bool (n1 > n2)
         | Le -> i32_of_bool (n1 <= n2)
         | Ge -> i32_of_bool (n1 >= n2) )
       | S64 -> (
         let n1, n2 = Stack.pop2_i64 stack in
         match op with
         | Eq -> i32_of_bool (n1 = n2)
         | Ne -> i32_of_bool (n1 <> n2)
         | Lt -> i32_of_bool (n1 < n2)
         | Gt -> i32_of_bool (n1 > n2)
         | Le -> i32_of_bool (n1 <= n2)
         | Ge -> i32_of_bool (n1 >= n2) ) )

exception Branch of int

let indice_to_int = function
  | Raw i -> Unsigned.UInt32.to_int i
  | Symbolic id ->
    failwith @@ Format.sprintf "interpreter internal error: unbound id %s" id

let init_local (_id, t) =
  match t with
  | Num_type I32 -> Const_I32 0l
  | Num_type I64 -> Const_I64 0L
  | Num_type F32 -> Const_F32 0.
  | Num_type F64 -> Const_F64 0.
  | Ref_type Func_ref -> Const_null Func_ref
  | Ref_type Extern_ref -> Const_null Extern_ref

let rec exec_instr env module_indice locals stack instr =
  Format.printf "stack        : [ %a ]@." Stack.pp stack;
  Format.printf "running instr: %a@." Pp.instr instr;
  match instr with
  | Return -> raise Return
  | Nop -> ()
  | Unreachable -> failwith "unreachable"
  | I32_const n -> Stack.push stack (Const_I32 n)
  | I64_const n -> Stack.push stack (Const_I64 n)
  | F32_const f -> Stack.push stack (Const_F32 f)
  | F64_const f -> Stack.push stack (Const_F64 f)
  | I_unop (nn, op) -> exec_iunop stack nn op
  | F_unop (nn, op) -> exec_funop stack nn op
  | I_binop (nn, op) -> exec_ibinop stack nn op
  | F_binop (nn, op) -> exec_fbinop stack nn op
  | I_testop (nn, op) -> exec_itestop stack nn op
  | I_relop (nn, op) -> exec_irelop stack nn op
  | F_relop (nn, op) -> exec_frelop stack nn op
  | I_extend8_s _n -> failwith "TODO exec_instr"
  | I_extend16_s _n -> failwith "TODO exec_instr"
  | I64_extend32_s -> failwith "TODO exec_instr"
  | I32_wrap_i64 -> failwith "TODO exec_instr"
  | I64_extend_i32 _s -> failwith "TODO exec_instr"
  | I_trunc_f (_n, _n', _s) -> failwith "TODO exec_instr"
  | I_trunc_sat_f (_n, _n', _s) -> failwith "TODO exec_instr"
  | F32_demote_f64 -> failwith "TODO exec_instr"
  | F64_promote_f32 -> failwith "TODO exec_instr"
  | F_convert_i (_n, _n', _s) -> failwith "TODO exec_instr"
  | I_reinterpret_f (_n, _n') -> failwith "TODO exec_instr"
  | F_reinterpret_i (_n, _n') -> failwith "TODO exec_instr"
  | Ref_null t -> Stack.push stack (Const_null t)
  | Ref_is_null ->
    (* TODO: this is false, when do we return 0 ? :-) *)
    let _t = Stack.pop_const_null stack in
    Stack.push stack (Const_I32 1l)
  | Ref_func _fid -> failwith "TODO exec_instr"
  | Drop -> ignore (Stack.pop stack)
  | Local_get i -> Stack.push stack locals.(indice_to_int i)
  | Local_set i ->
    (* TODO: check type ? *)
    let v = Stack.pop stack in
    locals.(indice_to_int i) <- v
  | If_else (_bt, e1, e2) ->
    let b = Stack.pop_bool stack in
    exec_expr env module_indice locals stack
      ( if b then
        e1
      else
        e2 )
      false
  | Call i ->
    let module_ = env.modules.(module_indice) in
    let func = module_.funcs.(indice_to_int i) in
    let param_type, _result_type =
      match func.type_f with
      | FTId _i -> failwith "TODO"
      | FTFt (p, r) -> (p, r)
    in
    let args = Stack.pop_n stack (List.length param_type) in
    let res = exec_func env module_indice func args in
    List.iter (Stack.push stack) (List.rev res)
  | Br i -> raise (Branch (indice_to_int i))
  | Br_if i ->
    let b = Stack.pop_bool stack in
    if b then raise (Branch (indice_to_int i))
  | Loop (_bt, e) -> exec_expr env module_indice locals stack e true
  | Block (_bt, e) -> exec_expr env module_indice locals stack e false
  | Memory_size -> failwith "TODO Memory_size"
  | Memory_grow -> failwith "TODO Memory_grow"
  | Memory_fill -> failwith "TODO Memory_fill"
  | Memory_copy -> failwith "TODO Memory_copy"
  | Memory_init _i -> failwith "TODO Memory_init"
  | Select _t ->
    let b = Stack.pop_bool stack in
    let o2 = Stack.pop stack in
    let o1 = Stack.pop stack in
    Stack.push stack
      ( if b then
        o1
      else
        o2 )
  | Local_tee _ -> failwith "TODO Local_tee"
  | Global_get _ -> failwith "TODO Global_get"
  | Global_set _ -> failwith "TODO Global_set"
  | Table_get _ -> failwith "TODO Table_get"
  | Table_set _ -> failwith "TODO Table_set"
  | Table_size _ -> failwith "TODO Table_size"
  | Table_grow _ -> failwith "TODO Table_grow"
  | Table_fill _ -> failwith "TODO Table_fill"
  | Table_copy _ -> failwith "TODO Table_copy"
  | Table_init _ -> failwith "TODO Table_init"
  | Elem_drop _ -> failwith "TODO Elem_drop"
  | I_load _ -> failwith "TODO I_load"
  | F_load (_, _) -> failwith "TODO F_load"
  | I_store (_, _) -> failwith "TODO I_store"
  | F_store (_, _) -> failwith "TODO F_store"
  | I_load8 (_, _, _) -> failwith "TODO I_load8"
  | I_load16 (_, _, _) -> failwith "TODO I_load16"
  | I64_load32 (_, _) -> failwith "TODO I64_load32"
  | I_store8 (_, _) -> failwith "TODO I_store8"
  | I_store16 (_, _) -> failwith "TODO I_store16"
  | I64_store32 _ -> failwith "TODO I64"
  | Data_drop _ -> failwith "TODO Data_drop"
  | Br_table (_, _) -> failwith "TODO Br_table"
  | Call_indirect (_, _) -> failwith "TODO Call_indirect"

and exec_expr env module_indice locals stack e is_loop =
  List.iter
    (fun instr ->
      try exec_instr env module_indice locals stack instr with
      | Branch -1 -> ()
      | Branch 0 when is_loop ->
        exec_expr env module_indice locals stack e true (* TODO: -1 ? *)
      | Branch n -> raise (Branch (n - 1)) )
    e

and exec_func env module_indice func args =
  Format.printf "calling func : module %d, func %s@." module_indice
    (Option.value func.id ~default:"anonymous");
  let locals = Array.of_list @@ args @ List.map init_local func.locals in
  let stack = Stack.create () in
  let result =
    try
      exec_expr env module_indice locals stack func.body false;
      stack
    with
    | Return
    | Branch -1 ->
      stack
  in
  Format.printf "stack        : [ %a ]@." Stack.pp result;
  Stack.to_list result

let invoke env module_indice f args =
  Format.printf "invoke       : %s@." f;
  let module_ = env.modules.(module_indice) in
  let func =
    match Hashtbl.find_opt module_.exported_funcs f with
    | None -> failwith "undefined export"
    | Some func -> func
  in
  let module_ = env.modules.(module_indice) in
  let func = module_.funcs.(func) in
  exec_func env module_indice func args

let exec_action env = function
  | Invoke_indice (i, f, args) ->
    let result = invoke env i f args in
    (env, result)
  | Get _ -> failwith "not yet implemented"

let compare_result_const result const =
  match result with
  | Result_const c -> const = c
  | Result_func_ref -> failwith "TODO (compare_result_const)"
  | Result_extern_ref -> failwith "TODO (compare_result_const)"

let exec_assert env = function
  | SAssert_return (action, results_expected) ->
    (* TODO: why do we have to rev here, is it correct ? *)
    let results_expected = List.rev results_expected in
    let env, results_got = exec_action env action in
    let eq =
      List.length results_expected = List.length results_got
      && List.for_all2
           (fun result const -> compare_result_const result const)
           results_expected results_got
    in
    if not eq then begin
      Format.eprintf "assert_return failed !@.expected: `%a`@.got: `%a`@."
        (Format.pp_print_list Pp.result)
        results_expected Pp.consts results_got;
      exit 1
    end;
    env
  | _ -> failwith "not yet implemented"

let exec_register env name i =
  Hashtbl.replace env.registered_modules name i;
  env

let exec_module env i =
  let curr_func = ref (-1) in
  let seen_funcs = env.modules.(i).seen_funcs in
  List.iter
    (function
      | MFunc f ->
        incr curr_func;
        let i = !curr_func in
        Option.iter (fun id -> Hashtbl.replace seen_funcs id i) f.id
      | _ -> () )
    env.modules.(i).fields;
  { env with last_module = Some i }

let exec_cmd env = function
  | Module_indice i -> exec_module env i
  | Assert a -> exec_assert env a
  | Register_indice (name, i) -> exec_register env name i
  | Action a -> fst (exec_action env a)

let exec script modules =
  let env =
    List.fold_left
      (fun env cmd -> exec_cmd env cmd)
      { empty_env with modules } script
  in
  ignore env
