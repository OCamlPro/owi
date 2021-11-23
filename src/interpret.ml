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

let stack_to_list stack = Stack.to_seq stack |> List.of_seq

let pp_stack fmt stack =
  let stack = stack_to_list stack in
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt " ; ")
    Pp.const fmt stack

exception Return of const Stack.t

let exec_iunop stack nn op =
  let result =
    match Stack.pop stack with
    | Const_I32 n ->
      if nn <> S32 then failwith "invalid type";
      let res =
        match op with
        | Clz -> Op.i32_clz n
        | Ctz -> Op.i32_ctz n
        | Popcnt -> Op.i32_popcnt n
      in
      Const_I32 (Int32.of_int res)
    | Const_I64 n ->
      if nn <> S64 then failwith "invalid type";
      let res =
        match op with
        | Clz -> Op.i64_clz n
        | Ctz -> Op.i64_ctz n
        | Popcnt -> Op.i64_popcnt n
      in
      Const_I64 (Int64.of_int res)
    | _ -> failwith "invalid type"
  in
  Stack.push result stack

let exec_funop stack nn op =
  let result =
    match Stack.pop stack with
    | Const_F32 f ->
      if nn <> S32 then failwith "invalid type";
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
      Const_F32 res
    | Const_F64 f ->
      if nn <> S64 then failwith "invalid type";
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
      Const_F64 res
    | _ -> failwith "invalid type"
  in
  Stack.push result stack

let exec_ibinop stack nn (op : Types.ibinop) =
  let result =
    match (Stack.pop stack, Stack.pop stack) with
    | Const_I32 n1, Const_I32 n2 ->
      if nn <> S32 then failwith "invalid type";
      let res =
        match op with
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
        | Rotr -> failwith "TODO Rotr"
      in
      Const_I32 res
    | Const_I64 n1, Const_I64 n2 ->
      if nn <> S64 then failwith "invalid type";
      let res =
        match op with
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
        | Rotr -> failwith "TODO Rotr"
      in
      Const_I64 res
    | _, _ -> failwith "invalid type"
  in
  Stack.push result stack

let exec_fbinop stack nn (op : Types.fbinop) =
  let result =
    match (Stack.pop stack, Stack.pop stack) with
    | Const_F32 f1, Const_F32 f2 ->
      if nn <> S32 then failwith "invalid types";
      let res =
        match op with
        | Add -> Float.add f1 f2
        | Sub -> Float.sub f1 f2
        | Mul -> Float.mul f1 f2
        | Div -> Float.div f1 f2
        | Min -> Float.min f1 f2
        | Max -> Float.max f1 f2
        | Copysign -> Float.copy_sign f1 f2
      in
      Const_F32 res
    | Const_F64 f1, Const_F64 f2 ->
      if nn <> S64 then failwith "invalid types";
      let res =
        match op with
        | Add -> Float.add f1 f2
        | Sub -> Float.sub f1 f2
        | Mul -> Float.mul f1 f2
        | Div -> Float.div f1 f2
        | Min -> Float.min f1 f2
        | Max -> Float.max f1 f2
        | Copysign -> Float.copy_sign f1 f2
      in
      Const_F64 res
    | _, _ -> failwith "invalid type"
  in
  Stack.push result stack

let exec_itestop _stack _nn _op = failwith "TODO"

let exec_irelop _stack _nn _op = failwith "TODO"

let exec_frelop _stack _nn _op = failwith "TODO"

let exec_instr _env _locals stack instr =
  Format.printf "stack        : [ %a ]@." pp_stack stack;
  Format.printf "running instr: %a@." Pp.instr instr;
  match instr with
  | Return -> raise (Return stack)
  | I32_const n -> Stack.push (Const_I32 n) stack
  | I64_const n -> Stack.push (Const_I64 n) stack
  | F32_const f -> Stack.push (Const_F32 f) stack
  | F64_const f -> Stack.push (Const_F64 f) stack
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
  | Ref_null _t -> failwith "TODO exec_instr"
  | Ref_is_null -> failwith "TODO exec_instr"
  | Ref_func _fid -> failwith "TODO exec_instr"
  | Drop -> ignore (Stack.pop stack)
  | _ -> failwith "TODO (exec_instr)"

let exec_func env module_indice func_indice args =
  Format.printf "calling func : module %d, func %d@." module_indice func_indice;
  let module_ = env.modules.(module_indice) in
  let func = module_.funcs.(func_indice) in
  (* TODO: let locals = args @ func.locals in*)
  let locals = args in
  let stack = Stack.create () in
  let result =
    try
      List.iter (fun instr -> exec_instr env locals stack instr) func.body;
      stack
    with
    | Return stack -> stack
  in
  Format.printf "stack        : [ %a ]@." pp_stack result;
  stack_to_list result

let invoke env module_indice f args =
  let module_ = env.modules.(module_indice) in
  let func =
    match Hashtbl.find_opt module_.exported_funcs f with
    | None -> failwith "undefined export"
    | Some func -> func
  in
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
    let env, results_got = exec_action env action in
    let eq =
      List.length results_expected = List.length results_got
      && List.fold_left2
           (fun eq result const -> eq && compare_result_const result const)
           true results_expected results_got
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

let exec_module env i = { env with last_module = Some i }

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
