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

let exec_instr _env _locals stack instr =
  Format.printf "stack        : [ %a ]@." pp_stack stack;
  Format.printf "running instr: %a@." Pp.instr instr;
  match instr with
  | I32_const n -> Stack.push (Const_I32 n) stack
  | _ -> failwith "TODO (exec_instr)"

let exec_func env module_indice func_indice args =
  let module_ = env.modules.(module_indice) in
  let func = module_.funcs.(func_indice) in
  (* TODO: let locals = args @ func.locals in*)
  let locals = args in
  let stack = Stack.create () in
  List.iter (fun instr -> exec_instr env locals stack instr) func.body;
  Format.printf "stack        : [ %a ]@." pp_stack stack;
  stack_to_list stack

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
