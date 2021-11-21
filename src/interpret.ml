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

let _invoke _env _f _args = ()

let exec_action env = function
  | Invoke_indice (i, _f, _args) ->
    let module_ = env.modules.(i) in
    ignore module_;
    (env, [])
  | Get _ -> failwith "not yet implemented"

let exec_assert env = function
  | SAssert_return (action, results_expected) ->
    let env, results_got = exec_action env action in
    if results_expected <> results_expected then
      Format.eprintf "assert_return failed !@.expected: `%a`@.got: `%a`@."
        (Format.pp_print_list Pp.result)
        results_expected
        (Format.pp_print_list Pp.result)
        results_got;
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
