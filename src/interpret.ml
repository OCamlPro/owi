open Types

type env =
  { last_module : module_ option
  ; seen_modules : (string, module_) Hashtbl.t
  ; registered_modules : (string, module_) Hashtbl.t
  }

let empty_env =
  { last_module = None
  ; seen_modules = Hashtbl.create 64
  ; registered_modules = Hashtbl.create 64
  }

let _invoke _env _f _args = ()

let exec_action env = function
  | Invoke (mod_name, _f, _args) ->
    let module_ =
      match mod_name with
      | None -> begin
        match env.last_module with
        | None -> failwith "no module defined"
        | Some last_module -> last_module
      end
      | Some mod_name -> begin
        match Hashtbl.find_opt env.seen_modules mod_name with
        | None -> failwith (Format.sprintf "unknown module $%s" mod_name)
        | Some module_ -> module_
      end
    in
    ignore module_;
    (env, [])
  | Get _ -> failwith "not yet implemented"

let exec_assert env = function
  | Assert_return (action, results_expected) ->
    let env, results_got = exec_action env action in
    if results_expected <> results_expected then begin
      Format.eprintf "assert_return failed !@.expected: `%a`@.got: `%a`@."
        (Format.pp_print_list Pp.result) results_expected
        (Format.pp_print_list Pp.result) results_got
    end;
    env
  | _ -> failwith "not yet implemented"

let exec_register env name mod_name =
  let to_register =
    match mod_name with
    | None -> begin
      match env.last_module with
      | None -> failwith "no module defined"
      | Some last_module -> last_module
    end
    | Some mod_name -> begin
      match Hashtbl.find_opt env.seen_modules mod_name with
      | None -> failwith (Format.sprintf "unknown module $%s" mod_name)
      | Some module_ -> module_
    end
  in
  (* TODO: add/replace ? *)
  Hashtbl.replace env.registered_modules name to_register;
  env

let exec_module env m =
  Option.iter
    (fun id -> Hashtbl.replace env.seen_modules id m (* TODO: add/replace ? *))
    m.id;
  { env with last_module = Some m }

let exec_cmd env = function
  | Module m -> exec_module env m
  | Assert a -> exec_assert env a
  | Register (name, mod_name) -> exec_register env name mod_name
  | Action a -> fst (exec_action env a)

let exec ast =
  let env = List.fold_left (fun env cmd -> exec_cmd env cmd) empty_env ast in
  ignore env
