open Types

type action =
  | Invoke_indice of int * string * const list
  | Get_indice of int * string

type assert_ =
  | SAssert_return of action * result list
  | SAssert_trap of action * string
  | SAssert_exhaustion of action * string
  | SAssert_invalid of Types.module_ * string
  | SAssert_invalid_quote of string list * string

type cmd =
  | Module_indice of int
  | Assert of assert_
  | Register_indice of string * int
  | Action of action

type script = cmd list * module_ Array.t

let ignore_tmp =
  [ "type mismatch"
  ; "unknown local"
  ; "unknown global"
  ; "unknown function"
  ; "unknown label"
  ; "unknown elem segment 0"
  ; "unknown elem segment 4"
  ; "unknown table 0"
  ; "unknown table"
  ; "invalid result arity"
  ; "unknown data segment"
  ; "unknown function 7"
  ; "unknown memory 0"
  ; "undeclared function reference"
  ; "unknown data segment 1"
  ; "multiple memories"
  ; "unknown memory"
  ; "global is immutable"
  ; "duplicate export name"
  ; "unknown global 0"
  ; "unknown global 1"
  ; "alignment must not be larger than natural"
    (*
            | I_load8 (_nn, _sx, { align; _ }) as i ->
              if align >= 2 then
                failwith "alignment must not be larger than natural";

              i
            | I_load16 (_nn, _sx, { align; _ }) as i ->
              if align >= 4 then
                failwith "alignment must not be larger than natural";
              i
            | I64_load32 (_sx, { align; _ }) as i ->
              if align >= 8 then
                failwith "alignment must not be larger than natural";
              i
            | (I_load (nn, { align; _ }) | F_load (nn, { align; _ })) as i ->
              let max_allowed = match nn with S32 -> 8 | S64 -> 16 in
              if align >= max_allowed then
                failwith "alignment must not be larger than natural";
              i
             *)
  ; "duplicate func"
  ; "duplicate local"
  ]

let check_error ~expected ~got =
  let ok =
    got = "constant out of range"
    && (expected = "i32 constant out of range" || expected = "i32 constant")
    || got = expected
    || List.mem expected ignore_tmp
  in
  if not ok then begin
    Format.eprintf "expected: `%s`@." expected;
    Format.eprintf "got     : `%s`@." got;
    failwith got
  end

let check script =
  try
    List.iter
      (function
        | Module m -> begin
          match Check.module_ m with Ok () -> () | Error e -> failwith e
        end
        | _ -> () )
      script;
    Ok ()
  with Failure e -> Error e

let action last_module seen_modules = function
  | Invoke (mod_name, f, args) ->
    let i = Simplify.find_module mod_name last_module seen_modules in
    Invoke_indice (i, f, args)
  | Get (mod_name, n) ->
    let i = Simplify.find_module mod_name last_module seen_modules in
    Get_indice (i, n)

let assert_ curr_module last_module seen_modules =
  let action = action last_module seen_modules in
  function
  | Assert_return (a, res) -> (curr_module, SAssert_return (action a, res))
  | Assert_trap (a, failure) -> (curr_module, SAssert_trap (action a, failure))
  | Assert_trap_module _ ->
    (* This should have been handled before and turned into a module with `should_trap` set ! *)
    assert false
  | Assert_exhaustion (a, failure) ->
    (curr_module, SAssert_exhaustion (action a, failure))
  | Assert_malformed _ ->
    (* This should have been checked before and removed ! *)
    assert false
  | Assert_malformed_quote _ ->
    (* This should have been checked before and removed ! *)
    assert false
  | Assert_malformed_binary _ ->
    (* This should have been checked before and removed ! *)
    assert false
  | Assert_invalid (module_, failure) ->
    (curr_module, SAssert_invalid (module_, failure))
  | Assert_invalid_quote (m, failure) ->
    (curr_module, SAssert_invalid_quote (m, failure))
  | Assert_invalid_binary (_m, _failure) ->
    (* This should have been checked before and removed ! *)
    assert false
  | Assert_unlinkable (_m, _s) ->
    (* This should have been handled before and turned into a module with `should_not_link` set ! *)
    assert false

let rec simplify script =
  let script = Spectest.m :: Register ("spectest", Some "spectest") :: script in

  let _curr_module, modules, scr =
    let seen_modules = Hashtbl.create 512 in
    let registered_modules = Hashtbl.create 512 in
    List.fold_left
      (fun (curr_module, modules, scr) -> function
        | Module m ->
          let curr_module = curr_module + 1 in
          Debug.debug Format.err_formatter "simplifying module %d... "
            curr_module;
          Option.iter
            (fun id -> Hashtbl.replace seen_modules id curr_module)
            m.id;
          let cmd = Module_indice curr_module in
          let modules = Simplify.mk_module registered_modules m :: modules in
          Debug.debug Format.err_formatter "done !@\n";
          (curr_module, modules, cmd :: scr)
        | Assert (Assert_trap_module (m, msg)) ->
          let curr_module = curr_module + 1 in
          Debug.debug Format.err_formatter "simplifying assert module %d@."
            curr_module;
          Option.iter
            (fun id -> Hashtbl.replace seen_modules id curr_module)
            m.id;
          let cmd = Module_indice curr_module in
          let module_ = Simplify.mk_module registered_modules m in
          let module_ = { module_ with should_trap = Some msg } in
          (curr_module, module_ :: modules, cmd :: scr)
        | Assert (Assert_malformed_binary _) ->
          Debug.debug Format.err_formatter
            "simplifying assert malformed binary... ";
          Debug.debug Format.err_formatter "done !@\n";
          (* TODO: check this when binary format is supported *)
          (curr_module, modules, scr)
        | Assert (Assert_malformed_quote (m, expected)) ->
          Debug.debug Format.err_formatter
            "simplifying assert malformed quote... ";

          ( match Parse.from_string (String.concat "\n" m) with
          | Ok script -> (
            try
              match check script with
              | Ok () ->
                let _script, _modules = simplify script in
                check_error ~expected ~got:"Ok"
              | Error got -> check_error ~expected ~got
            with Failure got -> check_error ~expected ~got )
          | Error got -> check_error ~expected ~got );
          Debug.debug Format.err_formatter "done !@\n";
          (curr_module, modules, scr)
        | Assert (Assert_invalid_binary _) ->
          (* TODO: check this when binary format is supported *)
          (curr_module, modules, scr)
        | Assert (Assert_invalid (m, expected)) ->
          let got =
            try
              match Check.module_ m with
              | Ok () -> (
                let m = Simplify.mk_module registered_modules m in
                try
                  Link.module_ registered_modules
                    (Array.of_list @@ List.rev @@ (m :: modules))
                    (curr_module + 1);
                  "Ok"
                with Failure got -> got )
              | Error got -> got
            with Failure got -> got
          in
          check_error ~expected ~got;
          (curr_module, modules, scr)
        | Assert (Assert_invalid_quote (m, expected)) ->
          ( match Parse.from_string (String.concat "\n" m) with
          | Error got -> check_error ~expected ~got
          | Ok [ Module _m ] -> check_error ~expected ~got:"Ok"
          | Ok _ -> assert false );
          (curr_module, modules, scr)
        | Assert (Assert_unlinkable (m, msg)) ->
          let curr_module = curr_module + 1 in
          Debug.debug Format.err_formatter
            "simplifying (unlinkable) module %d@." curr_module;
          Option.iter
            (fun id -> Hashtbl.replace seen_modules id curr_module)
            m.id;
          let cmd = Module_indice curr_module in
          let module_ = Simplify.mk_module registered_modules m in
          let module_ = { module_ with Simplify.should_not_link = Some msg } in
          (curr_module, module_ :: modules, cmd :: scr)
        | Assert a ->
          let curr_module, cmd =
            assert_ curr_module (Some curr_module) seen_modules a
          in
          let cmd = Assert cmd in
          (curr_module, modules, cmd :: scr)
        | Register (name, mod_name) ->
          let indice =
            Simplify.find_module mod_name (Some curr_module) seen_modules
          in
          Hashtbl.replace registered_modules name indice;
          let cmd = Register_indice (name, indice) in
          (curr_module, modules, cmd :: scr)
        | Action a ->
          let cmd = Action (action (Some curr_module) seen_modules a) in
          (curr_module, modules, cmd :: scr) )
      (-1, [], []) script
  in

  let script = List.rev scr in
  let modules = List.rev modules |> Array.of_list in

  Debug.debug Format.err_formatter "END OF SIMPLIFY@\n";

  (script, modules)

let fmt = Format.std_formatter

let invoke env module_indice f args =
  Debug.debug fmt "invoke       : %s@." f;
  let module_ = env.Interpret.modules.(module_indice) in
  let func_indice =
    match Hashtbl.find_opt module_.exported_funcs f with
    | None -> failwith "undefined export"
    | Some indice -> indice
  in
  let module_indice, func =
    Link.get_func env.modules module_indice func_indice
  in
  Interpret.exec_func env module_indice func args

let exec_action env = function
  | Invoke_indice (i, f, args) ->
    let result = invoke env i f args in
    (env, result)
  | Get_indice (mi, name) -> (
    match Hashtbl.find_opt env.modules.(mi).exported_globals name with
    | None -> failwith "exec_action"
    | Some g ->
      let _mi, _t, e = Link.get_global env.modules mi g in
      (env, [ e ]) )

let compare_result_const result const =
  match (result, const) with
  | Result_const (Literal (Const_I32 n)), Const_I32 n' -> n = n'
  | Result_const (Literal (Const_I64 n)), Const_I64 n' -> n = n'
  | Result_const (Literal (Const_F32 n)), Const_F32 n' -> n = n'
  | Result_const (Literal (Const_F64 n)), Const_F64 n' -> n = n'
  | Result_const (Literal (Const_null rt)), Const_null rt' -> rt = rt'
  | Result_const (Literal (Const_host n)), Const_host n' -> n = n'
  | Result_const (Nan_canon S32), Const_F32 f ->
    f = Float32.pos_nan || f = Float32.neg_nan
  | Result_const (Nan_canon S64), Const_F64 f ->
    f = Float64.pos_nan || f = Float64.neg_nan
  | Result_const (Nan_arith S32), Const_F32 f ->
    let pos_nan = Float32.to_bits Float32.pos_nan in
    Int32.logand (Float32.to_bits f) pos_nan = pos_nan
  | Result_const (Nan_arith S64), Const_F64 f ->
    let pos_nan = Float64.to_bits Float64.pos_nan in
    Int64.logand (Float64.to_bits f) pos_nan = pos_nan
  | Result_const (Nan_arith _), _
  | Result_const (Nan_canon _), _
  | Result_const (Literal (Const_I32 _)), _
  | Result_const (Literal (Const_I64 _)), _
  | Result_const (Literal (Const_F32 _)), _
  | Result_const (Literal (Const_F64 _)), _
  | Result_const (Literal (Const_null _)), _
  | Result_const (Literal (Const_host _)), _ ->
    false
  | Result_func_ref, _ -> failwith "TODO (compare_result_const)"
  | Result_extern_ref, _ -> failwith "TODO (compare_result_const)"

let exec_assert env = function
  | SAssert_return (action, results_expected) ->
    Debug.debug fmt "assert return...@.";
    let env, results_got = exec_action env action in
    let results_got = List.rev results_got in
    let eq =
      List.length results_expected = List.length results_got
      && List.for_all2
           (fun result const -> compare_result_const result const)
           results_expected results_got
    in
    if not eq then begin
      failwith
      @@ Format.asprintf
           "assert_return failed !@.expected: `%a`@.got     : `%a`@."
           (Format.pp_print_list
              ~pp_sep:(fun fmt () -> Format.fprintf fmt " ")
              Pp.result )
           results_expected Pp.consts results_got
    end;
    env
  | SAssert_trap (action, expected) ->
    begin
      try
        let _env, _results = exec_action env action in
        failwith
        @@ Format.sprintf "assert_trap failed ; expected `%s` but did not trap"
             expected
      with Trap msg ->
        let res = msg = expected in
        if not res then
          Debug.debug Format.err_formatter "expected `%s` but got `%s`@."
            expected msg;
        assert (msg = expected)
    end;
    env
  | SAssert_exhaustion (action, expected) -> (
    try
      ignore @@ exec_action env action;
      Format.eprintf "@.@.@. EXPECTED : `%S` @.@.@. " expected;
      assert false
    with Stack_overflow ->
      assert (expected = "call stack exhausted");
      env )
  | SAssert_invalid (_mod, _failure) -> (* TODO *) env
  | SAssert_invalid_quote (_mod, _failure) -> (* TODO *) env

let exec script modules =
  let env =
    List.fold_left
      (fun env -> function
        | Module_indice i -> Interpret.exec_module env i
        | Assert a -> exec_assert env a
        | Register_indice (name, i) ->
          Hashtbl.replace env.Interpret.registered_modules name i;
          env
        | Action a -> fst (exec_action env a)
      )
      { Interpret.modules; registered_modules = Hashtbl.create 64 }
      script
  in
  ignore env
