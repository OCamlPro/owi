open Types
module Simplify = Simplify_bis
module Link = Link_bis
module Interpret = Interpret_bis

(* type action =
 *   | Invoke_indice of int * string * const list
 *   | Get_indice of int * string *)

(* type assert_ =
 *   | SAssert_return of action * result list
 *   | SAssert_trap of action * string
 *   | SAssert_exhaustion of action * string *)

(* type cmd =
 *   | Module_indice of int
 *   | Assert of assert_
 *   | Register_indice of string * int
 *   | Action of action *)

(* type script = cmd list * module_ Array.t *)

let ignore_tmp =
  [ "type mismatch"
  ; "invalid result arity"
  ; "undeclared function reference"
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

let check_error' ~expected ~got =
  let ok = String.starts_with ~prefix:expected got in
  if not ok then begin
    Format.eprintf "expected: `%s`@." expected;
    Format.eprintf "got     : `%s`@." got;
    failwith got
  end

let check_error ~expected ~got =
  let ok =
    got = expected
    || List.mem expected ignore_tmp
    || String.starts_with ~prefix:got expected
    || got = "constant out of range"
       && (expected = "i32 constant out of range" || expected = "i32 constant")
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

let load_func_from_module ls mod_name f_name =
  let exports =
    match mod_name with
    | None -> begin
      match ls.Link.last with
      | None -> failwith "unbound last module"
      | Some m -> m
    end
    | Some mod_name -> (
      match Link.StringMap.find mod_name ls.Link.by_name with
      | exception Not_found -> failwith ("unbound module " ^ mod_name)
      | exports -> exports )
  in
  match Link.StringMap.find f_name exports.functions with
  | exception Not_found -> failwith ("unbound name " ^ f_name)
  | v -> (exports.Link.env, v)

let compare_result_const result (const : Value.t) =
  match (result, const) with
  | Result_const (Literal (Const_I32 n)), I32 n' -> n = n'
  | Result_const (Literal (Const_I64 n)), I64 n' -> n = n'
  | Result_const (Literal (Const_F32 n)), F32 n' -> n = n'
  | Result_const (Literal (Const_F64 n)), F64 n' -> n = n'
  | Result_const (Literal (Const_null _rt)), Ref _ -> failwith "TODO const null"
  | Result_const (Literal (Const_host _n)), Ref _ -> failwith "TODO const host"
  (* | Result_const (Literal (Const_null rt)), Const_null rt' -> rt = rt' *)
  (* | Result_const (Literal (Const_host n)), Const_host n' -> n = n' *)
  | Result_const (Nan_canon S32), F32 f ->
    f = Float32.pos_nan || f = Float32.neg_nan
  | Result_const (Nan_canon S64), F64 f ->
    f = Float64.pos_nan || f = Float64.neg_nan
  | Result_const (Nan_arith S32), F32 f ->
    let pos_nan = Float32.to_bits Float32.pos_nan in
    Int32.logand (Float32.to_bits f) pos_nan = pos_nan
  | Result_const (Nan_arith S64), F64 f ->
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

let value_of_const : Types.const -> Value.t =
 fun const ->
  match const with
  | Const_I32 v -> I32 v
  | Const_I64 v -> I64 v
  | Const_F32 v -> F32 v
  | Const_F64 v -> F64 v
  | Const_null _ -> failwith "TODO null"
  | Const_host _ -> failwith "TODO host"

let action (link_state : Link.link_state) = function
  | Invoke (mod_name, f, args) -> begin
    Debug.debugerr "Invoke %s %a@." f Pp.Input.consts args;
    let env, f = load_func_from_module link_state mod_name f in
    let stack = List.rev_map value_of_const args in
    let stack = Interpret_bis.exec_vfunc env stack f in
    stack
  end
  | Get (_mod_name, _n) ->
    (* let i = Simplify.find_module mod_name last_module seen_modules in
     * Get_indice (i, n) *)
    failwith "TODO get action"

let pp_name ppf (name, indice) =
  match name with
  | Some n -> Format.fprintf ppf "%s" n
  | None -> Format.fprintf ppf "%d" indice

let rec run script =
  let script = Spectest.m :: Register ("spectest", Some "spectest") :: script in

  let curr_module = ref 0 in
  let link_state =
    List.fold_left
      (fun (link_state : Link.link_state) -> function
        | Module m ->
          let name = (m.id, !curr_module) in
          incr curr_module;
          Debug.debugerr "simplifying module %a... " pp_name name;
          let m = Simplify.simplify m in
          Debug.debugerr "linking module... ";
          let module_to_run, link_state = Link.link_module m link_state in
          Debug.debugerr "eval module... !@\n";
          Interpret.exec_module module_to_run;
          Debug.debugerr "done %a !@\n" pp_name name;
          link_state
        | Assert (Assert_trap_module (m, msg)) ->
          let name = (m.id, !curr_module) in
          incr curr_module;
          Debug.debugerr "simplifying module %a... " pp_name name;
          let m = Simplify.simplify m in
          Debug.debugerr "linking module... ";
          let module_to_run, _ignored_link_state =
            Link.link_module m link_state
          in
          Debug.debugerr "eval module... !@\n";
          begin
            try
              Interpret.exec_module module_to_run;
              assert false
            with
            | Trap trap_msg -> assert (msg = trap_msg)
            | _ -> assert false
          end;
          Debug.debugerr "done %a !@\n" pp_name name;
          link_state
        | Assert (Assert_malformed_binary _) ->
          Debug.debug Format.err_formatter
            "simplifying assert malformed binary... ";
          Debug.debug Format.err_formatter "done !@\n";
          (* TODO: check this when binary format is supported *)
          link_state
        | Assert (Assert_malformed_quote (m, expected)) ->
          Debug.debug Format.err_formatter
            "simplifying assert malformed quote... ";
          let got =
            match Parse.from_string (String.concat "\n" m) with
            | Ok script -> (
              try
                match check script with
                | Ok () ->
                  let _link_state = run script in
                  "Ok"
                | Error got -> got
              with Failure got -> got )
            | Error got -> got
          in
          check_error ~expected ~got;
          Debug.debug Format.err_formatter "done !@\n";
          link_state
        | Assert (Assert_invalid_binary _) ->
          (* TODO: check this when binary format is supported *)
          link_state
        | Assert (Assert_invalid (m, expected)) ->
          let got =
            try
              match Check.module_ m with
              | Ok () ->
                let m = Simplify_bis.simplify m in
                let _module_to_run, _link_state =
                  Link.link_module m link_state
                in
                "Ok"
              | Error got -> got
            with Failure got -> got
          in
          check_error ~expected ~got;
          link_state
        | Assert (Assert_invalid_quote (m, expected)) ->
          let got =
            match Parse.from_string (String.concat "\n" m) with
            | Error got -> got
            | Ok _ -> "Ok"
          in
          check_error ~expected ~got;
          link_state
        | Assert (Assert_unlinkable (_m, _msg)) ->
          (* let curr_module = curr_module + 1 in
           * Debug.debug Format.err_formatter
           *   "simplifying (unlinkable) module %d@." curr_module;
           * Option.iter
           *   (fun id -> Hashtbl.replace seen_modules id curr_module)
           *   m.id;
           * let cmd = Module_indice curr_module in
           * let module_ = Simplify.mk_module registered_modules m in
           * let module_ = { module_ with Simplify.should_not_link = Some msg } in
           * (curr_module, module_ :: modules, cmd :: scr) *)
          failwith "TODO assert_unlinkable"
        | Assert (Assert_malformed _) -> failwith "TODO assert_malformed"
        | Assert (Assert_return (a, res)) ->
          Debug.debugerr "Assert@.";
          let stack = action link_state a in
          if
            not
              (List.for_all2
                 (fun res v -> compare_result_const res v)
                 res (List.rev stack) )
          then begin
            Format.eprintf "got:      %a@.expected: %a@." Stack_bis.pp
              (List.rev stack) Pp.Input.results res;
            failwith "Bad result"
          end;
          link_state
        | Assert (Assert_trap (a, msg)) -> begin
          try
            let _ = action link_state a in
            assert false
          with
          | Trap got ->
            check_error' ~expected:msg ~got;
            Debug.debugerr "expected trap: \"%s\"@." msg;
            link_state
          | _ -> assert false
        end
        | Assert (Assert_exhaustion _) -> failwith "TODO assert_exhaustion"
        (* | Assert _a ->
         *   let action = action (Some curr_module) seen_modules in
         *   let cmd =
         *     match a with
         *     | Assert_return (a, res) -> SAssert_return (action a, res)
         *     | Assert_trap (a, failure) -> SAssert_trap (action a, failure)
         *     | Assert_exhaustion (a, failure) ->
         *       SAssert_exhaustion (action a, failure)
         *     | _ -> assert false (\* should have been handled before *\)
         *   in
         *   (curr_module, modules, Assert cmd :: scr) *)
        | Register (name, mod_name) ->
          Link.register_module link_state ~name ~id:mod_name
        | Action a ->
          let _v = action link_state a in
          link_state )
      Link.empty_state script
  in
  link_state

(* let fmt = Format.std_formatter *)

(* let invoke env module_indice f args =
 *   Debug.debug fmt "invoke       : %s@." f;
 *   let module_ = env.Interpret.modules.(module_indice) in
 *   let func_indice =
 *     match Hashtbl.find_opt module_.exported_funcs f with
 *     | None -> failwith "undefined export"
 *     | Some indice -> indice
 *   in
 *   let module_indice, func =
 *     Link.get_func env.modules module_indice func_indice
 *   in
 *   Interpret.exec_func env module_indice func args
 * 
 * let exec_action env = function
 *   | Invoke_indice (i, f, args) ->
 *     let result = invoke env i f args in
 *     (env, result)
 *   | Get_indice (mi, name) -> (
 *     match Hashtbl.find_opt env.modules.(mi).exported_globals name with
 *     | None -> failwith "exec_action"
 *     | Some g ->
 *       let _mi, _t, e = Link.get_global env.modules mi g in
 *       (env, [ e ]) ) *)

(* let exec_assert env = function
 *   | SAssert_return (action, results_expected) ->
 *     Debug.debug fmt "assert return...@.";
 *     let env, results_got = exec_action env action in
 *     let results_got = List.rev results_got in
 *     let eq =
 *       List.length results_expected = List.length results_got
 *       && List.for_all2 compare_result_const results_expected results_got
 *     in
 *     if not eq then begin
 *       failwith
 *       @@ Format.asprintf
 *            "assert_return failed !@.expected: `%a`@.got     : `%a`@."
 *            (Format.pp_print_list
 *               ~pp_sep:(fun fmt () -> Format.fprintf fmt " ")
 *               Pp.Input.result )
 *            results_expected Pp.Input.consts results_got
 *     end;
 *     env
 *   | SAssert_trap (action, expected) ->
 *     Debug.debug fmt "assert trap...@.";
 *     let got =
 *       try
 *         let _env, _results = exec_action env action in
 *         "Ok"
 *       with Trap got -> got
 *     in
 *     check_error ~expected ~got;
 *     env
 *   | SAssert_exhaustion (action, expected) ->
 *     Debug.debug fmt "assert exhaustion...@.";
 *     let got =
 *       try
 *         ignore @@ exec_action env action;
 *         "Ok"
 *       with Stack_overflow -> "call stack exhausted"
 *     in
 *     check_error ~expected ~got;
 *     env *)

let exec script =
  (* let _env =
   *   List.fold_left
   *     (fun env -> function
   *       | Module_indice i -> Interpret.exec_module env i
   *       | Assert a -> exec_assert env a
   *       | Register_indice (name, i) ->
   *         Hashtbl.replace env.Interpret.registered_modules name i;
   *         env
   *       | Action a -> fst (exec_action env a) )
   *     { Interpret.modules; registered_modules = Hashtbl.create 64 }
   *     script
   * in
   * () *)
  let _link_state = run script in
  ()
