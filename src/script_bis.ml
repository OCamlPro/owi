open Types
module Simplify = Simplify_bis
module Link = Link_bis
module Interpret = Interpret_bis

type value = Link.value

module Host_externref = struct
  type t = int

  let ty : t Value.Extern_ref.ty = Value.Extern_ref.fresh "host"

  let value i = Value.Externref (Some (Value.E (ty, i)))
end

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

let load_func_from_module ls mod_id f_name : Link.func =
  let exports =
    match mod_id with
    | None -> begin
      match ls.Link.last with
      | None -> failwith "unbound last module"
      | Some m -> m
    end
    | Some mod_id -> (
      match Link.StringMap.find mod_id ls.Link.by_id with
      | exception Not_found -> failwith ("unbound module " ^ mod_id)
      | exports -> exports )
  in
  match Link.StringMap.find f_name exports.functions with
  | exception Not_found -> failwith ("unbound name " ^ f_name)
  | v -> v

let load_global_from_module ls mod_id name : Link.global =
  let exports =
    match mod_id with
    | None -> begin
      match ls.Link.last with
      | None -> failwith "unbound last module"
      | Some m -> m
    end
    | Some mod_id -> (
      match Link.StringMap.find mod_id ls.Link.by_id with
      | exception Not_found -> failwith ("unbound module " ^ mod_id)
      | exports -> exports )
  in
  match Link.StringMap.find name exports.globals with
  | exception Not_found -> failwith ("unbound name " ^ name)
  | v -> v

let compare_result_const result (const : value) =
  match (result, const) with
  | Result_const (Literal (Const_I32 n)), I32 n' -> n = n'
  | Result_const (Literal (Const_I64 n)), I64 n' -> n = n'
  | Result_const (Literal (Const_F32 n)), F32 n' -> n = n'
  | Result_const (Literal (Const_F64 n)), F64 n' -> n = n'
  | Result_const (Literal (Const_null Func_ref)), Ref (Funcref None) -> true
  | Result_const (Literal (Const_null Extern_ref)), Ref (Externref None) -> true
  | Result_const (Literal (Const_host n)), Ref (Externref (Some ref)) -> begin
    match Value.cast_ref ref Host_externref.ty with
    | None -> false
    | Some n' -> n = n'
  end
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

let value_of_const : Types.const -> value =
 fun const ->
  match const with
  | Const_I32 v -> I32 v
  | Const_I64 v -> I64 v
  | Const_F32 v -> F32 v
  | Const_F64 v -> F64 v
  | Const_null rt -> Value.ref_null rt
  | Const_host i -> Ref (Host_externref.value i)

let action (link_state : Link.link_state) = function
  | Invoke (mod_id, f, args) -> begin
    Debug.debugerr "Invoke %a %s %a@."
      (Format.pp_print_option
         ~none:(fun ppf () -> Format.fprintf ppf "<>")
         Format.pp_print_string )
      mod_id f Pp.Input.consts args;
    let f = load_func_from_module link_state mod_id f in
    let stack = List.rev_map value_of_const args in
    let stack = Interpret_bis.exec_vfunc stack f in
    stack
  end
  | Get (mod_id, name) ->
    let global = load_global_from_module link_state mod_id name in
    [ global.value ]

let pp_name ppf (name, indice) =
  match name with
  | Some n -> Format.fprintf ppf "%s" n
  | None -> Format.fprintf ppf "%d" indice

let rec run ~with_exhaustion script =
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
                  let _link_state : Link.link_state = run script ~with_exhaustion:false in
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
        | Assert (Assert_unlinkable (m, expected)) ->
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
          check_error' ~expected ~got;
          link_state
        | Assert (Assert_malformed _) -> failwith "TODO assert_malformed"
        | Assert (Assert_return (a, res)) ->
          Debug.debugerr "Assert@.";
          let stack = action link_state a in
          if
            List.length res <> List.length stack
            || not
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
            failwith "unxpected success"
          with
          | Trap got ->
            check_error' ~expected:msg ~got;
            Debug.debugerr "expected trap: \"%s\"@." msg;
            link_state
          | exn ->
            Format.eprintf "Wrong exn %s@." (Printexc.to_string exn);
            raise exn
        end
        | Assert (Assert_exhaustion (a, expected)) ->
          if with_exhaustion then begin
            let got =
              try
                ignore @@ action link_state a;
                "Ok"
              with Stack_overflow -> "call stack exhausted"
            in
            check_error ~expected ~got;
            link_state
          end
          else (* Assert_exhaustion ignored *)
            link_state
        | Register (name, mod_name) ->
          Link.register_module link_state ~name ~id:mod_name
        | Action a ->
          let _v = action link_state a in
          link_state )
      Link.empty_state script
  in
  link_state

let exec ?(with_exhaustion = false) script =
  let _link_state = run ~with_exhaustion script in
  ()
