(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax
module Stack = Stack.Make [@inlined hint] (Concrete_value)

module Host_externref = struct
  type t = int

  let ty : t Type.Id.t = Type.Id.make ()

  let value i = Concrete_ref.extern ty i
end

let check_error ~expected ~got : unit Result.t =
  let ok =
    String.equal (Result.err_to_string got) expected
    || String.starts_with ~prefix:expected (Result.err_to_string got)
    ||
    match got with
    | (`Msg s | `Parse_fail s)
      when String.starts_with ~prefix:"constant out of range" s ->
      String.starts_with ~prefix:"i32 constant" expected
    | `Constant_out_of_range ->
      String.starts_with ~prefix:"i32 constant" expected
    | `Parse_fail "unexpected end of section or function"
    | `Msg "unexpected end of section or function" ->
      String.equal expected "section size mismatch"
    | `Parse_fail "END opcode expected" ->
      String.equal expected "illegal opcode"
      || String.equal expected "unexpected end"
    | `Parse_fail "integer representation too long (read_SN 2)" ->
      String.equal expected "unexpected end of section or function"
    | `Parse_fail "integer representation too long (read_UN 2)" ->
      String.equal expected "unexpected end of section or function"
      || String.equal expected "length out of bounds"
      || String.equal expected "unexpected end"
    | `Parse_fail "integer too large (read_limits)" ->
      String.equal expected "integer representation too long"
    | `Parse_fail "function and code section have inconsistent lengths" ->
      String.equal expected "unexpected content after last section"
    | _ -> false
  in
  if not ok then begin
    Error (`Failed_with_but_expected (got, expected))
  end
  else Ok ()

let check_error_result expected = function
  | Ok _whatever -> Error (`Did_not_fail_but_expected expected)
  | Error got -> check_error ~expected ~got

let load_module ls mod_id =
  match mod_id with
  | None -> begin
    match Link.State.get_last ls with
    | None -> Error `Unbound_last_module
    | Some m -> Ok m
  end
  | Some mod_id -> (
    match Link.State.get_by_id ls mod_id with
    | None -> Error (`Unbound_module mod_id)
    | Some exports -> Ok exports )

let load_func_from_module ls mod_id f_name =
  let* exports, env_id = load_module ls mod_id in
  match Link.StringMap.find_opt f_name exports.functions with
  | None -> Error (`Unbound_name f_name)
  | Some v -> Ok (v, env_id)

let load_global_from_module ls mod_id name =
  let* exports, _env_id = load_module ls mod_id in
  match Link.StringMap.find_opt name exports.globals with
  | None -> Error (`Unbound_name name)
  | Some v -> Ok v

let compare_result_const result (const : Concrete_value.t) =
  match (result, const) with
  | Wast.Result_const (Literal (Const_I32 n)), I32 n' -> Int32.eq n n'
  | Result_const (Literal (Const_I64 n)), I64 n' -> Int64.eq n n'
  | Result_const (Literal (Const_F32 n)), F32 n' ->
    Float32.eq n n' || String.equal (Float32.to_string n) (Float32.to_string n')
  | Result_const (Literal (Const_F64 n)), F64 n' ->
    Float64.eq n n' || String.equal (Float64.to_string n) (Float64.to_string n')
  | Result_const (Literal (Const_V128 n)), V128 n' -> Concrete_v128.eq n n'
  | Result_const (Literal (Const_null Func_ht)), Ref (Func None) -> true
  | Result_const (Literal (Const_null Extern_ht)), Ref (Extern None) -> true
  | Result_const (Literal (Const_extern n)), Ref (Extern (Some ref)) -> begin
    match Concrete_ref.Extern.cast ref Host_externref.ty with
    | None -> false
    | Some n' -> n = n'
  end
  | Result_const (Nan_canon S32), F32 f ->
    Float32.is_pos_nan f || Float32.is_neg_nan f
  | Result_const (Nan_canon S64), F64 f ->
    Float64.is_pos_nan f || Float64.is_neg_nan f
  | Result_const (Nan_arith S32), F32 f ->
    let pos_nan = Float32.to_bits Float32.pos_nan in
    Int32.eq (Int32.logand (Float32.to_bits f) pos_nan) pos_nan
  | Result_const (Nan_arith S64), F64 f ->
    let pos_nan = Float64.to_bits Float64.pos_nan in
    Int64.eq (Int64.logand (Float64.to_bits f) pos_nan) pos_nan
  | Result_const (Nan_arith _), _
  | Result_const (Nan_canon _), _
  | Result_const (Literal (Const_I32 _)), _
  | Result_const (Literal (Const_I64 _)), _
  | Result_const (Literal (Const_F32 _)), _
  | Result_const (Literal (Const_F64 _)), _
  | Result_const (Literal (Const_null _)), _
  | Result_const (Literal (Const_host _)), _ ->
    false
  | _ ->
    Log.err (fun m -> m "TODO: unimplemented Script.compare_result_const");
    assert false

let value_of_const : Wast.const -> Concrete_value.t = function
  | Const_I32 v -> Concrete_value.I32 v
  | Const_I64 v -> Concrete_value.I64 v
  | Const_F32 v -> Concrete_value.F32 v
  | Const_F64 v -> Concrete_value.F64 v
  | Const_V128 v -> Concrete_value.V128 v
  | Const_null rt -> Concrete_value.Ref (Concrete_ref.null rt)
  | Const_extern i -> Concrete_value.Ref (Host_externref.value i)
  | i ->
    Log.err (fun m ->
      m "TODO: unimplemented Script.value_of_const %a)" Wast.pp_const i );
    assert false

let action (link_state : Concrete_extern_func.extern_func Link.State.t) =
  function
  | Wast.Invoke (mod_id, f, args) -> begin
    Log.info (fun m ->
      m "invoke %a %s %a..."
        (Fmt.option ~none:Fmt.nop Fmt.string)
        mod_id f Wast.pp_consts args );
    let* f, env_id = load_func_from_module link_state mod_id f in
    let stack = List.rev_map value_of_const args in
    let envs = Link.State.get_envs link_state in
    let module I = Interpret.Concrete (Interpret.Default_parameters) in
    I.exec_vfunc_from_outside ~locals:stack ~env:env_id ~envs f
  end
  | Get (mod_id, name) ->
    Log.info (fun m -> m "get...");
    let+ global = load_global_from_module link_state mod_id name in
    [ global.value ]

let unsafe = false

let run ~no_exhaustion script =
  let state =
    Link.State.empty ()
    |> Link.Extern.modul ~name:"spectest_extern" Spectest.extern_m
  in
  let script = Spectest.m :: Register ("spectest", Some "spectest") :: script in
  let registered = ref false in
  let curr_module = ref 0 in
  let module I = Interpret.Concrete (Interpret.Default_parameters) in
  list_fold_left
    (fun (link_state : Concrete_extern_func.extern_func Link.State.t) ->
      function
      | Wast.Text_module m ->
        if !curr_module = 0 then
          (* TODO: disable printing*)
          ();
        Log.info (fun m -> m "*** module");
        incr curr_module;
        let* m, link_state =
          Compile.Text.until_link link_state ~unsafe ~name:None m
        in
        let+ () = I.modul link_state m in
        (* TODO: enable printing again! *)
        link_state
      | Wast.Quoted_module m ->
        Log.info (fun m -> m "*** quoted module");
        incr curr_module;
        let* m = Parse.Text.Inline_module.from_string m in
        let* m, link_state =
          Compile.Text.until_link link_state ~unsafe ~name:None m
        in
        let+ () = I.modul link_state m in
        link_state
      | Wast.Binary_module (id, m) ->
        Log.info (fun m -> m "*** binary module");
        incr curr_module;
        let* m = Parse.Binary.Module.from_string m in
        let m = { m with id } in
        let* m, link_state =
          Compile.Binary.until_link link_state ~unsafe ~name:None m
        in
        let+ () = I.modul link_state m in
        link_state
      | Assert (Assert_trap_module (m, expected)) ->
        Log.info (fun m -> m "*** assert_trap");
        incr curr_module;
        let* m, link_state =
          Compile.Text.until_link link_state ~unsafe ~name:None m
        in
        let got = I.modul link_state m in
        let+ () = check_error_result expected got in
        link_state
      | Assert (Assert_malformed_binary (m, expected)) ->
        Log.info (fun m -> m "*** assert_malformed_binary");
        let got = Parse.Binary.Module.from_string m in
        let+ () = check_error_result expected got in
        link_state
      | Assert (Assert_malformed_quote (m, expected)) ->
        Log.info (fun m -> m "*** assert_malformed_quote");
        (* TODO: use Parse.Text.Module.from_string instead *)
        let got = Parse.Text.Script.from_string m in
        let+ () =
          match got with
          | Error got -> check_error ~expected ~got
          | Ok [ Text_module m ] ->
            let got = Compile.Text.until_binary ~unsafe m in
            check_error_result expected got
          | _ -> assert false
        in
        link_state
      | Assert (Assert_invalid_binary (m, expected)) ->
        Log.info (fun m -> m "*** assert_invalid_binary");
        let got = Parse.Binary.Module.from_string m in
        let+ () =
          match got with
          | Error got -> check_error ~expected ~got
          | Ok m -> begin
            match Binary_validate.modul m with
            | Error got -> check_error ~expected ~got
            | Ok () ->
              let got = Link.Binary.modul link_state ~name:None m in
              check_error_result expected got
          end
        in
        link_state
      | Assert (Assert_invalid (m, expected)) ->
        Log.info (fun m -> m "*** assert_invalid");
        let got = Compile.Text.until_link link_state ~unsafe ~name:None m in
        let+ () = check_error_result expected got in
        link_state
      | Assert (Assert_invalid_quote (m, expected)) ->
        Log.info (fun m -> m "*** assert_invalid_quote");
        let got = Parse.Text.Module.from_string m in
        let+ () = check_error_result expected got in
        link_state
      | Assert (Assert_unlinkable (m, expected)) ->
        Log.info (fun m -> m "*** assert_unlinkable");
        let got = Compile.Text.until_link link_state ~unsafe ~name:None m in
        let+ () = check_error_result expected got in
        link_state
      | Assert (Assert_malformed (m, expected)) ->
        Log.info (fun m -> m "*** assert_malformed");
        let got = Compile.Text.until_link ~unsafe ~name:None link_state m in
        let+ () = check_error_result expected got in
        assert false
      | Assert (Assert_return (a, res)) ->
        Log.info (fun m -> m "*** assert_return");
        let* stack = action link_state a in
        let stack = List.rev stack in
        if
          List.compare_lengths res stack <> 0
          || not (List.for_all2 compare_result_const res stack)
        then begin
          Log.err (fun m ->
            m "got:      %a@.expected: %a" Stack.pp stack Wast.pp_results res );
          Error `Bad_result
        end
        else Ok link_state
      | Assert (Assert_trap (a, expected)) ->
        Log.info (fun m -> m "*** assert_trap");
        let got = action link_state a in
        let+ () = check_error_result expected got in
        link_state
      | Assert (Assert_exhaustion (a, expected)) ->
        Log.info (fun m -> m "*** assert_exhaustion");
        let+ () =
          if no_exhaustion then Ok ()
          else
            let got = action link_state a in
            check_error_result expected got
        in
        link_state
      | Register (name, mod_name) ->
        if !curr_module = 1 && not !registered then (* TODO: disable debug *) ();
        Log.info (fun m -> m "*** register");
        let+ state = Link.register_last_module link_state ~name ~id:mod_name in
        (* TODO: enable debug again! *)
        state
      | Action a ->
        Log.info (fun m -> m "*** action");
        let+ _stack = action link_state a in
        link_state )
    state script

let exec ~no_exhaustion script =
  let+ _link_state = run ~no_exhaustion script in
  ()
