(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Types
open Syntax
module Stack = Stack.Make [@inlined hint] (V)

module Host_externref = struct
  type t = int

  let ty : t Type.Id.t = Type.Id.make ()

  let value i = Concrete_value.Externref (Some (Concrete_value.E (ty, i)))
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
    | `Msg "unexpected end of section or function" ->
      String.equal expected "section size mismatch"
    | _ -> false
  in
  if not ok then begin
    Error (`Failed_with_but_expected (got, expected))
  end
  else Ok ()

let check_error_result expected = function
  | Ok _whatever -> Error (`Did_not_fail_but_expected expected)
  | Error got -> check_error ~expected ~got

let load_func_from_module ls mod_id f_name =
  let* exports, env_id =
    match mod_id with
    | None -> begin
      match ls.Link.last with
      | None -> Error `Unbound_last_module
      | Some m -> Ok m
    end
    | Some mod_id -> (
      match Link.StringMap.find_opt mod_id ls.Link.by_id with
      | None -> Error (`Unbound_module mod_id)
      | Some exports -> Ok exports )
  in
  match Link.StringMap.find_opt f_name exports.functions with
  | None -> Error (`Unbound_name f_name)
  | Some v -> Ok (v, env_id)

let load_global_from_module ls mod_id name =
  let* exports =
    match mod_id with
    | None -> begin
      match ls.Link.last with
      | None -> Error `Unbound_last_module
      | Some (m, _env_id) -> Ok m
    end
    | Some mod_id -> (
      match Link.StringMap.find_opt mod_id ls.Link.by_id with
      | None -> Error (`Unbound_module mod_id)
      | Some (exports, _env_id) -> Ok exports )
  in
  match Link.StringMap.find_opt name exports.globals with
  | None -> Error (`Unbound_name name)
  | Some v -> Ok v

let compare_result_const result (const : Concrete_value.t) =
  match (result, const) with
  | Text.Result_const (Literal (Const_I32 n)), I32 n' -> Int32.eq n n'
  | Result_const (Literal (Const_I64 n)), I64 n' -> Int64.eq n n'
  | Result_const (Literal (Const_F32 n)), F32 n' ->
    Float32.eq n n' || String.equal (Float32.to_string n) (Float32.to_string n')
  | Result_const (Literal (Const_F64 n)), F64 n' ->
    Float64.eq n n' || String.equal (Float64.to_string n) (Float64.to_string n')
  | Result_const (Literal (Const_null Func_ht)), Ref (Funcref None) -> true
  | Result_const (Literal (Const_null Extern_ht)), Ref (Externref None) -> true
  | Result_const (Literal (Const_extern n)), Ref (Externref (Some ref)) -> begin
    match Concrete_value.cast_ref ref Host_externref.ty with
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
    Log.debug0 "TODO (Script.compare_result_const)@\n";
    assert false

let value_of_const : text const -> V.t Result.t = function
  | Const_I32 v -> ok @@ Concrete_value.I32 v
  | Const_I64 v -> ok @@ Concrete_value.I64 v
  | Const_F32 v -> ok @@ Concrete_value.F32 v
  | Const_F64 v -> ok @@ Concrete_value.F64 v
  | Const_null rt ->
    let+ rt = Binary_types.convert_heap_type None rt in
    Concrete_value.ref_null rt
  | Const_extern i -> ok @@ Concrete_value.Ref (Host_externref.value i)
  | i ->
    Log.debug2 "TODO (Script.value_of_const) %a@\n" Types.pp_const i;
    assert false

let action (link_state : Concrete_value.Func.extern_func Link.state) = function
  | Text.Invoke (mod_id, f, args) -> begin
    Log.debug5 "invoke %a %s %a...@\n"
      (Fmt.option ~none:Fmt.nop Fmt.string)
      mod_id f Types.pp_consts args;
    let* f, env_id = load_func_from_module link_state mod_id f in
    let* stack = list_map value_of_const args in
    let stack = List.rev stack in
    Interpret.Concrete.exec_vfunc_from_outside ~locals:stack ~env:env_id
      ~envs:link_state.envs f
  end
  | Get (mod_id, name) ->
    Log.debug0 "get...@\n";
    let+ global = load_global_from_module link_state mod_id name in
    [ global.value ]

let unsafe = false

let run ~no_exhaustion ~optimize script =
  let state =
    Link.extern_module Link.empty_state ~name:"spectest_extern"
      Spectest.extern_m
  in
  let script = Spectest.m :: Register ("spectest", Some "spectest") :: script in
  let debug_on = !Log.debug_on in
  let registered = ref false in
  let curr_module = ref 0 in
  list_fold_left
    (fun (link_state : Concrete_value.Func.extern_func Link.state) -> function
      | Text.Text_module m ->
        if !curr_module = 0 then Log.debug_on := false;
        Log.debug0 "*** module@\n";
        incr curr_module;
        let+ link_state =
          Compile.Text.until_interpret link_state ~unsafe ~rac:false ~srac:false
            ~optimize ~name:None m
        in
        Log.debug_on := debug_on;
        link_state
      | Text.Quoted_module m ->
        Log.debug0 "*** quoted module@\n";
        incr curr_module;
        let* m = Parse.Text.Inline_module.from_string m in
        let+ link_state =
          Compile.Text.until_interpret link_state ~unsafe ~rac:false ~srac:false
            ~optimize ~name:None m
        in
        link_state
      | Text.Binary_module (id, m) ->
        Log.debug0 "*** binary module@\n";
        incr curr_module;
        let* m = Parse.Binary.Module.from_string m in
        let m = { m with id } in
        let+ link_state =
          Compile.Binary.until_interpret link_state ~unsafe ~optimize ~name:None
            m
        in
        link_state
      | Assert (Assert_trap_module (m, expected)) ->
        Log.debug0 "*** assert_trap@\n";
        incr curr_module;
        let* m, link_state =
          Compile.Text.until_link link_state ~unsafe ~rac:false ~srac:false
            ~optimize ~name:None m
        in
        let got = Interpret.Concrete.modul link_state.envs m in
        let+ () = check_error_result expected got in
        link_state
      | Assert (Assert_malformed_binary (m, expected)) ->
        Log.debug0 "*** assert_malformed_binary@\n";
        let got = Parse.Binary.Module.from_string m in
        let+ () = check_error_result expected got in
        link_state
      | Assert (Assert_malformed_quote (m, expected)) ->
        Log.debug0 "*** assert_malformed_quote@\n";
        (* TODO: use Parse.Text.Module.from_string instead *)
        let got = Parse.Text.Script.from_string m in
        let+ () =
          match got with
          | Error got -> check_error ~expected ~got
          | Ok [ Text_module m ] ->
            let got =
              Compile.Text.until_binary ~unsafe ~rac:false ~srac:false m
            in
            check_error_result expected got
          | _ -> assert false
        in
        link_state
      | Assert (Assert_invalid_binary (m, expected)) ->
        Log.debug0 "*** assert_invalid_binary@\n";
        let got = Parse.Binary.Module.from_string m in
        let+ () =
          match got with
          | Error got -> check_error ~expected ~got
          | Ok m -> begin
            match Binary_validate.modul m with
            | Error got -> check_error ~expected ~got
            | Ok () ->
              let got = Link.modul link_state ~name:None m in
              check_error_result expected got
          end
        in
        link_state
      | Assert (Assert_invalid (m, expected)) ->
        Log.debug0 "*** assert_invalid@\n";
        let got =
          Compile.Text.until_link link_state ~unsafe ~rac:false ~srac:false
            ~optimize ~name:None m
        in
        let+ () = check_error_result expected got in
        link_state
      | Assert (Assert_invalid_quote (m, expected)) ->
        Log.debug0 "*** assert_invalid_quote@\n";
        let got = Parse.Text.Module.from_string m in
        let+ () = check_error_result expected got in
        link_state
      | Assert (Assert_unlinkable (m, expected)) ->
        Log.debug0 "*** assert_unlinkable@\n";
        let got =
          Compile.Text.until_link link_state ~unsafe ~rac:false ~srac:false
            ~optimize ~name:None m
        in
        let+ () = check_error_result expected got in
        link_state
      | Assert (Assert_malformed (m, expected)) ->
        Log.debug0 "*** assert_malformed@\n";
        let got =
          Compile.Text.until_link ~unsafe ~optimize ~rac:false ~srac:false
            ~name:None link_state m
        in
        let+ () = check_error_result expected got in
        assert false
      | Assert (Assert_return (a, res)) ->
        Log.debug0 "*** assert_return@\n";
        let* stack = action link_state a in
        let stack = List.rev stack in
        if
          List.compare_lengths res stack <> 0
          || not (List.for_all2 compare_result_const res stack)
        then begin
          Fmt.epr "got:      %a@.expected: %a@." Stack.pp stack Text.pp_results
            res;
          Error `Bad_result
        end
        else Ok link_state
      | Assert (Assert_trap (a, expected)) ->
        Log.debug0 "*** assert_trap@\n";
        let got = action link_state a in
        let+ () = check_error_result expected got in
        link_state
      | Assert (Assert_exhaustion (a, expected)) ->
        Log.debug0 "*** assert_exhaustion@\n";
        let+ () =
          if no_exhaustion then Ok ()
          else
            let got = action link_state a in
            check_error_result expected got
        in
        link_state
      | Register (name, mod_name) ->
        if !curr_module = 1 && not !registered then Log.debug_on := false;
        Log.debug0 "*** register@\n";
        let+ state = Link.register_module link_state ~name ~id:mod_name in
        Log.debug_on := debug_on;
        state
      | Action a ->
        Log.debug0 "*** action@\n";
        let+ _stack = action link_state a in
        link_state )
    state script

let exec ~no_exhaustion ~optimize script =
  let+ _link_state = run ~no_exhaustion ~optimize script in
  ()
