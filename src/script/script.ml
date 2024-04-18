(*****************************************************************************)
(*                                                                           *)
(*  Owi                                                                      *)
(*                                                                           *)
(*  Copyright (C) 2021-2024 OCamlPro                                         *)
(*                                                                           *)
(*  SPDX-License-Identifier: AGPL-3.0-or-later                               *)
(*                                                                           *)
(*  This program is free software: you can redistribute it and/or modify     *)
(*  it under the terms of the GNU Affero General Public License as published *)
(*  by the Free Software Foundation, either version 3 of the License, or     *)
(*  (at your option) any later version.                                      *)
(*                                                                           *)
(*  This program is distributed in the hope that it will be useful,          *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(*  GNU Affero General Public License for more details.                      *)
(*                                                                           *)
(*  You should have received a copy of the GNU Affero General Public License *)
(*  along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
(*                                                                           *)
(*****************************************************************************)

open Types
open Syntax
module Stack = Stack.Make (V) [@@inlined hint]

module Host_externref = struct
  type t = int

  let ty : t Type.Id.t = Type.Id.make ()

  let value i = Concrete_value.Externref (Some (Concrete_value.E (ty, i)))
end

let check_error ~expected ~got : unit Result.t =
  let ok =
    Result.err_to_string got = expected
    || String.starts_with ~prefix:expected (Result.err_to_string got)
    || ( got = `Constant_out_of_range
       || got = `Msg "constant out of range"
       || got = `Parse_fail "constant out of range" )
       && (expected = "i32 constant out of range" || expected = "i32 constant")
  in
  if not ok then begin
    Error got
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
      match Link.StringMap.find mod_id ls.Link.by_id with
      | exception Not_found -> Error (`Unbound_module mod_id)
      | exports -> Ok exports )
  in
  match Link.StringMap.find f_name exports.functions with
  | exception Not_found -> Error (`Unbound_name f_name)
  | v -> Ok (v, env_id)

let load_global_from_module ls mod_id name =
  let* exports =
    match mod_id with
    | None -> begin
      match ls.Link.last with
      | None -> Error `Unbound_last_module
      | Some (m, _env_id) -> Ok m
    end
    | Some mod_id -> (
      match Link.StringMap.find mod_id ls.Link.by_id with
      | exception Not_found -> Error (`Unbound_module mod_id)
      | exports, _env_id -> Ok exports )
  in
  match Link.StringMap.find name exports.globals with
  | exception Not_found -> Error (`Unbound_name name)
  | v -> Ok v

let compare_result_const result (const : Concrete_value.t) =
  match (result, const) with
  | Text.Result_const (Literal (Const_I32 n)), I32 n' -> n = n'
  | Result_const (Literal (Const_I64 n)), I64 n' -> n = n'
  | Result_const (Literal (Const_F32 n)), F32 n' -> n = n'
  | Result_const (Literal (Const_F64 n)), F64 n' -> n = n'
  | Result_const (Literal (Const_null Func_ht)), Ref (Funcref None) -> true
  | Result_const (Literal (Const_null Extern_ht)), Ref (Externref None) -> true
  | Result_const (Literal (Const_extern n)), Ref (Externref (Some ref)) -> begin
    match Concrete_value.cast_ref ref Host_externref.ty with
    | None -> false
    | Some n' -> n = n'
  end
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
  | _ ->
    Log.debug0 "TODO (Script.compare_result_const)@\n";
    assert false

let value_of_const : text const -> V.t Result.t = function
  | Const_I32 v -> ok @@ Concrete_value.I32 v
  | Const_I64 v -> ok @@ Concrete_value.I64 v
  | Const_F32 v -> ok @@ Concrete_value.F32 v
  | Const_F64 v -> ok @@ Concrete_value.F64 v
  | Const_null rt ->
    let+ rt = Simplified_types.convert_heap_type None rt in
    Concrete_value.ref_null rt
  | Const_extern i -> ok @@ Concrete_value.Ref (Host_externref.value i)
  | i ->
    Log.debug2 "TODO (Script.value_of_const) %a@\n" Types.pp_const i;
    assert false

let action (link_state : Concrete_value.Func.extern_func Link.state) = function
  | Text.Invoke (mod_id, f, args) -> begin
    Log.debug5 "invoke %a %s %a...@\n"
      (Format.pp_option ~none:Format.pp_nothing Format.pp_string)
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
      | Text.Module m ->
        if !curr_module = 0 then Log.debug_on := false;
        Log.debug0 "*** module@\n";
        incr curr_module;
        let+ link_state =
          Compile.until_interpret link_state ~unsafe ~optimize ~name:None m
        in
        Log.debug_on := debug_on;
        link_state
      | Assert (Assert_trap_module (m, expected)) ->
        Log.debug0 "*** assert_trap@\n";
        incr curr_module;
        let* m, link_state =
          Compile.until_link link_state ~unsafe ~optimize ~name:None m
        in
        let+ () =
          check_error_result expected
            (Interpret.Concrete.modul link_state.envs m)
        in
        link_state
      | Assert (Assert_malformed_binary _) ->
        Log.debug0 "*** assert_malformed_binary@\n";
        (* TODO: check this when binary format is supported *)
        Ok link_state
      | Assert (Assert_malformed_quote (m, expected)) ->
        Log.debug0 "*** assert_malformed_quote@\n";
        let+ () =
          match Parse.Script.from_string (String.concat "\n" m) with
          | Error got -> check_error ~expected ~got
          | Ok [ Module m ] -> (
            match Compile.until_simplify ~unsafe m with
            | Error got -> check_error ~expected ~got
            | Ok _m ->
              let got = `No_error in
              check_error ~expected ~got )
          | Ok _ -> assert false
        in
        link_state
      | Assert (Assert_invalid_binary _) ->
        Log.debug0 "*** assert_invalid_binary@\n";
        (* TODO: check this when binary format is supported *)
        Ok link_state
      | Assert (Assert_invalid (m, expected)) ->
        Log.debug0 "*** assert_invalid@\n";
        let+ () =
          match
            Compile.until_link link_state ~unsafe ~optimize ~name:None m
          with
          | Ok _ -> check_error ~expected ~got:`No_error
          | Error got -> check_error ~expected ~got
        in
        link_state
      | Assert (Assert_invalid_quote (m, expected)) ->
        Log.debug0 "*** assert_invalid_quote@\n";
        let got = Parse.Script.from_string (String.concat "\n" m) in
        let+ () = check_error_result expected got in
        link_state
      | Assert (Assert_unlinkable (m, expected)) ->
        Log.debug0 "*** assert_unlinkable@\n";
        let+ () =
          check_error_result expected
            (Compile.until_link link_state ~unsafe ~optimize ~name:None m)
        in
        link_state
      | Assert (Assert_malformed _) ->
        Log.debug0 "*** assert_malformed@\n";
        Log.err "TODO"
      | Assert (Assert_return (a, res)) ->
        Log.debug0 "*** assert_return@\n";
        let* stack = action link_state a in
        if
          List.compare_lengths res stack <> 0
          || not (List.for_all2 compare_result_const res (List.rev stack))
        then begin
          Format.pp_err "got:      %a@.expected: %a@." Stack.pp (List.rev stack)
            Text.pp_results res;
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
        if !curr_module = 1 && !registered = false then Log.debug_on := false;
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
