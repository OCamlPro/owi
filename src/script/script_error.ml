(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

let check_error ~expected ~got : unit Result.t =
  let ok =
    String.equal (Result.err_to_string got) expected
    || String.starts_with ~prefix:expected (Result.err_to_string got)
    ||
    match got with
    | (`Msg s | `Parse_fail s)
      when String.starts_with ~prefix:"constant out of range" s ->
      String.starts_with ~prefix:"i32 constant" expected
      || String.equal expected "offset out of range"
    | `Constant_out_of_range ->
      String.starts_with ~prefix:"i32 constant" expected
      || String.equal expected "table size"
      || String.equal expected "memory size"
    | `Parse_fail "unexpected end of section or function"
    | `Msg "unexpected end of section or function" ->
      String.equal expected "section size mismatch"
    | `Parse_fail "END opcode expected" ->
      String.equal expected "illegal opcode"
      || String.equal expected "unexpected end"
      || String.equal expected "unexpected end of section or function"
    | (`Msg s | `Parse_fail s)
      when String.starts_with s ~prefix:"integer representation too long" ->
      String.equal expected "unexpected end of section or function"
      || String.equal expected "unexpected end of section or function"
      || String.equal expected "length out of bounds"
      || String.equal expected "unexpected end"
      || String.equal expected "integer too large"
    | `Parse_fail "integer too large (read_limits)" ->
      String.equal expected "integer representation too long"
      || String.equal expected "malformed limits flags"
    | `Parse_fail "offset out of range" ->
      String.equal expected "integer representation too long"
    | (`Msg s | `Parse_fail s)
      when String.equal s "function and code section have inconsistent lengths"
           || String.equal s
                "data count and data section have inconsistent lengths"
           || String.equal s "malformed section id" ->
      String.equal expected "unexpected content after last section"
    | `Empty_identifier -> String.equal expected "unknown operator"
    | `Offset_out_of_range ->
      (* TODO: should be removed *)
      String.equal "alignment must not be larger than natural" expected
    | `Msg got -> String.equal got expected
    | _ -> false
  in
  if not ok then begin
    Error (`Failed_with_but_expected (got, expected))
  end
  else Ok ()

let check_result ~expected ~got =
  match got with
  | Ok _whatever -> Error (`Did_not_fail_but_expected expected)
  | Error got -> check_error ~expected ~got
