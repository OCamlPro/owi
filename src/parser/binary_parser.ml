(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(* binary format specification:
   https://webassembly.github.io/spec/core/binary/modules.html#binary-importsec *)

open Binary
open Syntax
open Types

module Input = struct
  type t =
    { bytes : string
    ; pt : int
    ; size : int
    ; error_msg_info : string
    }

  let is_empty input = input.size = 0

  let from_str_bytes str error_msg_info =
    let size = String.length str in
    { bytes = str; pt = 0; size; error_msg_info }

  let sub ~pos ~len error_msg_info input =
    if pos <= input.size && len <= input.size - pos then
      Ok { input with pt = input.pt + pos; size = len; error_msg_info }
    else
      Error
        (`Msg
          (Format.sprintf "length out of bounds in section %s" error_msg_info)
          )

  let sub_suffix pos error_msg_info input =
    sub ~pos ~len:(input.size - pos) error_msg_info input

  let sub_prefix len error_msg_info input = sub ~pos:0 ~len error_msg_info input

  let get n input =
    if n < input.size then Ok (String.get input.bytes (input.pt + n))
    else Error (`Msg "unexpected end of section or function")

  let get0 = get 0
end

let string_of_char_list char_list =
  let buf = Buffer.create 64 in
  List.iter (Buffer.add_char buf) char_list;
  Buffer.contents buf

let read_byte input =
  let* c = Input.get0 input in
  let+ next_input = Input.sub_suffix 1 input.error_msg_info input in
  (c, next_input)

(* https://en.wikipedia.org/wiki/LEB128#Unsigned_LEB128 *)
let read_UN n input =
  let bytes_limit = int_of_float (Float.ceil (float_of_int n /. 7.)) in
  let rec aux bytes_read n input =
    let* () =
      if n <= 0 then Error (`Msg "integer representation too long") else Ok ()
    in
    let* b, input = read_byte input in
    let bytes_read = succ bytes_read in
    let* () =
      if bytes_read > bytes_limit then
        Error (`Msg "integer representation too large")
      else Ok ()
    in
    let b = Char.code b in
    let x = Int64.of_int (b land 0x7f) in
    if b land 0x80 = 0 then Ok (x, input)
    else
      (* TODO: make this tail-rec *)
      let+ i64, input = aux bytes_read (n - 7) input in
      (Int64.logor x (Int64.shl i64 7L), input)
  in
  aux 0 n input

let read_U32 input =
  let* i64, input = read_UN 32 input in
  if i64 >= Int64.shift_left 1L 32 then Error (`Msg "integer too large")
  else Ok (Int64.to_int i64, input)

(* https://en.wikipedia.org/wiki/LEB128#Signed_LEB128 *)
let read_SN n input =
  let bytes_limit = int_of_float (Float.ceil (float_of_int n /. 7.)) in
  let rec aux bytes_read n input =
    let* () =
      if n <= 0 then Error (`Msg "integer representation too long") else Ok ()
    in
    let* b, input = read_byte input in
    let bytes_read = succ bytes_read in
    let* () =
      if bytes_read > bytes_limit then
        Error (`Msg "integer representation too large")
      else Ok ()
    in
    let b = Char.code b in
    let x = Int64.of_int (b land 0x7f) in
    if b land 0x80 = 0 then
      let x =
        if b land 0x40 = 0 then x else Int64.(logor x (logxor (-1L) 0x7fL))
      in
      Ok (x, input)
    else
      (* TODO: make this tail-rec *)
      let+ i64, input = aux bytes_read (n - 7) input in
      (Int64.logor x (Int64.shl i64 7L), input)
  in
  aux 0 n input

let read_S32 input =
  let* i64, input = read_SN 32 input in
  let max = Int64.shift_left 1L 31 in
  let min = Int64.shift_left (-1L) 31 in
  if i64 >= max || i64 < min then Error (`Msg "integer too large")
  else Ok (Int64.to_int32 i64, input)

let read_S64 input =
  let* i64, input = read_SN 64 input in
  let max = Int64.shift_left 1L 63 in
  let min = Int64.shift_left (-1L) 63 in
  if i64 >= max || i64 < min then Error (`Msg "integer too large")
  else Ok (i64, input)

let read_F32 input =
  let i32_of_byte input =
    let+ b, input = read_byte input in
    (Int32.of_int (int_of_char b), input)
  in
  let* i1, input = i32_of_byte input in
  let* i2, input = i32_of_byte input in
  let* i3, input = i32_of_byte input in
  let+ i4, input = i32_of_byte input in
  let i32 = Int32.shl i4 24l in
  let i32 = Int32.logor i32 (Int32.shl i3 16l) in
  let i32 = Int32.logor i32 (Int32.shl i2 8l) in
  let i32 = Int32.logor i32 i1 in
  (Float32.of_bits i32, input)

let read_F64 input =
  let i64_of_byte input =
    let+ b, input = read_byte input in
    (Int64.of_int (int_of_char b), input)
  in
  let* i1, input = i64_of_byte input in
  let* i2, input = i64_of_byte input in
  let* i3, input = i64_of_byte input in
  let* i4, input = i64_of_byte input in
  let* i5, input = i64_of_byte input in
  let* i6, input = i64_of_byte input in
  let* i7, input = i64_of_byte input in
  let+ i8, input = i64_of_byte input in
  let i64 = Int64.shl i8 56L in
  let i64 = Int64.logor i64 (Int64.shl i7 48L) in
  let i64 = Int64.logor i64 (Int64.shl i6 40L) in
  let i64 = Int64.logor i64 (Int64.shl i5 32L) in
  let i64 = Int64.logor i64 (Int64.shl i4 24L) in
  let i64 = Int64.logor i64 (Int64.shl i3 16L) in
  let i64 = Int64.logor i64 (Int64.shl i2 8L) in
  let i64 = Int64.logor i64 i1 in
  (Float64.of_bits i64, input)

let vector parse_elt input =
  let* nb_elt, input = read_U32 input in
  let rec loop loop_id input acc =
    if nb_elt = loop_id then Ok (List.rev acc, input)
    else
      let* acc_elt, input = parse_elt loop_id input in
      let acc = acc_elt :: acc in
      loop (loop_id + 1) input acc
  in
  loop 0 input []

let vector_no_id f input = vector (fun _id -> f) input

let deserialize_indice input : (Types.binary Types.indice * Input.t, _) result =
  let+ indice, input = read_U32 input in
  (Raw indice, input)

let deserialize_reftype input =
  let* b, input = read_byte input in
  match b with
  | '\x70' -> Ok ((Null, Func_ht), input)
  | '\x6F' -> Ok ((Null, Extern_ht), input)
  | _c -> Error (`Msg "malformed reference type")

let deserialize_valtype input =
  let* b, input = read_byte input in
  match b with
  | '\x7F' -> Ok (Num_type I32, input)
  | '\x7E' -> Ok (Num_type I64, input)
  | '\x7D' -> Ok (Num_type F32, input)
  | '\x7C' -> Ok (Num_type F64, input)
  | '\x7B' -> assert false (* (V128, input) *)
  | '\x70' -> Ok (Ref_type (Null, Func_ht), input)
  | '\x6F' -> Ok (Ref_type (Null, Extern_ht), input)
  | _c -> Error (`Msg "integer too large")

let deserialize_mut input =
  let* b, input = read_byte input in
  match b with
  | '\x00' -> Ok (Const, input)
  | '\x01' -> Ok (Var, input)
  | _c -> Error (`Msg "malformed mutability")

let deserialize_limits input =
  let* b, input = read_byte input in
  match b with
  | '\x00' ->
    let+ min, input = read_U32 input in
    ({ min; max = None }, input)
  | '\x01' ->
    let* min, input = read_U32 input in
    let+ max, input = read_U32 input in
    ({ min; max = Some max }, input)
  | _c -> Error (`Msg "integer too large")

let deserialize_memarg input =
  let* align, input = read_U32 input in
  let+ offset, input = read_U32 input in
  let align = Int32.of_int align in
  let offset = Int32.of_int offset in
  ({ align; offset }, input)

let deserialize_FC input =
  let* i, input = read_U32 input in
  match i with
  | 0 -> Ok (I_trunc_sat_f (S32, S32, S), input)
  | 1 -> Ok (I_trunc_sat_f (S32, S32, U), input)
  | 2 -> Ok (I_trunc_sat_f (S32, S64, S), input)
  | 3 -> Ok (I_trunc_sat_f (S32, S64, U), input)
  | 4 -> Ok (I_trunc_sat_f (S64, S32, S), input)
  | 5 -> Ok (I_trunc_sat_f (S64, S32, U), input)
  | 6 -> Ok (I_trunc_sat_f (S64, S64, S), input)
  | 7 -> Ok (I_trunc_sat_f (S64, S64, U), input)
  | 8 ->
    let* dataidx, input = deserialize_indice input in
    let* b, input = read_byte input in
    begin
      match b with
      | '\x00' -> Ok (Memory_init dataidx, input)
      | c -> Error (`Msg (Format.sprintf "deserialize_0xFC 8 error: char %c" c))
    end
  | 9 ->
    let* dataidx, input = deserialize_indice input in
    Ok (Data_drop dataidx, input)
  | 10 ->
    let* b, input = read_byte input in
    begin
      match b with
      | '\x00' ->
        let* b, input = read_byte input in
        begin
          match b with
          | '\x00' -> Ok (Memory_copy, input)
          | c ->
            Error (`Msg (Format.sprintf "deserialize_0xFC 10 error: char %c" c))
        end
      | c -> Error (`Msg (Format.sprintf "deserialize_0xFC 10 error: char %c" c))
    end
  | 11 ->
    let* b, input = read_byte input in
    begin
      match b with
      | '\x00' -> Ok (Memory_fill, input)
      | c -> Error (`Msg (Format.sprintf "deserialize_0xFC 11 error: char %c" c))
    end
  | 12 ->
    let* elemidx, input = deserialize_indice input in
    let* tableidx, input = deserialize_indice input in
    Ok (Table_init (tableidx, elemidx), input)
  | 13 ->
    let* elemidx, input = deserialize_indice input in
    Ok (Elem_drop elemidx, input)
  | 14 ->
    let* tableidx1, input = deserialize_indice input in
    let* tableidx2, input = deserialize_indice input in
    Ok (Table_copy (tableidx1, tableidx2), input)
  | 15 ->
    let* tableidx, input = deserialize_indice input in
    Ok (Table_grow tableidx, input)
  | 16 ->
    let* tableidx, input = deserialize_indice input in
    Ok (Table_size tableidx, input)
  | 17 ->
    let* tableidx, input = deserialize_indice input in
    Ok (Table_fill tableidx, input)
  | i -> Error (`Msg (Format.sprintf "deserialize_0xFC %d error" i))

let rec deserialize_instr block_type_array input =
  let* b, next_input = read_byte input in
  match b with
  | '\x00' -> Ok (Unreachable, next_input)
  | '\x01' -> Ok (Nop, next_input)
  | '\x02' ->
    let* b, next2_input = read_byte next_input in
    begin
      match b with
      | '\x40' ->
        let* expr, next2_input =
          deserialize_code block_type_array [] next2_input
        in
        Ok (Block (None, Some (Bt_raw (None, ([], []))), expr), next2_input)
      | '\x7F' | '\x7E' | '\x7D' | '\x7C' | '\x7B' | '\x70' | '\x6F' ->
        let* vt, next_input = deserialize_valtype next_input in
        let* expr, next_input =
          deserialize_code block_type_array [] next_input
        in
        Ok
          ( Block (None, Some (Bt_raw (None, ([ (None, vt) ], []))), expr)
          , next_input )
      | _ ->
        let* si, next_input = read_SN 33 next_input in
        let* expr, next_input =
          deserialize_code block_type_array [] next_input
        in
        let block_type = Array.get block_type_array (Int64.to_int si) in
        Ok (Block (None, Some block_type, expr), next_input)
    end
  | '\x03' ->
    let* b, next2_input = read_byte next_input in
    begin
      match b with
      | '\x40' ->
        let* expr, next2_input =
          deserialize_code block_type_array [] next2_input
        in
        Ok (Loop (None, Some (Bt_raw (None, ([], []))), expr), next2_input)
      | '\x7F' | '\x7E' | '\x7D' | '\x7C' | '\x7B' | '\x70' | '\x6F' ->
        let* vt, next_input = deserialize_valtype next_input in
        let* expr, next_input =
          deserialize_code block_type_array [] next_input
        in
        Ok
          ( Loop (None, Some (Bt_raw (None, ([ (None, vt) ], []))), expr)
          , next_input )
      | _ ->
        let* si, next_input = read_SN 33 next_input in
        let* expr, next_input =
          deserialize_code block_type_array [] next_input
        in
        let block_type = Array.get block_type_array (Int64.to_int si) in
        Ok (Loop (None, Some block_type, expr), next_input)
    end
  | '\x04' ->
    let* b, next2_input = read_byte next_input in
    begin
      match b with
      | '\x40' ->
        let* expr_then, expr_else, next2_input =
          deserialize_codes_if block_type_array [] [] next2_input
        in
        Ok
          ( If_else (None, Some (Bt_raw (None, ([], []))), expr_then, expr_else)
          , next2_input )
      | '\x7F' | '\x7E' | '\x7D' | '\x7C' | '\x7B' | '\x70' | '\x6F' ->
        let* vt, next_input = deserialize_valtype next_input in
        let* expr_then, expr_else, next_input =
          deserialize_codes_if block_type_array [] [] next_input
        in
        Ok
          ( If_else
              ( None
              , Some (Bt_raw (None, ([ (None, vt) ], [])))
              , expr_then
              , expr_else )
          , next_input )
      | _ ->
        let* si, next_input = read_SN 33 next_input in
        let* expr_then, expr_else, next_input =
          deserialize_codes_if block_type_array [] [] next_input
        in
        let block_type = Array.get block_type_array (Int64.to_int si) in
        Ok (If_else (None, Some block_type, expr_then, expr_else), next_input)
    end
  | '\x0C' ->
    let* labelidx, next_input = deserialize_indice next_input in
    Ok (Br labelidx, next_input)
  | '\x0D' ->
    let* labelidx, next_input = deserialize_indice next_input in
    Ok (Br_if labelidx, next_input)
  | '\x0F' -> Ok (Return, next_input)
  | '\x10' ->
    let* funcidx, next_input = deserialize_indice next_input in
    Ok (Call funcidx, next_input)
  | '\x11' ->
    let* Raw typeidx, next_input = deserialize_indice next_input in
    let* tableidx, next_input = deserialize_indice next_input in
    let block_type = Array.get block_type_array typeidx in
    Ok (Call_indirect (tableidx, block_type), next_input)
  | '\x1A' -> Ok (Drop, next_input)
  | '\x1B' -> Ok (Select None, next_input)
  | '\x1C' ->
    let* valtypes, next_input = vector_no_id deserialize_valtype next_input in
    Ok (Select (Some valtypes), next_input)
  | '\x20' ->
    let* localidx, next_input = deserialize_indice next_input in
    Ok (Local_get localidx, next_input)
  | '\x21' ->
    let* localidx, next_input = deserialize_indice next_input in
    Ok (Local_set localidx, next_input)
  | '\x22' ->
    let* localidx, next_input = deserialize_indice next_input in
    Ok (Local_tee localidx, next_input)
  | '\x23' ->
    let* globalidx, next_input = deserialize_indice next_input in
    Ok (Global_get globalidx, next_input)
  | '\x24' ->
    let* globalidx, next_input = deserialize_indice next_input in
    Ok (Global_set globalidx, next_input)
  | '\x25' ->
    let* tableidx, next_input = deserialize_indice next_input in
    Ok (Table_get tableidx, next_input)
  | '\x26' ->
    let* tableidx, next_input = deserialize_indice next_input in
    Ok (Table_set tableidx, next_input)
  | '\x28' ->
    let* memarg, next_input = deserialize_memarg next_input in
    Ok (I_load (S32, memarg), next_input)
  | '\x29' ->
    let* memarg, next_input = deserialize_memarg next_input in
    Ok (I_load (S64, memarg), next_input)
  | '\x2A' ->
    let* memarg, next_input = deserialize_memarg next_input in
    Ok (F_load (S32, memarg), next_input)
  | '\x2B' ->
    let* memarg, next_input = deserialize_memarg next_input in
    Ok (F_load (S64, memarg), next_input)
  | '\x2C' ->
    let* memarg, next_input = deserialize_memarg next_input in
    Ok (I_load8 (S32, S, memarg), next_input)
  | '\x2D' ->
    let* memarg, next_input = deserialize_memarg next_input in
    Ok (I_load8 (S32, U, memarg), next_input)
  | '\x2E' ->
    let* memarg, next_input = deserialize_memarg next_input in
    Ok (I_load16 (S32, S, memarg), next_input)
  | '\x2F' ->
    let* memarg, next_input = deserialize_memarg next_input in
    Ok (I_load16 (S32, U, memarg), next_input)
  | '\x30' ->
    let* memarg, next_input = deserialize_memarg next_input in
    Ok (I_load8 (S64, S, memarg), next_input)
  | '\x31' ->
    let* memarg, next_input = deserialize_memarg next_input in
    Ok (I_load8 (S64, U, memarg), next_input)
  | '\x32' ->
    let* memarg, next_input = deserialize_memarg next_input in
    Ok (I_load16 (S64, S, memarg), next_input)
  | '\x33' ->
    let* memarg, next_input = deserialize_memarg next_input in
    Ok (I_load16 (S64, U, memarg), next_input)
  | '\x34' ->
    let* memarg, next_input = deserialize_memarg next_input in
    Ok (I64_load32 (S, memarg), next_input)
  | '\x35' ->
    let* memarg, next_input = deserialize_memarg next_input in
    Ok (I64_load32 (U, memarg), next_input)
  | '\x36' ->
    let* memarg, next_input = deserialize_memarg next_input in
    Ok (I_store (S32, memarg), next_input)
  | '\x37' ->
    let* memarg, next_input = deserialize_memarg next_input in
    Ok (I_store (S64, memarg), next_input)
  | '\x38' ->
    let* memarg, next_input = deserialize_memarg next_input in
    Ok (F_store (S32, memarg), next_input)
  | '\x39' ->
    let* memarg, next_input = deserialize_memarg next_input in
    Ok (F_store (S64, memarg), next_input)
  | '\x3A' ->
    let* memarg, next_input = deserialize_memarg next_input in
    Ok (I_store8 (S32, memarg), next_input)
  | '\x3B' ->
    let* memarg, next_input = deserialize_memarg next_input in
    Ok (I_store16 (S32, memarg), next_input)
  | '\x3C' ->
    let* memarg, next_input = deserialize_memarg next_input in
    Ok (I_store8 (S64, memarg), next_input)
  | '\x3D' ->
    let* memarg, next_input = deserialize_memarg next_input in
    Ok (I_store16 (S64, memarg), next_input)
  | '\x3E' ->
    let* memarg, next_input = deserialize_memarg next_input in
    Ok (I64_store32 memarg, next_input)
  | '\x3F' ->
    let* b, next_input = read_byte next_input in
    begin
      match b with
      | '\x00' -> Ok (Memory_size, next_input)
      | c ->
        Error
          (`Msg (Format.sprintf "deserialize_instruction 0x3F error: char %c" c))
    end
  | '\x40' ->
    let* b, next_input = read_byte next_input in
    begin
      match b with
      | '\x00' -> Ok (Memory_grow, next_input)
      | c ->
        Error
          (`Msg (Format.sprintf "deserialize_instruction 0x40 error: char %c" c))
    end
  | '\x41' ->
    let* i32, next_input = read_S32 next_input in
    Ok (I32_const i32, next_input)
  | '\x42' ->
    let* i64, next_input = read_S64 next_input in
    Ok (I64_const i64, next_input)
  | '\x43' ->
    let* f32, next_input = read_F32 next_input in
    Ok (F32_const f32, next_input)
  | '\x44' ->
    let* f64, next_input = read_F64 next_input in
    Ok (F64_const f64, next_input)
  | '\x45' -> Ok (I_testop (S32, Eqz), next_input)
  | '\x46' -> Ok (I_relop (S32, Eq), next_input)
  | '\x47' -> Ok (I_relop (S32, Ne), next_input)
  | '\x48' -> Ok (I_relop (S32, Lt S), next_input)
  | '\x49' -> Ok (I_relop (S32, Lt U), next_input)
  | '\x4A' -> Ok (I_relop (S32, Gt S), next_input)
  | '\x4B' -> Ok (I_relop (S32, Gt U), next_input)
  | '\x4C' -> Ok (I_relop (S32, Le S), next_input)
  | '\x4D' -> Ok (I_relop (S32, Le U), next_input)
  | '\x4E' -> Ok (I_relop (S32, Ge S), next_input)
  | '\x4F' -> Ok (I_relop (S32, Ge U), next_input)
  | '\x50' -> Ok (I_testop (S64, Eqz), next_input)
  | '\x51' -> Ok (I_relop (S64, Eq), next_input)
  | '\x52' -> Ok (I_relop (S64, Ne), next_input)
  | '\x53' -> Ok (I_relop (S64, Lt S), next_input)
  | '\x54' -> Ok (I_relop (S64, Lt U), next_input)
  | '\x55' -> Ok (I_relop (S64, Gt S), next_input)
  | '\x56' -> Ok (I_relop (S64, Gt U), next_input)
  | '\x57' -> Ok (I_relop (S64, Le S), next_input)
  | '\x58' -> Ok (I_relop (S64, Le U), next_input)
  | '\x59' -> Ok (I_relop (S64, Ge S), next_input)
  | '\x5A' -> Ok (I_relop (S64, Ge U), next_input)
  | '\x5B' -> Ok (F_relop (S32, Eq), next_input)
  | '\x5C' -> Ok (F_relop (S32, Ne), next_input)
  | '\x5D' -> Ok (F_relop (S32, Lt), next_input)
  | '\x5E' -> Ok (F_relop (S32, Gt), next_input)
  | '\x5F' -> Ok (F_relop (S32, Le), next_input)
  | '\x60' -> Ok (F_relop (S32, Ge), next_input)
  | '\x61' -> Ok (F_relop (S64, Eq), next_input)
  | '\x62' -> Ok (F_relop (S64, Ne), next_input)
  | '\x63' -> Ok (F_relop (S64, Lt), next_input)
  | '\x64' -> Ok (F_relop (S64, Gt), next_input)
  | '\x65' -> Ok (F_relop (S64, Le), next_input)
  | '\x66' -> Ok (F_relop (S64, Ge), next_input)
  | '\x67' -> Ok (I_unop (S32, Clz), next_input)
  | '\x68' -> Ok (I_unop (S32, Ctz), next_input)
  | '\x69' -> Ok (I_unop (S32, Popcnt), next_input)
  | '\x6A' -> Ok (I_binop (S32, Add), next_input)
  | '\x6B' -> Ok (I_binop (S32, Sub), next_input)
  | '\x6C' -> Ok (I_binop (S32, Mul), next_input)
  | '\x6D' -> Ok (I_binop (S32, Div S), next_input)
  | '\x6E' -> Ok (I_binop (S32, Div U), next_input)
  | '\x6F' -> Ok (I_binop (S32, Rem S), next_input)
  | '\x70' -> Ok (I_binop (S32, Rem U), next_input)
  | '\x71' -> Ok (I_binop (S32, And), next_input)
  | '\x72' -> Ok (I_binop (S32, Or), next_input)
  | '\x73' -> Ok (I_binop (S32, Xor), next_input)
  | '\x74' -> Ok (I_binop (S32, Shl), next_input)
  | '\x75' -> Ok (I_binop (S32, Shr S), next_input)
  | '\x76' -> Ok (I_binop (S32, Shr U), next_input)
  | '\x77' -> Ok (I_binop (S32, Rotl), next_input)
  | '\x78' -> Ok (I_binop (S32, Rotr), next_input)
  | '\x79' -> Ok (I_unop (S64, Clz), next_input)
  | '\x7A' -> Ok (I_unop (S64, Ctz), next_input)
  | '\x7B' -> Ok (I_unop (S64, Popcnt), next_input)
  | '\x7C' -> Ok (I_binop (S64, Add), next_input)
  | '\x7D' -> Ok (I_binop (S64, Sub), next_input)
  | '\x7E' -> Ok (I_binop (S64, Mul), next_input)
  | '\x7F' -> Ok (I_binop (S64, Div S), next_input)
  | '\x80' -> Ok (I_binop (S64, Div U), next_input)
  | '\x81' -> Ok (I_binop (S64, Rem S), next_input)
  | '\x82' -> Ok (I_binop (S64, Rem U), next_input)
  | '\x83' -> Ok (I_binop (S64, And), next_input)
  | '\x84' -> Ok (I_binop (S64, Or), next_input)
  | '\x85' -> Ok (I_binop (S64, Xor), next_input)
  | '\x86' -> Ok (I_binop (S64, Shl), next_input)
  | '\x87' -> Ok (I_binop (S64, Shr S), next_input)
  | '\x88' -> Ok (I_binop (S64, Shr U), next_input)
  | '\x89' -> Ok (I_binop (S64, Rotl), next_input)
  | '\x8A' -> Ok (I_binop (S64, Rotr), next_input)
  | '\x8B' -> Ok (F_unop (S32, Abs), next_input)
  | '\x8C' -> Ok (F_unop (S32, Neg), next_input)
  | '\x8D' -> Ok (F_unop (S32, Ceil), next_input)
  | '\x8E' -> Ok (F_unop (S32, Floor), next_input)
  | '\x8F' -> Ok (F_unop (S32, Trunc), next_input)
  | '\x90' -> Ok (F_unop (S32, Nearest), next_input)
  | '\x91' -> Ok (F_unop (S32, Sqrt), next_input)
  | '\x92' -> Ok (F_binop (S32, Add), next_input)
  | '\x93' -> Ok (F_binop (S32, Sub), next_input)
  | '\x94' -> Ok (F_binop (S32, Mul), next_input)
  | '\x95' -> Ok (F_binop (S32, Div), next_input)
  | '\x96' -> Ok (F_binop (S32, Min), next_input)
  | '\x97' -> Ok (F_binop (S32, Max), next_input)
  | '\x98' -> Ok (F_binop (S32, Copysign), next_input)
  | '\x99' -> Ok (F_unop (S64, Abs), next_input)
  | '\x9A' -> Ok (F_unop (S64, Neg), next_input)
  | '\x9B' -> Ok (F_unop (S64, Ceil), next_input)
  | '\x9C' -> Ok (F_unop (S64, Floor), next_input)
  | '\x9D' -> Ok (F_unop (S64, Trunc), next_input)
  | '\x9E' -> Ok (F_unop (S64, Nearest), next_input)
  | '\x9F' -> Ok (F_unop (S64, Sqrt), next_input)
  | '\xA0' -> Ok (F_binop (S64, Add), next_input)
  | '\xA1' -> Ok (F_binop (S64, Sub), next_input)
  | '\xA2' -> Ok (F_binop (S64, Mul), next_input)
  | '\xA3' -> Ok (F_binop (S64, Div), next_input)
  | '\xA4' -> Ok (F_binop (S64, Min), next_input)
  | '\xA5' -> Ok (F_binop (S64, Max), next_input)
  | '\xA6' -> Ok (F_binop (S64, Copysign), next_input)
  | '\xA7' -> Ok (I32_wrap_i64, next_input)
  | '\xA8' -> Ok (I_trunc_f (S32, S32, S), next_input)
  | '\xA9' -> Ok (I_trunc_f (S32, S32, U), next_input)
  | '\xAA' -> Ok (I_trunc_f (S32, S64, S), next_input)
  | '\xAB' -> Ok (I_trunc_f (S32, S64, U), next_input)
  | '\xAC' -> Ok (I64_extend_i32 S, next_input)
  | '\xAD' -> Ok (I64_extend_i32 U, next_input)
  | '\xAE' -> Ok (I_trunc_f (S64, S32, S), next_input)
  | '\xAF' -> Ok (I_trunc_f (S64, S32, U), next_input)
  | '\xB0' -> Ok (I_trunc_f (S64, S64, S), next_input)
  | '\xB1' -> Ok (I_trunc_f (S64, S64, U), next_input)
  | '\xB2' -> Ok (F_convert_i (S32, S32, S), next_input)
  | '\xB3' -> Ok (F_convert_i (S32, S32, U), next_input)
  | '\xB4' -> Ok (F_convert_i (S32, S64, S), next_input)
  | '\xB5' -> Ok (F_convert_i (S32, S64, U), next_input)
  | '\xB6' -> Ok (F32_demote_f64, next_input)
  | '\xB7' -> Ok (F_convert_i (S64, S32, S), next_input)
  | '\xB8' -> Ok (F_convert_i (S64, S32, U), next_input)
  | '\xB9' -> Ok (F_convert_i (S64, S64, S), next_input)
  | '\xBA' -> Ok (F_convert_i (S64, S64, U), next_input)
  | '\xBB' -> Ok (F64_promote_f32, next_input)
  | '\xBC' -> Ok (I_reinterpret_f (S32, S32), next_input)
  | '\xBD' -> Ok (I_reinterpret_f (S64, S64), next_input)
  | '\xBE' -> Ok (F_reinterpret_i (S32, S32), next_input)
  | '\xBF' -> Ok (F_reinterpret_i (S64, S64), next_input)
  | '\xC0' -> Ok (I_extend8_s S32, next_input)
  | '\xC1' -> Ok (I_extend16_s S32, next_input)
  | '\xC2' -> Ok (I_extend8_s S64, next_input)
  | '\xC3' -> Ok (I_extend16_s S64, next_input)
  | '\xC4' -> Ok (I64_extend32_s, next_input)
  | '\xD0' ->
    let* (_null, reftype), next_input = deserialize_reftype next_input in
    Ok (Ref_null reftype, next_input)
  | '\xD1' -> Ok (Ref_is_null, next_input)
  | '\xD2' ->
    let* funcidx, next_input = deserialize_indice next_input in
    Ok (Ref_func funcidx, next_input)
  | '\xFC' -> deserialize_FC input
  | _c -> Error (`Msg "END opcode expected")

and deserialize_code block_type_array instr_list input :
  ('a expr * Input.t, _) result =
  let* b, next_input = read_byte input in
  match b with
  | '\x0B' -> Ok (List.rev instr_list, next_input)
  | _ ->
    let* instr, input = deserialize_instr block_type_array input in
    let instr_list = instr :: instr_list in
    deserialize_code block_type_array instr_list input

and deserialize_codes_if block_type_array instr_list_then instr_list_else input
  : ('a expr * 'a expr * Input.t, _) result =
  let* b, next_input = read_byte input in
  match b with
  | '\x05' ->
    let* instr_list_else, next_input =
      deserialize_code block_type_array instr_list_else next_input
    in
    Ok (List.rev instr_list_then, instr_list_else, next_input)
  | '\x0B' -> Ok (List.rev instr_list_then, List.rev instr_list_else, next_input)
  | _ ->
    let* instr, input = deserialize_instr block_type_array input in
    let instr_list_then = instr :: instr_list_then in
    deserialize_codes_if block_type_array instr_list_then instr_list_else input

type ('a, 'b) import =
  | Func of int
  | Table of limits * 'a ref_type
  | Mem of limits
  | Global of mut * 'b val_type

let magic_check str =
  if String.length str < 4 then Error (`Msg "unexpected end")
  else
    let magic = String.sub str 0 4 in
    if String.equal magic "\x00\x61\x73\x6d" then Ok ()
    else Error (`Msg "magic header not detected")

let version_check str =
  if String.length str < 8 then Error (`Msg "unexpected end")
  else
    let version = String.sub str 4 4 in
    if String.equal version "\x01\x00\x00\x00" then Ok ()
    else Error (`Msg "unknown binary version")

let section_parse input error_msg_info ~expected_id default
  section_content_parse =
  if Input.is_empty input then Ok (default, input)
  else
    let* id = Input.get0 input in
    if id = expected_id then
      let* input = Input.sub_suffix 1 error_msg_info input in
      let* size, input = read_U32 input in
      let* section_input = Input.sub_prefix size error_msg_info input in
      let* next_input = Input.sub_suffix size error_msg_info input in
      let* res, after_section_input = section_content_parse section_input in
      if not (Input.is_empty after_section_input) then
        Error (`Msg "section size mismatch")
      else Ok (res, next_input)
    else Ok (default, input)

let section_custom input =
  let consume_to_end x error_msg_info input =
    let+ input = Input.sub ~pos:0 ~len:0 error_msg_info input in
    (x, input)
  in
  section_parse input "custom_section" ~expected_id:'\x00' None @@ fun input ->
  let* name, input = vector_no_id read_byte input in
  let+ (), input = consume_to_end () "custom_section" input in
  (Some name, input)

let section_type input =
  section_parse input "type_section" ~expected_id:'\x01' []
    (vector (fun id input ->
         let* fcttype, input = read_byte input in
         let* () =
           if fcttype <> '\x60' then
             Error (`Msg "integer representation too long")
           else Ok ()
         in
         let* params, input = vector_no_id deserialize_valtype input in
         let+ results, input = vector_no_id deserialize_valtype input in
         let params = List.map (fun param -> (None, param)) params in
         (Bt_raw (Some (Raw id), (params, results)), input) ) )

let section_import input =
  section_parse input "import_section" ~expected_id:'\x02' []
  @@ vector_no_id (fun input ->
         let* modul, input = vector_no_id read_byte input in
         let* name, input = vector_no_id read_byte input in
         let modul = string_of_char_list modul in
         let name = string_of_char_list name in
         let* import_typeidx, input = read_byte input in
         match import_typeidx with
         | '\x00' ->
           let* typeidx, input = read_U32 input in
           Ok ((modul, name, Func typeidx), input)
         | '\x01' ->
           let* ref_type, input = deserialize_reftype input in
           let* limits, input = deserialize_limits input in
           Ok ((modul, name, Table (limits, ref_type)), input)
         | '\x02' ->
           let* limits, input = deserialize_limits input in
           Ok ((modul, name, Mem limits), input)
         | '\x03' ->
           let* val_type, input = deserialize_valtype input in
           let* mut, input = deserialize_mut input in
           Ok ((modul, name, Global (mut, val_type)), input)
         | _c -> Error (`Msg "integer too large") )

let section_function input =
  section_parse input "function_section" ~expected_id:'\x03' []
  @@ vector_no_id read_U32

let section_table input =
  section_parse input "table_section" ~expected_id:'\x04' []
  @@ vector_no_id (fun input ->
         let* ref_type, input = deserialize_reftype input in
         let+ limits, input = deserialize_limits input in
         ((limits, ref_type), input) )

let section_memory input =
  section_parse input "memory_section" ~expected_id:'\x05' []
  @@ vector_no_id (fun input ->
         let+ limits, input = deserialize_limits input in
         ((None, limits), input) )

let section_global block_type_array input =
  section_parse input "global_section" ~expected_id:'\x06' []
  @@ vector_no_id (fun input ->
         let* val_type, input = deserialize_valtype input in
         let* mut, input = deserialize_mut input in
         let+ expr, input = deserialize_code block_type_array [] input in
         ((expr, (mut, val_type)), input) )

let section_export input =
  section_parse input "export_section" ~expected_id:'\x07' []
  @@ vector_no_id (fun input ->
         let* name, input = vector_no_id read_byte input in
         let name = string_of_char_list name in
         let* export_typeidx, input = read_byte input in
         let+ id, input = read_U32 input in
         ((export_typeidx, { id; name }), input) )

let section_start input =
  section_parse input "start_section" ~expected_id:'\x08' None @@ fun input ->
  let+ idx_start_func, input = read_U32 input in
  (Some idx_start_func, input)

let section_element block_type_array input =
  section_parse input "element_section" ~expected_id:'\x09' []
    (vector_no_id (fun input ->
         let* i, input = read_U32 input in
         match i with
         | 0 ->
           let* expr, input = deserialize_code block_type_array [] input in
           let* funcidx_l, input = vector_no_id deserialize_indice input in
           let init =
             List.map (fun funcidx -> [ Ref_func funcidx ]) funcidx_l
           in
           Ok
             ( { id = None
               ; typ = (Null, Func_ht)
               ; init
               ; mode = Elem_active (Some 0, expr)
               }
             , input )
         | 1 ->
           let* elemkind, input = read_byte input in
           begin
             match elemkind with
             | '\x00' ->
               let* funcidx_l, input = vector_no_id deserialize_indice input in
               let init =
                 List.map (fun funcidx -> [ Ref_func funcidx ]) funcidx_l
               in
               Ok
                 ( { id = None
                   ; typ = (Null, Func_ht)
                   ; init
                   ; mode = Elem_passive
                   }
                 , input )
             | c ->
               Error (`Msg (Format.sprintf "element_section 1 error: char %c" c))
           end
         | 2 ->
           let* Raw tableidx, input = deserialize_indice input in
           let* expr, input = deserialize_code block_type_array [] input in
           let* elemkind, input = read_byte input in
           begin
             match elemkind with
             | '\x00' ->
               let* funcidx_l, input = vector_no_id deserialize_indice input in
               let init =
                 List.map (fun funcidx -> [ Ref_func funcidx ]) funcidx_l
               in
               Ok
                 ( { id = None
                   ; typ = (Null, Func_ht)
                   ; init
                   ; mode = Elem_active (Some tableidx, expr)
                   }
                 , input )
             | c ->
               Error (`Msg (Format.sprintf "element_section 2 error: char %c" c))
           end
         | 3 ->
           let* elemkind, input = read_byte input in
           begin
             match elemkind with
             | '\x00' ->
               let* funcidx_l, input = vector_no_id deserialize_indice input in
               let init =
                 List.map (fun funcidx -> [ Ref_func funcidx ]) funcidx_l
               in
               Ok
                 ( { id = None
                   ; typ = (Null, Func_ht)
                   ; init
                   ; mode = Elem_declarative
                   }
                 , input )
             | c ->
               Error (`Msg (Format.sprintf "element_section 3 error: char %c" c))
           end
         | 4 ->
           let* expr, input = deserialize_code block_type_array [] input in
           let* init, input =
             vector_no_id
               (fun input -> deserialize_code block_type_array [] input)
               input
           in
           Ok
             ( { id = None
               ; typ = (Null, Func_ht)
               ; init
               ; mode = Elem_active (Some 0, expr)
               }
             , input )
         | 5 ->
           let* typ, input = deserialize_reftype input in
           let* init, input =
             vector_no_id
               (fun input -> deserialize_code block_type_array [] input)
               input
           in
           Ok ({ id = None; typ; init; mode = Elem_passive }, input)
         | 6 ->
           let* Raw tableidx, input = deserialize_indice input in
           let* expr, input = deserialize_code block_type_array [] input in
           let* typ, input = deserialize_reftype input in
           let* init, input =
             vector_no_id
               (fun input -> deserialize_code block_type_array [] input)
               input
           in
           Ok
             ( { id = None; typ; init; mode = Elem_active (Some tableidx, expr) }
             , input )
         | 7 ->
           let* typ, input = deserialize_reftype input in
           let* init, input =
             vector_no_id
               (fun input -> deserialize_code block_type_array [] input)
               input
           in
           Ok ({ id = None; typ; init; mode = Elem_declarative }, input)
         | i -> Error (`Msg (Format.sprintf "element_section %d error" i)) ) )

let section_code block_type_array input =
  section_parse input "code_section" ~expected_id:'\x0A' []
    (vector_no_id (fun input ->
         let* _size, input = read_U32 input in
         let* locals, input =
           vector_no_id
             (fun input ->
               let* nb, input = read_U32 input in
               let+ vt, input = deserialize_valtype input in
               (List.init nb (fun _ -> (None, vt)), input) )
             input
         in
         let locals = List.flatten locals in
         let+ code, input = deserialize_code block_type_array [] input in
         ((locals, code), input) ) )

let section_data block_type_array input =
  section_parse input "data_section" ~expected_id:'\x0B' []
    (vector_no_id (fun input ->
         let* i, input = read_U32 input in
         match i with
         | 0 ->
           let* expr, input = deserialize_code block_type_array [] input in
           let* bytes, input = vector_no_id read_byte input in
           let init = string_of_char_list bytes in
           Ok ({ id = None; init; mode = Data_active (Some 0, expr) }, input)
         | 1 ->
           let* bytes, input = vector_no_id read_byte input in
           let init = string_of_char_list bytes in
           Ok ({ id = None; init; mode = Data_passive }, input)
         | 2 ->
           let* memidx, input = read_U32 input in
           let* expr, input = deserialize_code block_type_array [] input in
           let* bytes, input = vector_no_id read_byte input in
           let init = string_of_char_list bytes in
           Ok
             ({ id = None; init; mode = Data_active (Some memidx, expr) }, input)
         | i -> Error (`Msg (Format.sprintf "data_section %d error" i)) ) )

let sections_iterate (modul : Binary.modul) (input : Input.t) =
  (* Custom *)
  let* _custom_section, input = section_custom input in

  (* Type *)
  let* block_type_list_type, input = section_type input in
  let block_type_array = Array.of_list block_type_list_type in

  (* Imports *)
  let* imports, input = section_import input in

  (* Function *)
  let* typeidx_list_func, input = section_function input in

  (* Tables *)
  let* tables, input = section_table input in

  (* Memory *)
  let* mems, input = section_memory input in

  (* Globals *)
  let* globals, input = section_global block_type_array input in

  (* Exports *)
  let* exports, input = section_export input in

  (* Start *)
  let* start, input = section_start input in

  (* Elements *)
  let* elems, input = section_element block_type_array input in

  (* Code *)
  let* locals_code_list, input = section_code block_type_array input in
  let* () =
    if List.length typeidx_list_func <> List.length locals_code_list then
      Error (`Msg "function and code section have inconsistent lengths")
    else Ok ()
  in

  (* Data *)
  let+ datas, input = section_data block_type_array input in

  let* () =
    if not @@ Input.is_empty input then Error (`Msg "malformed section id")
    else Ok ()
  in

  let indexed_of_list l = List.mapi Indexed.return l in

  (* Memories *)
  let mem =
    let local = List.map (fun mem -> Runtime.Local mem) mems in
    let imported =
      List.filter_map
        (function
          | modul, name, Mem desc ->
            Option.some
            @@ Runtime.Imported { modul; name; assigned_name = None; desc }
          | _not_a_memory_import -> None )
        imports
    in
    let values = indexed_of_list (local @ imported) in
    { Named.values; named = String_map.empty }
  in

  (* Globals *)
  let global =
    let local =
      List.map
        (fun (init, typ) -> Runtime.Local { typ; init; id = None })
        globals
    in
    let imported =
      List.filter_map
        (function
          | modul, name, Global (mut, val_type) ->
            Option.some
            @@ Runtime.Imported
                 { modul; name; assigned_name = None; desc = (mut, val_type) }
          | _not_a_global_import -> None )
        imports
    in
    let values = indexed_of_list (local @ imported) in
    { Named.values; named = String_map.empty }
  in

  (* Functions *)
  let func =
    let local =
      List.map2
        (fun typeidx (locals, body) ->
          Runtime.Local
            { type_f = Array.get block_type_array typeidx
            ; locals
            ; body
            ; id = None
            } )
        typeidx_list_func locals_code_list
    in
    let imported =
      List.filter_map
        (function
          | modul, name, Func typeidx ->
            Option.some
            @@ Runtime.Imported
                 { modul
                 ; name
                 ; assigned_name = None
                 ; desc = Array.get block_type_array typeidx
                 }
          | _not_a_function_import -> None )
        imports
    in
    let values = indexed_of_list (local @ imported) in
    { Named.values; named = String_map.empty }
  in

  (* Tables *)
  let table =
    let local = List.map (fun tbl -> Runtime.Local (None, tbl)) tables in
    let imported =
      List.filter_map
        (function
          | modul, name, Table (limits, ref_type) ->
            Option.some
            @@ Runtime.Imported
                 { modul
                 ; name
                 ; assigned_name = None
                 ; desc = (limits, ref_type)
                 }
          | _not_a_table_import -> None )
        imports
    in
    let values = indexed_of_list (local @ imported) in
    { Named.values; named = String_map.empty }
  in

  (* Elems *)
  let elem =
    let values = indexed_of_list elems in
    { Named.values; named = String_map.empty }
  in

  (* Data *)
  let data =
    let values = indexed_of_list datas in
    { Named.values; named = String_map.empty }
  in

  (* Exports *)
  let empty_exports = { global = []; mem = []; table = []; func = [] } in
  let exports =
    List.fold_left
      (fun (exports : exports) (export_typeidx, export) ->
        match export_typeidx with
        | '\x00' ->
          let func = export :: exports.func in
          { exports with func }
        | '\x01' ->
          let table = export :: exports.table in
          { exports with table }
        | '\x02' ->
          let mem = export :: exports.mem in
          { exports with mem }
        | '\x03' ->
          let global = export :: exports.global in
          { exports with global }
        | _ -> failwith "deserialize_exportdesc error" )
      empty_exports exports
  in

  Ok { modul with global; mem; elem; func; table; start; data; exports }

let from_string content =
  let* () = magic_check content in
  let* () = version_check content in
  let* input =
    Input.from_str_bytes content "full_file" |> Input.sub_suffix 8 "full_file"
  in
  let* m = sections_iterate Binary.empty_modul input in
  m

let from_channel chan =
  let content = In_channel.input_all chan in
  from_string content

let from_file (filename : Fpath.t) =
  let* res =
    Bos.OS.File.with_ic filename (fun chan () -> from_channel chan) ()
  in
  res
