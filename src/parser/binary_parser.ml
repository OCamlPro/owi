(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(* binary format specification:
   https://webassembly.github.io/spec/core/binary/modules.html#binary-importsec *)

open Binary
open Syntax
open Types

module Input : sig
  type t

  val is_empty : t -> bool

  val size : t -> int

  val sub_suffix : int -> t -> t Result.t

  val sub_prefix : int -> t -> t Result.t

  val get : int -> t -> char option

  val of_string : string -> t

  val sub : pos:int -> len:int -> t -> t Result.t
end = struct
  type t =
    { bytes : string
    ; pt : int
    ; size : int
    }

  let size s = s.size

  let is_empty input = input.size = 0

  let of_string str =
    let size = String.length str in
    { bytes = str; pt = 0; size }

  let sub ~pos ~len input =
    if pos <= input.size && len <= input.size - pos then
      Ok { input with pt = input.pt + pos; size = len }
    else Fmt.error_msg "length out of bounds in section"

  let sub_suffix pos input = sub ~pos ~len:(input.size - pos) input

  let sub_prefix len input = sub ~pos:0 ~len input

  let get n input =
    if n < input.size then Some (String.get input.bytes (input.pt + n))
    else None
end

let string_of_char_list char_list =
  let buf = Buffer.create 64 in
  List.iter (Buffer.add_char buf) char_list;
  Buffer.contents buf

let read_byte ~msg input =
  match Input.get 0 input with
  | None -> Fmt.error_msg "%s" msg
  | Some c ->
    let+ next_input = Input.sub_suffix 1 input in
    (c, next_input)

(* https://en.wikipedia.org/wiki/LEB128#Unsigned_LEB128 *)
let read_UN n input =
  let rec aux n input =
    let* () =
      if n <= 0 then Fmt.error_msg "integer representation too long" else Ok ()
    in
    let* b, input = read_byte ~msg:"integer representation too long" input in
    let b = Char.code b in
    let* () =
      if n >= 7 || b land 0x7f < 1 lsl n then Ok ()
      else Fmt.error_msg "integer too large"
    in
    let x = Int64.of_int (b land 0x7f) in
    if b land 0x80 = 0 then Ok (x, input)
    else
      (* TODO: make this tail-rec *)
      let+ i64, input = aux (n - 7) input in
      (Int64.logor x (Int64.shl i64 7L), input)
  in
  aux n input

let read_U32 input =
  let+ i64, input = read_UN 32 input in
  (Int64.to_int i64, input)

(* https://en.wikipedia.org/wiki/LEB128#Signed_LEB128 *)
let read_SN n input =
  let rec aux n input =
    let* () =
      if n <= 0 then Fmt.error_msg "integer representation too long" else Ok ()
    in
    let* b, input = read_byte ~msg:"integer representation too long" input in
    let b = Char.code b in
    let mask = (-1 lsl (n - 1)) land 0x7f in
    let* () =
      if n >= 7 || b land mask = 0 || b land mask = mask then Ok ()
      else Fmt.error_msg "integer too large"
    in
    let x = Int64.of_int (b land 0x7f) in
    if b land 0x80 = 0 then
      let x =
        if b land 0x40 = 0 then x else Int64.(logor x (logxor (-1L) 0x7fL))
      in
      Ok (x, input)
    else
      (* TODO: make this tail-rec *)
      let+ i64, input = aux (n - 7) input in
      (Int64.logor x (Int64.shl i64 7L), input)
  in
  aux n input

let read_S7 input =
  let+ i64, input = read_SN 7 input in
  (Int64.to_int i64, input)

let read_S32 input =
  let+ i64, input = read_SN 32 input in
  (Int64.to_int32 i64, input)

let read_S33 input =
  let+ i64, input = read_SN 33 input in
  (i64, input)

let read_S64 input =
  let+ i64, input = read_SN 64 input in
  (i64, input)

let read_F32 input =
  let i32_of_byte input =
    let+ b, input = read_byte ~msg:"read_F32" input in
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
    let+ b, input = read_byte ~msg:"read_F64" input in
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

let check_end_opcode ?unexpected_eoi_msg input =
  let msg = Option.value unexpected_eoi_msg ~default:"END opcode expected" in
  match read_byte ~msg input with
  | Ok ('\x0B', input) -> Ok input
  | Ok (c, _input) ->
    Fmt.error_msg "END opcode expected (got %s instead)" (Char.escaped c)
  | Error _ as e -> e

let check_zero_opcode input =
  let msg = "zero byte expected" in
  match read_byte ~msg input with
  | Ok ('\x00', input) -> Ok input
  | Ok (c, _input) -> Fmt.error_msg "%s (got %s instead)" msg (Char.escaped c)
  | Error _ as e -> e

let read_bytes ~msg input = vector_no_id (read_byte ~msg) input

let read_indice input : (Types.binary Types.indice * Input.t, _) result =
  let+ indice, input = read_U32 input in
  (Raw indice, input)

let read_numtype input =
  let* b, input = read_S7 input in
  match b with
  | -0x01 -> Ok (I32, input)
  | -0x02 -> Ok (I64, input)
  | -0x03 -> Ok (F32, input)
  | -0x04 -> Ok (F64, input)
  | b -> Fmt.error_msg "malformed number type: %d" b

let read_vectype input =
  let* b, _input = read_S7 input in
  match b with
  | -0x05 ->
    (* V128 *)
    assert false
  | b -> Fmt.error_msg "malformed vector type: %d" b

let read_reftype input =
  let* b, input = read_S7 input in
  match b with
  | -0x10 -> Ok ((Null, Func_ht), input)
  | -0x11 -> Ok ((Null, Extern_ht), input)
  | b -> Fmt.error_msg "malformed reference type: %d" b

let read_valtype input =
  match read_numtype input with
  | Ok (t, input) -> Ok (Num_type t, input)
  | Error _ -> (
    match read_vectype input with
    | Ok (_t, _input) -> assert false
    | Error _ -> (
      match read_reftype input with
      | Ok (t, input) -> Ok (Ref_type t, input)
      | Error _ as e -> e ) )

let read_valtypes input = vector_no_id read_valtype input

let read_mut input =
  let* b, input = read_byte ~msg:"read_mut" input in
  match b with
  | '\x00' -> Ok (Const, input)
  | '\x01' -> Ok (Var, input)
  | _c -> Fmt.error_msg "malformed mutability"

let read_limits input =
  let* b, input = read_byte ~msg:"read_limits" input in
  match b with
  | '\x00' ->
    let+ min, input = read_U32 input in
    ({ min; max = None }, input)
  | '\x01' ->
    let* min, input = read_U32 input in
    let+ max, input = read_U32 input in
    ({ min; max = Some max }, input)
  | _c -> Fmt.error_msg "integer too large (read_limits)"

let read_memarg max_align input =
  let* align, input = read_U32 input in
  if align >= max_align then Fmt.error_msg "malformed memop flags"
  else
    let+ offset, input = read_U32 input in
    let align = Int32.of_int align in
    let offset = Int32.of_int offset in
    ({ align; offset }, input)

let read_FC input =
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
    let* dataidx, input = read_indice input in
    let+ input = check_zero_opcode input in
    (Memory_init dataidx, input)
  | 9 ->
    let+ dataidx, input = read_indice input in
    (Data_drop dataidx, input)
  | 10 ->
    let* input = check_zero_opcode input in
    let+ input = check_zero_opcode input in
    (Memory_copy, input)
  | 11 ->
    let+ input = check_zero_opcode input in
    (Memory_fill, input)
  | 12 ->
    let* elemidx, input = read_indice input in
    let+ tableidx, input = read_indice input in
    (Table_init (tableidx, elemidx), input)
  | 13 ->
    let+ elemidx, input = read_indice input in
    (Elem_drop elemidx, input)
  | 14 ->
    let* tableidx1, input = read_indice input in
    let+ tableidx2, input = read_indice input in
    (Table_copy (tableidx1, tableidx2), input)
  | 15 ->
    let+ tableidx, input = read_indice input in
    (Table_grow tableidx, input)
  | 16 ->
    let+ tableidx, input = read_indice input in
    (Table_size tableidx, input)
  | 17 ->
    let+ tableidx, input = read_indice input in
    (Table_fill tableidx, input)
  | i -> Fmt.error_msg "illegal opcode (1) %i" i

let block_type_of_rec_type t =
  (* TODO: this is a ugly hack, it is necessary for now and should be removed at some point... *)
  match t with
  | [ (_id, (_final, _subtypes, Def_func_t (pt, rt))) ] ->
    Bt_raw (None, (pt, rt))
  | _ -> assert false

let read_block_type types input =
  match read_S33 input with
  | Ok (i, input) when Int64.ge i 0L ->
    let block_type = block_type_of_rec_type types.(Int64.to_int i) in
    Ok (block_type, input)
  | Error _ | Ok _ -> begin
    match read_byte ~msg:"read_block_type" input with
    | Ok ('\x40', input) -> Ok (Bt_raw (None, ([], [])), input)
    | Error _ | Ok _ ->
      let* vt, input = read_valtype input in
      let pt, rt = ([], [ vt ]) in
      Ok (Bt_raw (None, (pt, rt)), input)
  end

let rec read_instr types input =
  let* b, input = read_byte ~msg:"read_instr" input in
  match b with
  | '\x00' -> Ok (Unreachable, input)
  | '\x01' -> Ok (Nop, input)
  | '\x02' ->
    let* bt, input = read_block_type types input in
    let* expr, input = read_expr types input in
    let+ input = check_end_opcode input in
    (Block (None, Some bt, expr), input)
  | '\x03' ->
    let* bt, input = read_block_type types input in
    let* expr, input = read_expr types input in
    let+ input = check_end_opcode input in
    (Loop (None, Some bt, expr), input)
  | '\x04' ->
    let* bt, input = read_block_type types input in
    let* expr1, input = read_expr types input in
    let* expr2, input =
      begin
        match read_byte ~msg:"read_instr (0x04)" input with
        | Ok ('\x05', input) -> read_expr types input
        | Ok _ | Error _ -> Ok ([], input)
      end
    in
    let+ input = check_end_opcode input in
    (If_else (None, Some bt, expr1, expr2), input)
  | '\x05' -> Fmt.error_msg "misplaced ELSE opcode"
  | '\x0B' -> Fmt.error_msg "misplaced END opcode"
  | '\x0C' ->
    let+ labelidx, input = read_indice input in
    (Br labelidx, input)
  | '\x0D' ->
    let+ labelidx, input = read_indice input in
    (Br_if labelidx, input)
  | '\x0E' ->
    let* xs, input = vector_no_id read_indice input in
    let xs = Array.of_list xs in
    let+ x, input = read_indice input in
    (Br_table (xs, x), input)
  | '\x0F' -> Ok (Return, input)
  | '\x10' ->
    let+ funcidx, input = read_indice input in
    (Call funcidx, input)
  | '\x11' ->
    let* Raw typeidx, input = read_indice input in
    let+ tableidx, input = read_indice input in
    (Call_indirect (tableidx, block_type_of_rec_type types.(typeidx)), input)
  | '\x1A' -> Ok (Drop, input)
  | '\x1B' -> Ok (Select None, input)
  | '\x1C' ->
    let+ valtypes, input = read_valtypes input in
    (Select (Some valtypes), input)
  | '\x20' ->
    let+ localidx, input = read_indice input in
    (Local_get localidx, input)
  | '\x21' ->
    let+ localidx, input = read_indice input in
    (Local_set localidx, input)
  | '\x22' ->
    let+ localidx, input = read_indice input in
    (Local_tee localidx, input)
  | '\x23' ->
    let+ globalidx, input = read_indice input in
    (Global_get globalidx, input)
  | '\x24' ->
    let+ globalidx, input = read_indice input in
    (Global_set globalidx, input)
  | '\x25' ->
    let+ tableidx, input = read_indice input in
    (Table_get tableidx, input)
  | '\x26' ->
    let+ tableidx, input = read_indice input in
    (Table_set tableidx, input)
  | '\x28' ->
    let+ memarg, input = read_memarg 32 input in
    (I_load (S32, memarg), input)
  | '\x29' ->
    let+ memarg, input = read_memarg 64 input in
    (I_load (S64, memarg), input)
  | '\x2A' ->
    let+ memarg, input = read_memarg 32 input in
    (F_load (S32, memarg), input)
  | '\x2B' ->
    let+ memarg, input = read_memarg 64 input in
    (F_load (S64, memarg), input)
  | '\x2C' ->
    let+ memarg, input = read_memarg 32 input in
    (I_load8 (S32, S, memarg), input)
  | '\x2D' ->
    let+ memarg, input = read_memarg 32 input in
    (I_load8 (S32, U, memarg), input)
  | '\x2E' ->
    let+ memarg, input = read_memarg 32 input in
    (I_load16 (S32, S, memarg), input)
  | '\x2F' ->
    let+ memarg, input = read_memarg 32 input in
    (I_load16 (S32, U, memarg), input)
  | '\x30' ->
    let+ memarg, input = read_memarg 64 input in
    (I_load8 (S64, S, memarg), input)
  | '\x31' ->
    let+ memarg, input = read_memarg 64 input in
    (I_load8 (S64, U, memarg), input)
  | '\x32' ->
    let+ memarg, input = read_memarg 64 input in
    (I_load16 (S64, S, memarg), input)
  | '\x33' ->
    let+ memarg, input = read_memarg 64 input in
    (I_load16 (S64, U, memarg), input)
  | '\x34' ->
    let+ memarg, input = read_memarg 32 input in
    (I64_load32 (S, memarg), input)
  | '\x35' ->
    let+ memarg, input = read_memarg 32 input in
    (I64_load32 (U, memarg), input)
  | '\x36' ->
    let+ memarg, input = read_memarg 32 input in
    (I_store (S32, memarg), input)
  | '\x37' ->
    let+ memarg, input = read_memarg 64 input in
    (I_store (S64, memarg), input)
  | '\x38' ->
    let+ memarg, input = read_memarg 32 input in
    (F_store (S32, memarg), input)
  | '\x39' ->
    let+ memarg, input = read_memarg 64 input in
    (F_store (S64, memarg), input)
  | '\x3A' ->
    let+ memarg, input = read_memarg 32 input in
    (I_store8 (S32, memarg), input)
  | '\x3B' ->
    let+ memarg, input = read_memarg 32 input in
    (I_store16 (S32, memarg), input)
  | '\x3C' ->
    let+ memarg, input = read_memarg 64 input in
    (I_store8 (S64, memarg), input)
  | '\x3D' ->
    let+ memarg, input = read_memarg 64 input in
    (I_store16 (S64, memarg), input)
  | '\x3E' ->
    let+ memarg, input = read_memarg 32 input in
    (I64_store32 memarg, input)
  | '\x3F' ->
    let+ input = check_zero_opcode input in
    (Memory_size, input)
  | '\x40' ->
    let+ input = check_zero_opcode input in
    (Memory_grow, input)
  | '\x41' ->
    let+ i32, input = read_S32 input in
    (I32_const i32, input)
  | '\x42' ->
    let+ i64, input = read_S64 input in
    (I64_const i64, input)
  | '\x43' ->
    let+ f32, input = read_F32 input in
    (F32_const f32, input)
  | '\x44' ->
    let+ f64, input = read_F64 input in
    (F64_const f64, input)
  | '\x45' -> Ok (I_testop (S32, Eqz), input)
  | '\x46' -> Ok (I_relop (S32, Eq), input)
  | '\x47' -> Ok (I_relop (S32, Ne), input)
  | '\x48' -> Ok (I_relop (S32, Lt S), input)
  | '\x49' -> Ok (I_relop (S32, Lt U), input)
  | '\x4A' -> Ok (I_relop (S32, Gt S), input)
  | '\x4B' -> Ok (I_relop (S32, Gt U), input)
  | '\x4C' -> Ok (I_relop (S32, Le S), input)
  | '\x4D' -> Ok (I_relop (S32, Le U), input)
  | '\x4E' -> Ok (I_relop (S32, Ge S), input)
  | '\x4F' -> Ok (I_relop (S32, Ge U), input)
  | '\x50' -> Ok (I_testop (S64, Eqz), input)
  | '\x51' -> Ok (I_relop (S64, Eq), input)
  | '\x52' -> Ok (I_relop (S64, Ne), input)
  | '\x53' -> Ok (I_relop (S64, Lt S), input)
  | '\x54' -> Ok (I_relop (S64, Lt U), input)
  | '\x55' -> Ok (I_relop (S64, Gt S), input)
  | '\x56' -> Ok (I_relop (S64, Gt U), input)
  | '\x57' -> Ok (I_relop (S64, Le S), input)
  | '\x58' -> Ok (I_relop (S64, Le U), input)
  | '\x59' -> Ok (I_relop (S64, Ge S), input)
  | '\x5A' -> Ok (I_relop (S64, Ge U), input)
  | '\x5B' -> Ok (F_relop (S32, Eq), input)
  | '\x5C' -> Ok (F_relop (S32, Ne), input)
  | '\x5D' -> Ok (F_relop (S32, Lt), input)
  | '\x5E' -> Ok (F_relop (S32, Gt), input)
  | '\x5F' -> Ok (F_relop (S32, Le), input)
  | '\x60' -> Ok (F_relop (S32, Ge), input)
  | '\x61' -> Ok (F_relop (S64, Eq), input)
  | '\x62' -> Ok (F_relop (S64, Ne), input)
  | '\x63' -> Ok (F_relop (S64, Lt), input)
  | '\x64' -> Ok (F_relop (S64, Gt), input)
  | '\x65' -> Ok (F_relop (S64, Le), input)
  | '\x66' -> Ok (F_relop (S64, Ge), input)
  | '\x67' -> Ok (I_unop (S32, Clz), input)
  | '\x68' -> Ok (I_unop (S32, Ctz), input)
  | '\x69' -> Ok (I_unop (S32, Popcnt), input)
  | '\x6A' -> Ok (I_binop (S32, Add), input)
  | '\x6B' -> Ok (I_binop (S32, Sub), input)
  | '\x6C' -> Ok (I_binop (S32, Mul), input)
  | '\x6D' -> Ok (I_binop (S32, Div S), input)
  | '\x6E' -> Ok (I_binop (S32, Div U), input)
  | '\x6F' -> Ok (I_binop (S32, Rem S), input)
  | '\x70' -> Ok (I_binop (S32, Rem U), input)
  | '\x71' -> Ok (I_binop (S32, And), input)
  | '\x72' -> Ok (I_binop (S32, Or), input)
  | '\x73' -> Ok (I_binop (S32, Xor), input)
  | '\x74' -> Ok (I_binop (S32, Shl), input)
  | '\x75' -> Ok (I_binop (S32, Shr S), input)
  | '\x76' -> Ok (I_binop (S32, Shr U), input)
  | '\x77' -> Ok (I_binop (S32, Rotl), input)
  | '\x78' -> Ok (I_binop (S32, Rotr), input)
  | '\x79' -> Ok (I_unop (S64, Clz), input)
  | '\x7A' -> Ok (I_unop (S64, Ctz), input)
  | '\x7B' -> Ok (I_unop (S64, Popcnt), input)
  | '\x7C' -> Ok (I_binop (S64, Add), input)
  | '\x7D' -> Ok (I_binop (S64, Sub), input)
  | '\x7E' -> Ok (I_binop (S64, Mul), input)
  | '\x7F' -> Ok (I_binop (S64, Div S), input)
  | '\x80' -> Ok (I_binop (S64, Div U), input)
  | '\x81' -> Ok (I_binop (S64, Rem S), input)
  | '\x82' -> Ok (I_binop (S64, Rem U), input)
  | '\x83' -> Ok (I_binop (S64, And), input)
  | '\x84' -> Ok (I_binop (S64, Or), input)
  | '\x85' -> Ok (I_binop (S64, Xor), input)
  | '\x86' -> Ok (I_binop (S64, Shl), input)
  | '\x87' -> Ok (I_binop (S64, Shr S), input)
  | '\x88' -> Ok (I_binop (S64, Shr U), input)
  | '\x89' -> Ok (I_binop (S64, Rotl), input)
  | '\x8A' -> Ok (I_binop (S64, Rotr), input)
  | '\x8B' -> Ok (F_unop (S32, Abs), input)
  | '\x8C' -> Ok (F_unop (S32, Neg), input)
  | '\x8D' -> Ok (F_unop (S32, Ceil), input)
  | '\x8E' -> Ok (F_unop (S32, Floor), input)
  | '\x8F' -> Ok (F_unop (S32, Trunc), input)
  | '\x90' -> Ok (F_unop (S32, Nearest), input)
  | '\x91' -> Ok (F_unop (S32, Sqrt), input)
  | '\x92' -> Ok (F_binop (S32, Add), input)
  | '\x93' -> Ok (F_binop (S32, Sub), input)
  | '\x94' -> Ok (F_binop (S32, Mul), input)
  | '\x95' -> Ok (F_binop (S32, Div), input)
  | '\x96' -> Ok (F_binop (S32, Min), input)
  | '\x97' -> Ok (F_binop (S32, Max), input)
  | '\x98' -> Ok (F_binop (S32, Copysign), input)
  | '\x99' -> Ok (F_unop (S64, Abs), input)
  | '\x9A' -> Ok (F_unop (S64, Neg), input)
  | '\x9B' -> Ok (F_unop (S64, Ceil), input)
  | '\x9C' -> Ok (F_unop (S64, Floor), input)
  | '\x9D' -> Ok (F_unop (S64, Trunc), input)
  | '\x9E' -> Ok (F_unop (S64, Nearest), input)
  | '\x9F' -> Ok (F_unop (S64, Sqrt), input)
  | '\xA0' -> Ok (F_binop (S64, Add), input)
  | '\xA1' -> Ok (F_binop (S64, Sub), input)
  | '\xA2' -> Ok (F_binop (S64, Mul), input)
  | '\xA3' -> Ok (F_binop (S64, Div), input)
  | '\xA4' -> Ok (F_binop (S64, Min), input)
  | '\xA5' -> Ok (F_binop (S64, Max), input)
  | '\xA6' -> Ok (F_binop (S64, Copysign), input)
  | '\xA7' -> Ok (I32_wrap_i64, input)
  | '\xA8' -> Ok (I_trunc_f (S32, S32, S), input)
  | '\xA9' -> Ok (I_trunc_f (S32, S32, U), input)
  | '\xAA' -> Ok (I_trunc_f (S32, S64, S), input)
  | '\xAB' -> Ok (I_trunc_f (S32, S64, U), input)
  | '\xAC' -> Ok (I64_extend_i32 S, input)
  | '\xAD' -> Ok (I64_extend_i32 U, input)
  | '\xAE' -> Ok (I_trunc_f (S64, S32, S), input)
  | '\xAF' -> Ok (I_trunc_f (S64, S32, U), input)
  | '\xB0' -> Ok (I_trunc_f (S64, S64, S), input)
  | '\xB1' -> Ok (I_trunc_f (S64, S64, U), input)
  | '\xB2' -> Ok (F_convert_i (S32, S32, S), input)
  | '\xB3' -> Ok (F_convert_i (S32, S32, U), input)
  | '\xB4' -> Ok (F_convert_i (S32, S64, S), input)
  | '\xB5' -> Ok (F_convert_i (S32, S64, U), input)
  | '\xB6' -> Ok (F32_demote_f64, input)
  | '\xB7' -> Ok (F_convert_i (S64, S32, S), input)
  | '\xB8' -> Ok (F_convert_i (S64, S32, U), input)
  | '\xB9' -> Ok (F_convert_i (S64, S64, S), input)
  | '\xBA' -> Ok (F_convert_i (S64, S64, U), input)
  | '\xBB' -> Ok (F64_promote_f32, input)
  | '\xBC' -> Ok (I_reinterpret_f (S32, S32), input)
  | '\xBD' -> Ok (I_reinterpret_f (S64, S64), input)
  | '\xBE' -> Ok (F_reinterpret_i (S32, S32), input)
  | '\xBF' -> Ok (F_reinterpret_i (S64, S64), input)
  | '\xC0' -> Ok (I_extend8_s S32, input)
  | '\xC1' -> Ok (I_extend16_s S32, input)
  | '\xC2' -> Ok (I_extend8_s S64, input)
  | '\xC3' -> Ok (I_extend16_s S64, input)
  | '\xC4' -> Ok (I64_extend32_s, input)
  | '\xD0' ->
    let+ (_null, reftype), input = read_reftype input in
    (Ref_null reftype, input)
  | '\xD1' -> Ok (Ref_is_null, input)
  | '\xD2' ->
    let+ funcidx, input = read_indice input in
    (Ref_func funcidx, input)
  | '\xFC' -> read_FC input
  | c -> Fmt.error_msg "illegal opcode (2) %s" (Char.escaped c)

and read_expr types input =
  let rec aux acc input =
    match read_byte ~msg:"read_expr" input with
    | Ok (('\x05' | '\x0B'), _) -> Ok (List.rev acc, input)
    | Error _ -> Ok (List.rev acc, input)
    | Ok _ ->
      let* instr, input = read_instr types input in
      aux (instr :: acc) input
  in
  aux [] input

let read_const types input =
  let* c, input = read_expr types input in
  let+ input = check_end_opcode input in
  (c, input)

type ('a, 'b) import =
  | Func of int
  | Table of limits * 'a ref_type
  | Mem of limits
  | Global of mut * 'b val_type

let magic_check str =
  if String.length str < 4 then Fmt.error_msg "unexpected end"
  else
    let magic = String.sub str 0 4 in
    if String.equal magic "\x00\x61\x73\x6d" then Ok ()
    else Fmt.error_msg "magic header not detected"

let version_check str =
  if String.length str < 8 then Fmt.error_msg "unexpected end"
  else
    let version = String.sub str 4 4 in
    if String.equal version "\x01\x00\x00\x00" then Ok ()
    else Fmt.error_msg "unknown binary version"

let check_section_id = function
  | '\x00' .. '\x0C' -> Ok ()
  | c -> Fmt.error_msg "malformed section id %s" (Char.escaped c)

let section_parse input ~expected_id default section_content_parse =
  match Input.get 0 input with
  | Some id when Char.equal id expected_id ->
    let* () = check_section_id id in
    let* input = Input.sub_suffix 1 input in
    let* () =
      if Input.size input = 0 then Fmt.error_msg "unexpected end" else Ok ()
    in
    let* size, input = read_U32 input in
    let* () =
      if size > Input.size input then Fmt.error_msg "length out of bounds"
      else Ok ()
    in
    let* section_input = Input.sub_prefix size input in
    let* next_input = Input.sub_suffix size input in
    let* res, after_section_input = section_content_parse section_input in
    if Input.size after_section_input > 0 then
      Fmt.error_msg "section size mismatch"
    else Ok (res, next_input)
  | None -> Ok (default, input)
  | Some id ->
    let* () = check_section_id id in
    Ok (default, input)

let parse_utf8_name input =
  let* () =
    if Input.size input = 0 then Fmt.error_msg "unexpected end" else Ok ()
  in
  let* name, input = read_bytes ~msg:"parse_utf8_name" input in
  let name = string_of_char_list name in
  let+ () = Wutf8.check_utf8 name in
  (name, input)

let section_custom input =
  let consume_to_end x input =
    let+ input = Input.sub ~pos:0 ~len:0 input in
    (x, input)
  in
  section_parse input ~expected_id:'\x00' None @@ fun input ->
  let* name, input = parse_utf8_name input in
  let+ (), input = consume_to_end () input in
  (Some name, input)

let read_type _id input =
  let* fcttype, input = read_byte ~msg:"read_type" input in
  let* () =
    match fcttype with
    | '\x60' -> Ok ()
    | _ -> Fmt.error_msg "integer representation too long"
  in
  let* params, input = read_valtypes input in
  let+ results, input = read_valtypes input in
  let params = List.map (fun param -> (None, param)) params in
  ([ (None, (Final, [], Def_func_t (params, results))) ], input)

let read_global_type input =
  let* val_type, input = read_valtype input in
  let+ mut, input = read_mut input in
  ((mut, val_type), input)

let read_import input =
  let* modul, input = parse_utf8_name input in
  let* name, input = parse_utf8_name input in
  let* import_typeidx, input = read_byte ~msg:"read_import" input in
  match import_typeidx with
  | '\x00' ->
    let+ typeidx, input = read_U32 input in
    ((modul, name, Func typeidx), input)
  | '\x01' ->
    let* ref_type, input = read_reftype input in
    let+ limits, input = read_limits input in
    ((modul, name, Table (limits, ref_type)), input)
  | '\x02' ->
    let+ limits, input = read_limits input in
    ((modul, name, Mem limits), input)
  | '\x03' ->
    let+ (mut, val_type), input = read_global_type input in
    ((modul, name, Global (mut, val_type)), input)
  | _c -> Fmt.error_msg "malformed import kind"

let read_table input =
  let* ref_type, input = read_reftype input in
  let+ limits, input = read_limits input in
  ((limits, ref_type), input)

let read_memory input =
  let+ limits, input = read_limits input in
  ((None, limits), input)

let read_global types input =
  let* typ, input = read_global_type input in
  let+ init, input = read_const types input in
  ({ typ; init; id = None }, input)

let read_export input =
  let* name, input = read_bytes ~msg:"read_export 1" input in
  let name = string_of_char_list name in
  let* export_typeidx, input = read_byte ~msg:"read_export 2" input in
  let+ id, input = read_U32 input in
  ((export_typeidx, { id; name }), input)

let read_elem_active types input =
  let* Raw index, input = read_indice input in
  let+ offset, input = read_const types input in
  (Elem_active (Some index, offset), input)

let read_elem_active_zero types input =
  let+ offset, input = read_const types input in
  (Elem_active (Some 0, offset), input)

let read_elem_index input =
  let+ index, input = read_indice input in
  ([ Ref_func index ], input)

let read_elem_kind input =
  let msg = "malformed element kind" in
  match read_byte ~msg input with
  | Ok ('\x00', input) -> Ok ((Null, Func_ht), input)
  | Ok (c, _input) ->
    Fmt.error_msg "%s (expected 0x00 but got %s)" msg (Char.escaped c)
  | Error _ as e -> e

let read_element types input =
  let* i, input = read_U32 input in
  let id = None in
  match i with
  | 0 ->
    let* mode, input = read_elem_active_zero types input in
    let+ init, input = vector_no_id read_elem_index input in
    let typ = (Null, Func_ht) in
    ({ id; typ; init; mode }, input)
  | 1 ->
    let mode = Elem_passive in
    let* typ, input = read_elem_kind input in
    let+ init, input = vector_no_id read_elem_index input in
    ({ id; typ; init; mode }, input)
  | 2 ->
    let* mode, input = read_elem_active types input in
    let* typ, input = read_elem_kind input in
    let+ init, input = vector_no_id read_elem_index input in
    ({ id; typ; init; mode }, input)
  | 3 ->
    let mode = Elem_declarative in
    let* typ, input = read_elem_kind input in
    let+ init, input = vector_no_id read_elem_index input in
    ({ id; typ; init; mode }, input)
  | 4 ->
    let* mode, input = read_elem_active_zero types input in
    let+ init, input = vector_no_id (read_const types) input in
    let typ = (Null, Func_ht) in
    ({ id; typ; init; mode }, input)
  | 5 ->
    let mode = Elem_passive in
    let* typ, input = read_reftype input in
    let+ init, input = vector_no_id (read_const types) input in
    ({ id; typ; init; mode }, input)
  | 6 ->
    let* mode, input = read_elem_active types input in
    let* typ, input = read_reftype input in
    let+ init, input = vector_no_id (read_const types) input in
    ({ id; typ; init; mode }, input)
  | 7 ->
    let mode = Elem_declarative in
    let* typ, input = read_reftype input in
    let+ init, input = vector_no_id (read_const types) input in
    ({ id; typ; init; mode }, input)
  | i -> Fmt.error_msg "malformed elements segment kind: %d" i

let read_local input =
  let* n, input = read_U32 input in
  let+ t, input = read_valtype input in
  ((n, t), input)

let read_locals input =
  let* nts, input = vector_no_id read_local input in
  let ns =
    List.map (fun (n, _t) -> Convert.Int64.extend_i32_u @@ Int32.of_int n) nts
  in
  let+ () =
    if not @@ Int64.lt_u (List.fold_left Int64.add 0L ns) 0x1_0000_0000L then
      Fmt.error_msg "too many locals"
    else Ok ()
  in
  let locals = List.map (fun (n, t) -> List.init n (fun _i -> (None, t))) nts in
  let locals = List.flatten locals in
  (locals, input)

let read_code types input =
  let* size, input = read_U32 input in
  let* code_input = Input.sub_prefix size input in
  let* next_input = Input.sub_suffix size input in
  let* locals, code_input = read_locals code_input in
  let* code, code_input = read_expr types code_input in
  let* () =
    if Input.size code_input = 0 && Input.size next_input = 0 then
      Fmt.error_msg "unexpected end of section or function"
    else Ok ()
  in
  let* code_input =
    check_end_opcode ~unexpected_eoi_msg:"unexpected end of section or function"
      code_input
  in
  if Input.size code_input > 0 then
    Fmt.error_msg "unexpected end of section or function"
  else Ok ((locals, code), next_input)

(* TODO: merge Elem and Data modes ? *)
let read_data_active types input =
  let* Raw index, input = read_indice input in
  let+ offset, input = read_const types input in
  (Data_active (index, offset), input)

let read_data_active_zero types input =
  let+ offset, input = read_const types input in
  (Data_active (0, offset), input)

let read_data types input =
  let* i, input = read_U32 input in
  let id = None in
  match i with
  | 0 ->
    let* mode, input = read_data_active_zero types input in
    let+ init, input = read_bytes ~msg:"read_data 0" input in
    let init = string_of_char_list init in
    ({ id; init; mode }, input)
  | 1 ->
    let mode = Data_passive in
    let+ init, input = read_bytes ~msg:"read_data 1" input in
    let init = string_of_char_list init in
    ({ id; init; mode }, input)
  | 2 ->
    let* mode, input = read_data_active types input in
    let+ init, input = read_bytes ~msg:"read_data 2" input in
    let init = string_of_char_list init in
    ({ id; init; mode }, input)
  | i -> Fmt.error_msg "malformed data segment kind %d" i

let parse_many_custom_section input =
  let rec aux acc input =
    let* custom_section, input = section_custom input in
    match custom_section with
    | None -> Ok (List.rev acc, input)
    | Some _ as custom_section -> aux (custom_section :: acc) input
  in
  aux [] input

let sections_iterate (input : Input.t) =
  (* Custom *)
  let* custom_sections, input = parse_many_custom_section input in

  (* Type *)
  let* types, input =
    section_parse input ~expected_id:'\x01' [] (vector read_type)
  in
  let types = Array.of_list types in

  (* Custom *)
  let* custom_sections', input = parse_many_custom_section input in
  let custom_sections = custom_sections @ custom_sections' in

  (* Imports *)
  let* import_section, input =
    section_parse input ~expected_id:'\x02' [] (vector_no_id read_import)
  in

  (* Custom *)
  let* custom_sections', input = parse_many_custom_section input in
  let custom_sections = custom_sections @ custom_sections' in

  (* Function *)
  let* function_section, input =
    section_parse input ~expected_id:'\x03' [] (vector_no_id read_U32)
  in

  (* Custom *)
  let* custom_sections', input = parse_many_custom_section input in
  let custom_sections = custom_sections @ custom_sections' in

  (* Tables *)
  let* table_section, input =
    section_parse input ~expected_id:'\x04' [] (vector_no_id read_table)
  in

  (* Custom *)
  let* custom_sections', input = parse_many_custom_section input in
  let custom_sections = custom_sections @ custom_sections' in

  (* Memory *)
  let* memory_section, input =
    section_parse input ~expected_id:'\x05' [] (vector_no_id read_memory)
  in

  (* Custom *)
  let* custom_sections', input = parse_many_custom_section input in
  let custom_sections = custom_sections @ custom_sections' in

  (* Globals *)
  let* global_section, input =
    section_parse input ~expected_id:'\x06' []
      (vector_no_id (read_global types))
  in

  (* Custom *)
  let* custom_sections', input = parse_many_custom_section input in
  let custom_sections = custom_sections @ custom_sections' in

  (* Exports *)
  let* export_section, input =
    section_parse input ~expected_id:'\x07' [] (vector_no_id read_export)
  in

  (* Custom *)
  let* custom_sections', input = parse_many_custom_section input in
  let custom_sections = custom_sections @ custom_sections' in

  (* Start *)
  let* start_section, input =
    section_parse input ~expected_id:'\x08' None @@ fun input ->
    let+ idx_start_func, input = read_U32 input in
    (Some idx_start_func, input)
  in

  (* Custom *)
  let* custom_sections', input = parse_many_custom_section input in
  let custom_sections = custom_sections @ custom_sections' in

  (* Elements *)
  let* element_section, input =
    section_parse input ~expected_id:'\x09' []
    @@ vector_no_id (read_element types)
  in

  (* Custom *)
  let* custom_sections', input = parse_many_custom_section input in
  let custom_sections = custom_sections @ custom_sections' in

  (* Data_count *)
  let* data_count_section, input =
    section_parse input ~expected_id:'\x0C' None @@ fun input ->
    let+ i, input = read_U32 input in
    (Some i, input)
  in

  (* Custom *)
  let* custom_sections', input = parse_many_custom_section input in
  let custom_sections = custom_sections @ custom_sections' in

  (* Code *)
  let* code_section, input =
    section_parse input ~expected_id:'\x0A' [] (vector_no_id (read_code types))
  in

  let* () =
    if List.compare_lengths function_section code_section <> 0 then
      Fmt.error_msg "function and code section have inconsistent lengths"
    else Ok ()
  in

  (* Custom *)
  let* custom_sections', input = parse_many_custom_section input in
  let custom_sections = custom_sections @ custom_sections' in

  (* Data *)
  let+ data, input =
    section_parse input ~expected_id:'\x0B' [] (vector_no_id (read_data types))
  in

  let data = Array.of_list data in

  let* () =
    match (Array.length data, data_count_section) with
    | 0, None -> Ok ()
    | _data_len, None ->
      let code_use_dataidx = ref false in
      let f_iter = function
        | Data_drop _ | Memory_init _ -> code_use_dataidx := true
        | _ -> ()
      in
      let expr = List.concat_map snd code_section in
      iter_expr f_iter expr;
      if !code_use_dataidx then Fmt.error_msg "data count section required"
      else Ok ()
    | data_len, Some data_count when data_len = data_count -> Ok ()
    | _ -> Fmt.error_msg "data count and data section have inconsistent lengths"
  in

  (* Custom *)
  let* custom_sections', input = parse_many_custom_section input in
  let custom_sections = custom_sections @ custom_sections' in

  let+ () =
    if not @@ Input.is_empty input then Fmt.error_msg "malformed section id"
    else Ok ()
  in

  (* Memories *)
  let mem =
    let local = List.map (fun mem -> Runtime.Local mem) memory_section in
    let imported =
      List.filter_map
        (function
          | modul, name, Mem desc ->
            Option.some
            @@ Runtime.Imported { modul; name; assigned_name = None; desc }
          | _not_a_memory_import -> None )
        import_section
    in
    Array.of_list (imported @ local)
  in

  (* Globals *)
  let global =
    let local = List.map (fun g -> Runtime.Local g) global_section in
    let imported =
      List.filter_map
        (function
          | modul, name, Global (mut, val_type) ->
            Option.some
            @@ Runtime.Imported
                 { modul; name; assigned_name = None; desc = (mut, val_type) }
          | _not_a_global_import -> None )
        import_section
    in
    Array.of_list (imported @ local)
  in

  (* Functions *)
  let func =
    let local =
      List.map2
        (fun typeidx (locals, body) ->
          Runtime.Local
            { type_f = block_type_of_rec_type types.(typeidx)
            ; locals
            ; body
            ; id = None
            } )
        function_section code_section
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
                 ; desc = block_type_of_rec_type types.(typeidx)
                 }
          | _not_a_function_import -> None )
        import_section
    in
    Array.of_list (imported @ local)
  in

  (* Tables *)
  let table =
    let local = List.map (fun tbl -> Runtime.Local (None, tbl)) table_section in
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
        import_section
    in
    Array.of_list (imported @ local)
  in

  (* Elems *)
  let elem = Array.of_list element_section in

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
        | _ -> Fmt.failwith "read_exportdesc error" )
      empty_exports export_section
  in
  let exports =
    { func = List.rev exports.func
    ; table = List.rev exports.table
    ; mem = List.rev exports.mem
    ; global = List.rev exports.global
    }
  in

  (* Custom *)
  let custom =
    List.filter_map (Option.map (fun x -> Uninterpreted x)) custom_sections
  in

  { Binary.Module.id = None
  ; types
  ; global
  ; mem
  ; elem
  ; func
  ; table
  ; start = start_section
  ; data
  ; exports
  ; custom
  }

let from_string content =
  let* () = magic_check content in
  let* () = version_check content in
  let* input = Input.of_string content |> Input.sub_suffix 8 in
  let* m = sections_iterate input in
  m

let from_channel chan =
  let content = In_channel.input_all chan in
  from_string content

let from_file (filename : Fpath.t) =
  let* res =
    Bos.OS.File.with_ic filename (fun chan () -> from_channel chan) ()
  in
  res
