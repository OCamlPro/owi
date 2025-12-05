(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Binary
open Syntax

(* add byte from int (ascii code) *)
let write_byte buf i =
  let c = Char.chr (i land 0xff) in
  Buffer.add_char buf c

(* add 2 bytes (16 bits) from int *)
let write_bytes_2 buf i =
  write_byte buf (i land 0xff);
  write_byte buf (i lsr 8)

(* add 4 bytes (32 bits) from int32 *)
let write_bytes_4 buf i =
  write_bytes_2 buf (Int32.to_int (Int32.logand i 0xffffl));
  write_bytes_2 buf (Int32.to_int (Int32.shift_right_logical i 16))

(* add 8 bytes (64 bits) from int64 *)
let write_bytes_8 buf i =
  write_bytes_4 buf (Int64.to_int32 (Int64.logand i 0xffffffffL));
  write_bytes_4 buf (Int64.to_int32 (Int64.shift_right i 32))

let rec write_u64 buf i =
  let b = Int64.to_int (Int64.logand i 0x7fL) in
  if Int64.le 0L i && Int64.lt i 128L then write_byte buf b
  else begin
    write_byte buf (b lor 0x80);
    write_u64 buf (Int64.shift_right_logical i 7)
  end

let write_u32 buf i =
  write_u64 buf (Int64.logand (Int64.of_int32 i) 0xffffffffL)

let write_u32_of_int buf i =
  let i = Int32.of_int i in
  write_u32 buf i

let write_string buf str =
  let len = String.length str in
  write_u32_of_int buf len;
  Buffer.add_string buf str

let rec write_s64 buf i =
  let b = Int64.to_int (Int64.logand i 0x7fL) in
  if Int64.le (-64L) i && Int64.lt i 64L then write_byte buf b
  else begin
    write_byte buf (b lor 0x80);
    write_s64 buf (Int64.shift_right i 7)
  end

let write_s32 buf i = write_s64 buf (Int64.of_int32 i)

let write_f32 buf f =
  let i32 = Float32.to_bits f in
  write_bytes_4 buf i32

let write_f64 buf f =
  let i64 = Float64.to_bits f in
  write_bytes_8 buf i64

let write_indice buf (idx : Binary.indice) = write_u32_of_int buf idx

let write_char_indice buf c idx =
  Buffer.add_char buf c;
  write_indice buf idx

let write_reftype buf ht =
  match ht with
  | Text.Func_ht -> Buffer.add_char buf '\x70'
  | Extern_ht -> Buffer.add_char buf '\x6F'

let get_char_valtype = function
  | Text.Num_type I32 -> '\x7F'
  | Num_type I64 -> '\x7E'
  | Num_type F32 -> '\x7D'
  | Num_type F64 -> '\x7C'
  | Num_type V128 -> '\x7B'
  | Ref_type (Null, Func_ht) -> '\x70'
  | Ref_type (Null, Extern_ht) -> '\x6F'
  | _ -> assert false

let write_valtype buf vt =
  let c = get_char_valtype vt in
  Buffer.add_char buf c

let encode_vector length iter buf datas encode_func =
  let vector_buf = Buffer.create 16 in
  let len = length datas in
  iter (encode_func vector_buf) datas;
  write_u32_of_int buf len;
  Buffer.add_buffer buf vector_buf

let encode_vector_list buf datas encode_func =
  encode_vector List.length List.iter buf datas encode_func

let encode_vector_array buf datas encode_func =
  encode_vector Array.length Array.iter buf datas encode_func

let write_resulttype buf (rt : Text.result_type) =
  encode_vector_list buf rt write_valtype

let write_paramtype buf (pt : Text.param_type) =
  let vt = List.map snd pt in
  write_resulttype buf vt

let write_mut buf (mut : Text.mut) =
  let c = match mut with Const -> '\x00' | Var -> '\x01' in
  Buffer.add_char buf c

let write_block_type buf (typ : Binary.block_type option) =
  match typ with
  | None | Some (Bt_raw (None, ([], []))) -> Buffer.add_char buf '\x40'
  | Some (Bt_raw (None, ([], [ vt ]))) -> write_valtype buf vt
  | Some (Bt_raw (Some idx, _)) -> write_indice buf idx
  (* TODO: memo
     will this pattern matching be enough with the use of the new modul.types field?
  *)
  | _ -> assert false (* TODO: same, new pattern matching cases ? *)

let write_block_type_idx buf (typ : Binary.block_type) =
  match typ with
  | Bt_raw (None, _) -> assert false
  | Bt_raw (Some idx, _) -> write_indice buf idx

let write_global_type buf ((mut, vt) : Text.Global.Type.t) =
  write_valtype buf vt;
  write_mut buf mut

let write_limits buf (limits : Text.limits) =
  match limits with
  | { min; max = None } ->
    Buffer.add_char buf '\x00';
    write_u32_of_int buf min
  | { min; max = Some max } ->
    Buffer.add_char buf '\x01';
    write_u32_of_int buf min;
    write_u32_of_int buf max

let write_memarg buf idx ({ offset; align } : Text.memarg) =
  if idx = 0 then write_u32 buf align
  else (
    (* Set the 6th bit if the id not 0 *)
    write_u32 buf (Int32.logor align 0x40l);
    write_indice buf idx );
  write_u32 buf offset

let write_memory buf ((_so, limits) : Text.Mem.t) = write_limits buf limits

let write_memory_import buf
  ({ modul_name; name; typ = limits; _ } : Text.limits Origin.imported) =
  write_string buf modul_name;
  write_string buf name;
  Buffer.add_char buf '\x02';
  write_limits buf limits

let write_table buf ((_so, (limits, (_nullable, heaptype))) : Text.Table.t) =
  write_reftype buf heaptype;
  write_limits buf limits

let write_table_import buf
  ({ modul_name; name; typ = limits, (_nullable, heaptype); _ } :
    Text.Table.Type.t Origin.imported ) =
  write_string buf modul_name;
  write_string buf name;
  Buffer.add_char buf '\x01';
  write_reftype buf heaptype;
  write_limits buf limits

let write_func_import buf
  ({ modul_name; name; typ; _ } : Binary.block_type Origin.imported) =
  write_string buf modul_name;
  write_string buf name;
  Buffer.add_char buf '\x00';
  write_block_type_idx buf typ

let write_fc buf i =
  Buffer.add_char buf '\xFC';
  write_u32_of_int buf i

let write_fd buf i =
  Buffer.add_char buf '\xFD';
  write_u32_of_int buf i

let rec write_instr buf instr =
  let add_char c = Buffer.add_char buf c in
  match instr.Annotated.raw with
  | Unreachable -> add_char '\x00'
  | Nop -> add_char '\x01'
  | Block (_str, bt, expr) ->
    add_char '\x02';
    write_block_type buf bt;
    write_expr buf expr ~end_op_code:None
  | Loop (_str, bt, expr) ->
    add_char '\x03';
    write_block_type buf bt;
    write_expr buf expr ~end_op_code:None
  | If_else (_str, bt, expr1, expr2) ->
    add_char '\x04';
    write_block_type buf bt;
    begin match expr2.raw with
    | [] -> write_expr buf expr1 ~end_op_code:None
    | _ ->
      write_expr buf expr1 ~end_op_code:(Some '\x05');
      write_expr buf expr2 ~end_op_code:None
    end
  | Br idx -> write_char_indice buf '\x0C' idx
  | Br_if idx -> write_char_indice buf '\x0D' idx
  | Br_table (idxs, idx) ->
    add_char '\x0E';
    encode_vector_array buf idxs write_indice;
    write_indice buf idx
  | Return -> add_char '\x0F'
  | Call idx -> write_char_indice buf '\x10' idx
  | Call_indirect (idx, bt) ->
    add_char '\x11';
    write_block_type_idx buf bt;
    write_indice buf idx
  | Drop -> add_char '\x1A'
  | Select None -> add_char '\x1B'
  | Select (Some vts) ->
    add_char '\x1C';
    List.iter (write_valtype buf) vts
  | Local_get idx -> write_char_indice buf '\x20' idx
  | Local_set idx -> write_char_indice buf '\x21' idx
  | Local_tee idx -> write_char_indice buf '\x22' idx
  | Global_get idx -> write_char_indice buf '\x23' idx
  | Global_set idx -> write_char_indice buf '\x24' idx
  | Table_get idx -> write_char_indice buf '\x25' idx
  | Table_set idx -> write_char_indice buf '\x26' idx
  | I_load (idx, S32, memarg) ->
    add_char '\x28';
    write_memarg buf idx memarg
  | I_load (idx, S64, memarg) ->
    add_char '\x29';
    write_memarg buf idx memarg
  | F_load (idx, S32, memarg) ->
    add_char '\x2A';
    write_memarg buf idx memarg
  | F_load (idx, S64, memarg) ->
    add_char '\x2B';
    write_memarg buf idx memarg
  | I_load8 (idx, S32, S, memarg) ->
    add_char '\x2C';
    write_memarg buf idx memarg
  | I_load8 (idx, S32, U, memarg) ->
    add_char '\x2D';
    write_memarg buf idx memarg
  | I_load16 (idx, S32, S, memarg) ->
    add_char '\x2E';
    write_memarg buf idx memarg
  | I_load16 (idx, S32, U, memarg) ->
    add_char '\x2F';
    write_memarg buf idx memarg
  | I_load8 (idx, S64, S, memarg) ->
    add_char '\x30';
    write_memarg buf idx memarg
  | I_load8 (idx, S64, U, memarg) ->
    add_char '\x31';
    write_memarg buf idx memarg
  | I_load16 (idx, S64, S, memarg) ->
    add_char '\x32';
    write_memarg buf idx memarg
  | I_load16 (idx, S64, U, memarg) ->
    add_char '\x33';
    write_memarg buf idx memarg
  | I64_load32 (idx, S, memarg) ->
    add_char '\x34';
    write_memarg buf idx memarg
  | I64_load32 (idx, U, memarg) ->
    add_char '\x35';
    write_memarg buf idx memarg
  | I_store (idx, S32, memarg) ->
    add_char '\x36';
    write_memarg buf idx memarg
  | I_store (idx, S64, memarg) ->
    add_char '\x37';
    write_memarg buf idx memarg
  | F_store (idx, S32, memarg) ->
    add_char '\x38';
    write_memarg buf idx memarg
  | F_store (idx, S64, memarg) ->
    add_char '\x39';
    write_memarg buf idx memarg
  | I_store8 (idx, S32, memarg) ->
    add_char '\x3A';
    write_memarg buf idx memarg
  | I_store16 (idx, S32, memarg) ->
    add_char '\x3B';
    write_memarg buf idx memarg
  | I_store8 (idx, S64, memarg) ->
    add_char '\x3C';
    write_memarg buf idx memarg
  | I_store16 (idx, S64, memarg) ->
    add_char '\x3D';
    write_memarg buf idx memarg
  | I64_store32 (idx, memarg) ->
    add_char '\x3E';
    write_memarg buf idx memarg
  | Memory_size idx -> write_char_indice buf '\x3F' idx
  | Memory_grow idx -> write_char_indice buf '\x40' idx
  | I32_const i ->
    add_char '\x41';
    write_s32 buf i
  | I64_const i ->
    add_char '\x42';
    write_s64 buf i
  | F32_const f ->
    add_char '\x43';
    write_f32 buf f
  | F64_const f ->
    add_char '\x44';
    write_f64 buf f
  | I_testop (S32, Eqz) -> add_char '\x45'
  | I_relop (S32, Eq) -> add_char '\x46'
  | I_relop (S32, Ne) -> add_char '\x47'
  | I_relop (S32, Lt S) -> add_char '\x48'
  | I_relop (S32, Lt U) -> add_char '\x49'
  | I_relop (S32, Gt S) -> add_char '\x4A'
  | I_relop (S32, Gt U) -> add_char '\x4B'
  | I_relop (S32, Le S) -> add_char '\x4C'
  | I_relop (S32, Le U) -> add_char '\x4D'
  | I_relop (S32, Ge S) -> add_char '\x4E'
  | I_relop (S32, Ge U) -> add_char '\x4F'
  | I_testop (S64, Eqz) -> add_char '\x50'
  | I_relop (S64, Eq) -> add_char '\x51'
  | I_relop (S64, Ne) -> add_char '\x52'
  | I_relop (S64, Lt S) -> add_char '\x53'
  | I_relop (S64, Lt U) -> add_char '\x54'
  | I_relop (S64, Gt S) -> add_char '\x55'
  | I_relop (S64, Gt U) -> add_char '\x56'
  | I_relop (S64, Le S) -> add_char '\x57'
  | I_relop (S64, Le U) -> add_char '\x58'
  | I_relop (S64, Ge S) -> add_char '\x59'
  | I_relop (S64, Ge U) -> add_char '\x5A'
  | F_relop (S32, Eq) -> add_char '\x5B'
  | F_relop (S32, Ne) -> add_char '\x5C'
  | F_relop (S32, Lt) -> add_char '\x5D'
  | F_relop (S32, Gt) -> add_char '\x5E'
  | F_relop (S32, Le) -> add_char '\x5F'
  | F_relop (S32, Ge) -> add_char '\x60'
  | F_relop (S64, Eq) -> add_char '\x61'
  | F_relop (S64, Ne) -> add_char '\x62'
  | F_relop (S64, Lt) -> add_char '\x63'
  | F_relop (S64, Gt) -> add_char '\x64'
  | F_relop (S64, Le) -> add_char '\x65'
  | F_relop (S64, Ge) -> add_char '\x66'
  | I_unop (S32, Clz) -> add_char '\x67'
  | I_unop (S32, Ctz) -> add_char '\x68'
  | I_unop (S32, Popcnt) -> add_char '\x69'
  | I_binop (S32, Add) -> add_char '\x6A'
  | I_binop (S32, Sub) -> add_char '\x6B'
  | I_binop (S32, Mul) -> add_char '\x6C'
  | I_binop (S32, Div S) -> add_char '\x6D'
  | I_binop (S32, Div U) -> add_char '\x6E'
  | I_binop (S32, Rem S) -> add_char '\x6F'
  | I_binop (S32, Rem U) -> add_char '\x70'
  | I_binop (S32, And) -> add_char '\x71'
  | I_binop (S32, Or) -> add_char '\x72'
  | I_binop (S32, Xor) -> add_char '\x73'
  | I_binop (S32, Shl) -> add_char '\x74'
  | I_binop (S32, Shr S) -> add_char '\x75'
  | I_binop (S32, Shr U) -> add_char '\x76'
  | I_binop (S32, Rotl) -> add_char '\x77'
  | I_binop (S32, Rotr) -> add_char '\x78'
  | I_unop (S64, Clz) -> add_char '\x79'
  | I_unop (S64, Ctz) -> add_char '\x7A'
  | I_unop (S64, Popcnt) -> add_char '\x7B'
  | I_binop (S64, Add) -> add_char '\x7C'
  | I_binop (S64, Sub) -> add_char '\x7D'
  | I_binop (S64, Mul) -> add_char '\x7E'
  | I_binop (S64, Div S) -> add_char '\x7F'
  | I_binop (S64, Div U) -> add_char '\x80'
  | I_binop (S64, Rem S) -> add_char '\x81'
  | I_binop (S64, Rem U) -> add_char '\x82'
  | I_binop (S64, And) -> add_char '\x83'
  | I_binop (S64, Or) -> add_char '\x84'
  | I_binop (S64, Xor) -> add_char '\x85'
  | I_binop (S64, Shl) -> add_char '\x86'
  | I_binop (S64, Shr S) -> add_char '\x87'
  | I_binop (S64, Shr U) -> add_char '\x88'
  | I_binop (S64, Rotl) -> add_char '\x89'
  | I_binop (S64, Rotr) -> add_char '\x8A'
  | F_unop (S32, Abs) -> add_char '\x8B'
  | F_unop (S32, Neg) -> add_char '\x8C'
  | F_unop (S32, Ceil) -> add_char '\x8D'
  | F_unop (S32, Floor) -> add_char '\x8E'
  | F_unop (S32, Trunc) -> add_char '\x8F'
  | F_unop (S32, Nearest) -> add_char '\x90'
  | F_unop (S32, Sqrt) -> add_char '\x91'
  | F_binop (S32, Add) -> add_char '\x92'
  | F_binop (S32, Sub) -> add_char '\x93'
  | F_binop (S32, Mul) -> add_char '\x94'
  | F_binop (S32, Div) -> add_char '\x95'
  | F_binop (S32, Min) -> add_char '\x96'
  | F_binop (S32, Max) -> add_char '\x97'
  | F_binop (S32, Copysign) -> add_char '\x98'
  | F_unop (S64, Abs) -> add_char '\x99'
  | F_unop (S64, Neg) -> add_char '\x9A'
  | F_unop (S64, Ceil) -> add_char '\x9B'
  | F_unop (S64, Floor) -> add_char '\x9C'
  | F_unop (S64, Trunc) -> add_char '\x9D'
  | F_unop (S64, Nearest) -> add_char '\x9E'
  | F_unop (S64, Sqrt) -> add_char '\x9F'
  | F_binop (S64, Add) -> add_char '\xA0'
  | F_binop (S64, Sub) -> add_char '\xA1'
  | F_binop (S64, Mul) -> add_char '\xA2'
  | F_binop (S64, Div) -> add_char '\xA3'
  | F_binop (S64, Min) -> add_char '\xA4'
  | F_binop (S64, Max) -> add_char '\xA5'
  | F_binop (S64, Copysign) -> add_char '\xA6'
  | I32_wrap_i64 -> add_char '\xA7'
  | I_trunc_f (S32, S32, S) -> add_char '\xA8'
  | I_trunc_f (S32, S32, U) -> add_char '\xA9'
  | I_trunc_f (S32, S64, S) -> add_char '\xAA'
  | I_trunc_f (S32, S64, U) -> add_char '\xAB'
  | I64_extend_i32 S -> add_char '\xAC'
  | I64_extend_i32 U -> add_char '\xAD'
  | I_trunc_f (S64, S32, S) -> add_char '\xAE'
  | I_trunc_f (S64, S32, U) -> add_char '\xAF'
  | I_trunc_f (S64, S64, S) -> add_char '\xB0'
  | I_trunc_f (S64, S64, U) -> add_char '\xB1'
  | F_convert_i (S32, S32, S) -> add_char '\xB2'
  | F_convert_i (S32, S32, U) -> add_char '\xB3'
  | F_convert_i (S32, S64, S) -> add_char '\xB4'
  | F_convert_i (S32, S64, U) -> add_char '\xB5'
  | F32_demote_f64 -> add_char '\xB6'
  | F_convert_i (S64, S32, S) -> add_char '\xB7'
  | F_convert_i (S64, S32, U) -> add_char '\xB8'
  | F_convert_i (S64, S64, S) -> add_char '\xB9'
  | F_convert_i (S64, S64, U) -> add_char '\xBA'
  | F64_promote_f32 -> add_char '\xBB'
  | I_reinterpret_f (S32, S32) -> add_char '\xBC'
  | I_reinterpret_f (S64, S64) -> add_char '\xBD'
  | F_reinterpret_i (S32, S32) -> add_char '\xBE'
  | F_reinterpret_i (S64, S64) -> add_char '\xBF'
  | I_extend8_s S32 -> add_char '\xC0'
  | I_extend16_s S32 -> add_char '\xC1'
  | I_extend8_s S64 -> add_char '\xC2'
  | I_extend16_s S64 -> add_char '\xC3'
  | I64_extend32_s -> add_char '\xC4'
  | Ref_null rt ->
    add_char '\xD0';
    write_reftype buf rt
  | Ref_is_null -> add_char '\xD1'
  | Ref_func idx -> write_char_indice buf '\xD2' idx
  | I_trunc_sat_f (S32, S32, S) -> write_fc buf 0
  | I_trunc_sat_f (S32, S32, U) -> write_fc buf 1
  | I_trunc_sat_f (S32, S64, S) -> write_fc buf 2
  | I_trunc_sat_f (S32, S64, U) -> write_fc buf 3
  | I_trunc_sat_f (S64, S32, S) -> write_fc buf 4
  | I_trunc_sat_f (S64, S32, U) -> write_fc buf 5
  | I_trunc_sat_f (S64, S64, S) -> write_fc buf 6
  | I_trunc_sat_f (S64, S64, U) -> write_fc buf 7
  | Memory_init (memidx, dataidx) ->
    write_fc buf 8;
    write_indice buf dataidx;
    write_indice buf memidx
  | Data_drop idx ->
    write_fc buf 9;
    write_indice buf idx
  | Memory_copy (id1, id2) ->
    write_fc buf 10;
    write_indice buf id1;
    write_indice buf id2
  | Memory_fill idx ->
    write_fc buf 11;
    write_indice buf idx
  | Table_init (tableidx, elemidx) ->
    write_fc buf 12;
    write_indice buf elemidx;
    write_indice buf tableidx
  | Elem_drop idx ->
    write_fc buf 13;
    write_indice buf idx
  | Table_copy (idx1, idx2) ->
    write_fc buf 14;
    write_indice buf idx1;
    write_indice buf idx2
  | Table_grow idx ->
    write_fc buf 15;
    write_indice buf idx
  | Table_size idx ->
    write_fc buf 16;
    write_indice buf idx
  | Table_fill idx ->
    write_fc buf 17;
    write_indice buf idx
  | V128_const v ->
    write_fd buf 12;
    let a, b = Concrete_v128.to_i64x2 v in
    write_bytes_8 buf a;
    write_bytes_8 buf b
  | V_ibinop (I8x16, Add) -> write_fd buf 110
  | V_ibinop (I8x16, Sub) -> write_fd buf 113
  | V_ibinop (I16x8, Add) -> write_fd buf 142
  | V_ibinop (I16x8, Sub) -> write_fd buf 145
  | V_ibinop (I32x4, Add) -> write_fd buf 174
  | V_ibinop (I32x4, Sub) -> write_fd buf 177
  | V_ibinop (I64x2, Add) -> write_fd buf 206
  | V_ibinop (I64x2, Sub) -> write_fd buf 209
  | I_reinterpret_f _ | F_reinterpret_i _ | Return_call _
  | Return_call_indirect _ | Return_call_ref _ | Call_ref _ | Extern_externalize
  | Extern_internalize ->
    assert false

and write_expr buf expr ~end_op_code =
  List.iter (write_instr buf) expr.Annotated.raw;
  let end_op_code = Option.value end_op_code ~default:'\x0B' in
  Buffer.add_char buf end_op_code

let write_export buf cid ({ name; id } : Binary.Export.t) =
  write_string buf name;
  Buffer.add_char buf cid;
  write_u32_of_int buf id

let write_global buf ({ typ; init; _ } : Global.t) =
  write_global_type buf typ;
  write_expr buf init ~end_op_code:None

let write_global_import buf
  ({ modul_name; name; typ = mut, valtype; _ } :
    Text.Global.Type.t Origin.imported ) =
  write_string buf modul_name;
  write_string buf name;
  Buffer.add_char buf '\x03';
  write_valtype buf valtype;
  write_mut buf mut

let write_locals buf locals =
  let compressed =
    List.rev
    @@ List.fold_left
         (fun compressed (_so, local_type) ->
           let c = get_char_valtype local_type in
           match compressed with
           | (ch, cnt) :: compressed when Char.equal ch c ->
             (c, cnt + 1) :: compressed
           | compressed -> (c, 1) :: compressed )
         [] locals
  in
  let len = List.length compressed in
  write_u32_of_int buf len;
  List.iter
    (fun (char, count) ->
      write_u32_of_int buf count;
      Buffer.add_char buf char )
    compressed

let write_element buf ({ typ = _, ht; init; mode; _ } : Elem.t) =
  let write_init buf init =
    let is_ref_func = ref true in
    encode_vector_list buf init (fun buf expr ->
      match expr.Annotated.raw with
      | [ { Annotated.raw = Ref_func idx; _ } ] -> write_indice buf idx
      | _ ->
        write_expr buf expr ~end_op_code:None;
        is_ref_func := false );
    !is_ref_func
  in
  match mode with
  | Passive ->
    let elem_buf = Buffer.create 16 in
    let is_ref_func = write_init elem_buf init in
    if is_ref_func then begin
      write_u32_of_int buf 1;
      Buffer.add_char buf '\x00';
      Buffer.add_buffer buf elem_buf
    end
    else begin
      write_u32_of_int buf 5;
      write_reftype buf ht;
      Buffer.add_buffer buf elem_buf
    end
  | Declarative ->
    let elem_buf = Buffer.create 16 in
    let is_ref_func = write_init elem_buf init in
    if is_ref_func then begin
      write_u32_of_int buf 3;
      Buffer.add_char buf '\x00';
      Buffer.add_buffer buf elem_buf
    end
    else begin
      write_u32_of_int buf 7;
      write_reftype buf ht;
      Buffer.add_buffer buf elem_buf
    end
  | Active (Some 0, expr) ->
    let elem_buf = Buffer.create 16 in
    let is_ref_func = write_init elem_buf init in
    if is_ref_func then write_u32_of_int buf 0 else write_u32_of_int buf 4;
    write_expr buf expr ~end_op_code:None;
    Buffer.add_buffer buf elem_buf
  | Active (Some i, expr) ->
    let elem_buf = Buffer.create 16 in
    let is_ref_func = write_init elem_buf init in
    if is_ref_func then begin
      write_u32_of_int buf 2;
      write_indice buf i;
      write_expr buf expr ~end_op_code:None;
      Buffer.add_char buf '\x00';
      Buffer.add_buffer buf elem_buf
    end
    else begin
      write_u32_of_int buf 6;
      write_indice buf i;
      write_expr buf expr ~end_op_code:None;
      write_reftype buf ht;
      Buffer.add_buffer buf elem_buf
    end
  | _ -> assert false

let write_data buf ({ init; mode; _ } : Data.t) =
  match mode with
  | Passive ->
    write_u32_of_int buf 1;
    write_string buf init
  | Active (0, expr) ->
    write_u32_of_int buf 0;
    write_expr buf expr ~end_op_code:None;
    write_string buf init
  | Active (i, expr) ->
    write_u32_of_int buf 2;
    write_u32_of_int buf i;
    write_expr buf expr ~end_op_code:None;
    write_string buf init

let encode_section buf id encode_func data =
  let section_buf = Buffer.create 16 in
  encode_func section_buf data;
  let section_len = Buffer.length section_buf in
  if section_len <> 0 then begin
    Buffer.add_char buf id;
    write_u32_of_int buf section_len;
    Buffer.add_buffer buf section_buf
  end

(* type: section 1 *)
let encode_types buf types =
  encode_vector_array buf types (fun buf (_id, (pt, rt)) ->
    Buffer.add_char buf '\x60';
    write_paramtype buf pt;
    write_resulttype buf rt )

(* import: section 2 *)
let encode_imports buf (funcs, tables, memories, globals) =
  let imp_buf = Buffer.create 16 in
  let len =
    List.length funcs + List.length tables + List.length memories
    + List.length globals
  in
  List.iter (write_func_import imp_buf) funcs;
  List.iter (write_table_import imp_buf) tables;
  List.iter (write_memory_import imp_buf) memories;
  List.iter (write_global_import imp_buf) globals;
  write_u32_of_int buf len;
  Buffer.add_buffer buf imp_buf

(* function: section 3 *)
let encode_functions buf (funcs : Binary.Func.t list) =
  let idx = ref 0 in
  encode_vector_list buf funcs (fun buf func ->
    write_block_type_idx buf func.type_f;
    incr idx )

(* table: section 4 *)
let encode_tables buf tables = encode_vector_list buf tables write_table

(* memory: section 5 *)
let encode_memories buf memories = encode_vector_list buf memories write_memory

(* global: section 6 *)
let encode_globals buf globals = encode_vector_list buf globals write_global

(* export: section 7 *)
let encode_exports buf ({ global; mem; table; func } : Module.Exports.t) =
  let exp_buf = Buffer.create 16 in
  let len =
    Array.length global + Array.length mem + Array.length table
    + Array.length func
  in
  let array_rev_iter f a =
    for i = Array.length a - 1 downto 0 do
      f a.(i)
    done
  in
  array_rev_iter (write_export exp_buf '\x03') global;
  array_rev_iter (write_export exp_buf '\x02') mem;
  array_rev_iter (write_export exp_buf '\x01') table;
  array_rev_iter (write_export exp_buf '\x00') func;
  write_u32_of_int buf len;
  Buffer.add_buffer buf exp_buf

(* start: section 8 *)
let encode_start buf int_opt =
  match int_opt with None -> () | Some funcidx -> write_u32_of_int buf funcidx

(* element: section 9 *)
let encode_elements buf elems = encode_vector_array buf elems write_element

(* datacount: section 12 *)
let encode_datacount buf datas =
  let len = Array.length datas in
  write_u32_of_int buf len

(* code: section 10 *)
let encode_codes buf funcs =
  encode_vector_list buf funcs (fun buf { Func.locals; body; _ } ->
    let code_buf = Buffer.create 16 in
    write_locals code_buf locals;
    write_expr code_buf body ~end_op_code:None;
    write_u32_of_int buf (Buffer.length code_buf);
    Buffer.add_buffer buf code_buf )

(* data: section 11 *)
let encode_datas buf datas = encode_vector_array buf datas write_data

let keep_local values =
  List.filter_map
    (function Origin.Local data -> Some data | Origin.Imported _data -> None)
    (Array.to_list values)

let keep_imported values =
  List.filter_map
    (function Origin.Local _data -> None | Origin.Imported data -> Some data)
    (Array.to_list values)

let encode
  ({ func; table; global; exports; start; data; mem; types; elem; _ } :
    Binary.Module.t ) =
  let buf = Buffer.create 256 in

  let local_funcs = keep_local func in
  let local_tables = keep_local table in
  let local_memories = keep_local mem in
  let local_globales = keep_local global in
  let imported_funcs = keep_imported func in
  let imported_tables = keep_imported table in
  let imported_memories = keep_imported mem in
  let imported_globals = keep_imported global in

  Buffer.add_string buf "\x00\x61\x73\x6d";
  (* magic *)
  Buffer.add_string buf "\x01\x00\x00\x00";
  (* version *)
  encode_section buf '\x01' encode_types types;
  encode_section buf '\x02' encode_imports
    (imported_funcs, imported_tables, imported_memories, imported_globals);
  encode_section buf '\x03' encode_functions local_funcs;
  encode_section buf '\x04' encode_tables local_tables;
  encode_section buf '\x05' encode_memories local_memories;
  encode_section buf '\x06' encode_globals local_globales;
  encode_section buf '\x07' encode_exports exports;
  encode_section buf '\x08' encode_start start;
  encode_section buf '\x09' encode_elements elem;
  encode_section buf '\x0C' encode_datacount data;
  encode_section buf '\x0A' encode_codes local_funcs;
  encode_section buf '\x0B' encode_datas data;
  Buffer.contents buf

let write_file outfile filename content =
  let _dir, filename = Fpath.split_base filename in
  let filename = Fpath.set_ext "wasm" filename in
  Bos.OS.File.write (Option.value outfile ~default:filename) content

let convert (outfile : Fpath.t option) (filename : Fpath.t) ~unsafe m =
  Log.info (fun m -> m "binary encoding ...");
  let* m = Compile.Text.until_validate ~unsafe m in
  let content = encode m in
  write_file outfile filename content
