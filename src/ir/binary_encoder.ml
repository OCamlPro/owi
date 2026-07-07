(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
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

let write_heaptype buf ht =
  match ht with
  | Binary.TypeUse id -> write_indice buf id
  | Exn_ht -> Buffer.add_char buf '\x69'
  | Array_ht -> Buffer.add_char buf '\x6A'
  | Struct_ht -> Buffer.add_char buf '\x6B'
  | I31_ht -> Buffer.add_char buf '\x6C'
  | Eq_ht -> Buffer.add_char buf '\x6D'
  | Any_ht -> Buffer.add_char buf '\x6E'
  | Extern_ht -> Buffer.add_char buf '\x6F'
  | Func_ht -> Buffer.add_char buf '\x70'
  | None_ht -> Buffer.add_char buf '\x71'
  | NoExtern_ht -> Buffer.add_char buf '\x72'
  | NoFunc_ht -> Buffer.add_char buf '\x73'
  | NoExn_ht -> Buffer.add_char buf '\x74'

let write_reftype buf (nullable : Text.nullable) ht =
  match nullable with
  | No_null ->
    Buffer.add_char buf '\x64';
    write_heaptype buf ht
  | Null -> (
    match ht with
    | Binary.TypeUse id ->
      write_char_indice buf '\x63' id;
      write_indice buf id
    | _ ->
      (* When it's nullable and an abs_heap_type write it directly *)
      write_heaptype buf ht )

let get_char_valtype = function
  | Binary.Num_type I32 -> '\x7F'
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

let write_resulttype buf (rt : Binary.result_type) =
  encode_vector_list buf rt write_valtype

let write_paramtype buf (pt : Binary.param_type) =
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

let write_global_type buf ((mut, vt) : Binary.Global.Type.t) =
  write_valtype buf vt;
  write_mut buf mut

let write_memory_limits buf (limits : Binary.Mem.Type.limits) : unit =
  match limits with
  | I32 { min; max = None } ->
    Buffer.add_char buf '\x00';
    write_u32 buf min
  | I32 { min; max = Some max } ->
    Buffer.add_char buf '\x01';
    write_u32 buf min;
    write_u32 buf max
  | I64 { min; max = None } ->
    Buffer.add_char buf '\x04';
    write_indice buf min
  | I64 { min; max = Some max } ->
    Buffer.add_char buf '\x05';
    write_indice buf min;
    write_indice buf max

let write_table_limits buf (limits : Binary.Table.Type.limits) : unit =
  match limits with
  | I32 { min; max = None } ->
    Buffer.add_char buf '\x00';
    write_u32 buf min
  | I32 { min; max = Some max } ->
    Buffer.add_char buf '\x01';
    write_u32 buf min;
    write_u32 buf max
  | I64 { min; max = None } ->
    Buffer.add_char buf '\x04';
    write_u64 buf min
  | I64 { min; max = Some max } ->
    Buffer.add_char buf '\x05';
    write_u64 buf min;
    write_u64 buf max

let write_memarg buf idx ({ offset; align } : memarg) =
  if idx = 0 then write_u32 buf align
  else (
    (* Set the 6th bit if the id not 0 *)
    write_u32 buf (Int32.logor align 0x40l);
    write_indice buf idx );
  write_u64 buf offset

let write_memory buf ((_so, limits) : Binary.Mem.t) =
  write_memory_limits buf limits

let write_memory_import buf
  ({ modul_name; name; typ = limits; _ } :
    Binary.Mem.Type.limits Origin.imported ) =
  write_string buf modul_name;
  write_string buf name;
  Buffer.add_char buf '\x02';
  write_memory_limits buf limits

let write_table_import buf
  ({ modul_name; name; typ = limits, (nullable, heaptype); _ } :
    Binary.Table.Type.t Origin.imported ) =
  write_string buf modul_name;
  write_string buf name;
  Buffer.add_char buf '\x01';
  write_reftype buf nullable heaptype;
  write_table_limits buf limits

let write_func_import buf
  ({ modul_name; name; typ; _ } : Binary.block_type Origin.imported) =
  write_string buf modul_name;
  write_string buf name;
  Buffer.add_char buf '\x00';
  write_block_type_idx buf typ

let write_fb buf i =
  Buffer.add_char buf '\xFB';
  write_u32_of_int buf i

let write_fc buf i =
  Buffer.add_char buf '\xFC';
  write_u32_of_int buf i

let write_fd buf i =
  Buffer.add_char buf '\xFD';
  write_u32_of_int buf i

let write_castop buf n1 n2 =
  match (n1, n2) with
  | Text.No_null, Text.No_null -> Buffer.add_char buf '\x00'
  | Null, No_null -> Buffer.add_char buf '\x01'
  | No_null, Null -> Buffer.add_char buf '\x02'
  | Null, Null -> Buffer.add_char buf '\x03'

let write_i32_instr buf : Binary.i32_instr -> _ =
  let add_char c = Buffer.add_char buf c in
  function
  | Load (idx, memarg) ->
    add_char '\x28';
    write_memarg buf idx memarg
  | Load8_s (idx, memarg) ->
    add_char '\x2C';
    write_memarg buf idx memarg
  | Load8_u (idx, memarg) ->
    add_char '\x2D';
    write_memarg buf idx memarg
  | Load16_s (idx, memarg) ->
    add_char '\x2E';
    write_memarg buf idx memarg
  | Load16_u (idx, memarg) ->
    add_char '\x2F';
    write_memarg buf idx memarg
  | Store (idx, memarg) ->
    add_char '\x36';
    write_memarg buf idx memarg
  | Store8 (idx, memarg) ->
    add_char '\x3A';
    write_memarg buf idx memarg
  | Store16 (idx, memarg) ->
    add_char '\x3B';
    write_memarg buf idx memarg
  | Const i ->
    add_char '\x41';
    write_s32 buf i
  | Eqz -> add_char '\x45'
  | Eq -> add_char '\x46'
  | Ne -> add_char '\x47'
  | Lt_s -> add_char '\x48'
  | Lt_u -> add_char '\x49'
  | Gt_s -> add_char '\x4A'
  | Gt_u -> add_char '\x4B'
  | Le_s -> add_char '\x4C'
  | Le_u -> add_char '\x4D'
  | Ge_s -> add_char '\x4E'
  | Ge_u -> add_char '\x4F'
  | Clz -> add_char '\x67'
  | Ctz -> add_char '\x68'
  | Popcnt -> add_char '\x69'
  | Add -> add_char '\x6A'
  | Sub -> add_char '\x6B'
  | Mul -> add_char '\x6C'
  | Div_s -> add_char '\x6D'
  | Div_u -> add_char '\x6E'
  | Rem_s -> add_char '\x6F'
  | Rem_u -> add_char '\x70'
  | And -> add_char '\x71'
  | Or -> add_char '\x72'
  | Xor -> add_char '\x73'
  | Shl -> add_char '\x74'
  | Shr_s -> add_char '\x75'
  | Shr_u -> add_char '\x76'
  | Rotl -> add_char '\x77'
  | Rotr -> add_char '\x78'
  | Wrap_i64 -> add_char '\xA7'
  | Trunc_f_s S32 -> add_char '\xA8'
  | Trunc_f_u S32 -> add_char '\xA9'
  | Trunc_f_s S64 -> add_char '\xAA'
  | Trunc_f_u S64 -> add_char '\xAB'
  | Reinterpret_f S32 -> add_char '\xBC'
  | Reinterpret_f S64 -> raise @@ Failure "TODO: i32.reinterpret_f64"
  | Extend8_s -> add_char '\xC0'
  | Extend16_s -> add_char '\xC1'
  | Trunc_sat_f_s S32 -> write_fc buf 0
  | Trunc_sat_f_u S32 -> write_fc buf 1
  | Trunc_sat_f_s S64 -> write_fc buf 2
  | Trunc_sat_f_u S64 -> write_fc buf 3

let write_i64_instr buf : Binary.i64_instr -> _ =
  let add_char c = Buffer.add_char buf c in
  function
  | Load (idx, memarg) ->
    add_char '\x29';
    write_memarg buf idx memarg
  | Load8_s (idx, memarg) ->
    add_char '\x30';
    write_memarg buf idx memarg
  | Load8_u (idx, memarg) ->
    add_char '\x31';
    write_memarg buf idx memarg
  | Load16_s (idx, memarg) ->
    add_char '\x32';
    write_memarg buf idx memarg
  | Load16_u (idx, memarg) ->
    add_char '\x33';
    write_memarg buf idx memarg
  | Load32_s (idx, memarg) ->
    add_char '\x34';
    write_memarg buf idx memarg
  | Load32_u (idx, memarg) ->
    add_char '\x35';
    write_memarg buf idx memarg
  | Store (idx, memarg) ->
    add_char '\x37';
    write_memarg buf idx memarg
  | Store8 (idx, memarg) ->
    add_char '\x3C';
    write_memarg buf idx memarg
  | Store16 (idx, memarg) ->
    add_char '\x3D';
    write_memarg buf idx memarg
  | Store32 (idx, memarg) ->
    add_char '\x3E';
    write_memarg buf idx memarg
  | Const i ->
    add_char '\x42';
    write_s64 buf i
  | Eqz -> add_char '\x50'
  | Eq -> add_char '\x51'
  | Ne -> add_char '\x52'
  | Lt_s -> add_char '\x53'
  | Lt_u -> add_char '\x54'
  | Gt_s -> add_char '\x55'
  | Gt_u -> add_char '\x56'
  | Le_s -> add_char '\x57'
  | Le_u -> add_char '\x58'
  | Ge_s -> add_char '\x59'
  | Ge_u -> add_char '\x5A'
  | Clz -> add_char '\x79'
  | Ctz -> add_char '\x7A'
  | Popcnt -> add_char '\x7B'
  | Add -> add_char '\x7C'
  | Sub -> add_char '\x7D'
  | Mul -> add_char '\x7E'
  | Div_s -> add_char '\x7F'
  | Div_u -> add_char '\x80'
  | Rem_s -> add_char '\x81'
  | Rem_u -> add_char '\x82'
  | And -> add_char '\x83'
  | Or -> add_char '\x84'
  | Xor -> add_char '\x85'
  | Shl -> add_char '\x86'
  | Shr_s -> add_char '\x87'
  | Shr_u -> add_char '\x88'
  | Rotl -> add_char '\x89'
  | Rotr -> add_char '\x8A'
  | Extend_i32_s -> add_char '\xAC'
  | Extend_i32_u -> add_char '\xAD'
  | Trunc_f_s S32 -> add_char '\xAE'
  | Trunc_f_u S32 -> add_char '\xAF'
  | Trunc_f_s S64 -> add_char '\xB0'
  | Trunc_f_u S64 -> add_char '\xB1'
  | Reinterpret_f S32 -> raise @@ Failure "TODO: i64.reinterpretf32"
  | Reinterpret_f S64 -> add_char '\xBD'
  | Extend8_s -> add_char '\xC2'
  | Extend16_s -> add_char '\xC3'
  | Extend32_s -> add_char '\xC4'
  | Trunc_sat_f_s S32 -> write_fc buf 4
  | Trunc_sat_f_u S32 -> write_fc buf 5
  | Trunc_sat_f_s S64 -> write_fc buf 6
  | Trunc_sat_f_u S64 -> write_fc buf 7

let write_f32_instr buf : Binary.f32_instr -> _ =
  let add_char c = Buffer.add_char buf c in
  function
  | Load (idx, memarg) ->
    add_char '\x2A';
    write_memarg buf idx memarg
  | Store (idx, memarg) ->
    add_char '\x38';
    write_memarg buf idx memarg
  | Const f ->
    add_char '\x43';
    write_f32 buf f
  | Eq -> add_char '\x5B'
  | Ne -> add_char '\x5C'
  | Lt -> add_char '\x5D'
  | Gt -> add_char '\x5E'
  | Le -> add_char '\x5F'
  | Ge -> add_char '\x60'
  | Abs -> add_char '\x8B'
  | Neg -> add_char '\x8C'
  | Ceil -> add_char '\x8D'
  | Floor -> add_char '\x8E'
  | Trunc -> add_char '\x8F'
  | Nearest -> add_char '\x90'
  | Sqrt -> add_char '\x91'
  | Add -> add_char '\x92'
  | Sub -> add_char '\x93'
  | Mul -> add_char '\x94'
  | Div -> add_char '\x95'
  | Min -> add_char '\x96'
  | Max -> add_char '\x97'
  | Copysign -> add_char '\x98'
  | Convert_i_s S32 -> add_char '\xB2'
  | Convert_i_u S32 -> add_char '\xB3'
  | Convert_i_s S64 -> add_char '\xB4'
  | Convert_i_u S64 -> add_char '\xB5'
  | Demote_f64 -> add_char '\xB6'
  | Reinterpret_i S32 -> add_char '\xBE'
  | Reinterpret_i S64 -> raise @@ Failure "TODO: f32.reinterpret_i64"

let write_f64_instr buf : Binary.f64_instr -> _ =
  let add_char c = Buffer.add_char buf c in
  function
  | Load (idx, memarg) ->
    add_char '\x2B';
    write_memarg buf idx memarg
  | Store (idx, memarg) ->
    add_char '\x39';
    write_memarg buf idx memarg
  | Const f ->
    add_char '\x44';
    write_f64 buf f
  | Eq -> add_char '\x61'
  | Ne -> add_char '\x62'
  | Lt -> add_char '\x63'
  | Gt -> add_char '\x64'
  | Le -> add_char '\x65'
  | Ge -> add_char '\x66'
  | Abs -> add_char '\x99'
  | Neg -> add_char '\x9A'
  | Ceil -> add_char '\x9B'
  | Floor -> add_char '\x9C'
  | Trunc -> add_char '\x9D'
  | Nearest -> add_char '\x9E'
  | Sqrt -> add_char '\x9F'
  | Add -> add_char '\xA0'
  | Sub -> add_char '\xA1'
  | Mul -> add_char '\xA2'
  | Div -> add_char '\xA3'
  | Min -> add_char '\xA4'
  | Max -> add_char '\xA5'
  | Copysign -> add_char '\xA6'
  | Convert_i_s S32 -> add_char '\xB7'
  | Convert_i_u S32 -> add_char '\xB8'
  | Convert_i_s S64 -> add_char '\xB9'
  | Convert_i_u S64 -> add_char '\xBA'
  | Promote_f32 -> add_char '\xBB'
  | Reinterpret_i S32 -> raise @@ Failure "TODO: f64.reinterpret_i32"
  | Reinterpret_i S64 -> add_char '\xBF'

let write_v128_instr buf (i : Binary.v128_instr) =
  match i with
  | Const v ->
    write_fd buf 12;
    let a, b = Concrete_v128.to_i64x2 v in
    write_bytes_8 buf a;
    write_bytes_8 buf b
  | Not -> write_fd buf 0x4D
  | And -> write_fd buf 0x4E
  | Or -> write_fd buf 0x50
  | Any_true -> write_fd buf 0x53
  | Load32_lane _ -> raise @@ Failure "TODO: v128.Load32_lane _"
  | Load64_zero _ -> raise @@ Failure "TODO: v128.Load64_zero _"
  | Load _ -> raise @@ Failure "TODO: v128.Load _"
  | Store _ -> raise @@ Failure "TODO: v128.Store _"
  | Load16x4_s _ -> raise @@ Failure "TODO: v128.Load16x4_s _"
  | Load16x4_u _ -> raise @@ Failure "TODO: v128.Load16x4_u _"
  | Bitselect -> raise @@ Failure "TODO: v128.Bitselect"
  | Xor -> raise @@ Failure "TODO: v128.Xor"
  | Andnot -> raise @@ Failure "TODO: v128.Andnot"
  | Load8_splat _ -> raise @@ Failure "TODO: v128.Load8_splat _"
  | Load8_lane _ -> raise @@ Failure "TODO: v128.Load8_lane _"
  | Load8x8_s _ -> raise @@ Failure "TODO: v128.Load8x8_s _"
  | Load8x8_u _ -> raise @@ Failure "TODO: v128.Load8x8_u _"
  | Load16_splat _ -> raise @@ Failure "TODO: v128.Load16_splat _"
  | Load16_lane _ -> raise @@ Failure "TODO: v128.Load16_lane _"
  | Load32_splat _ -> raise @@ Failure "TODO: v128.Load32_splat _"
  | Load32_zero _ -> raise @@ Failure "TODO: v128.Load32_zero _"
  | Load64_splat _ -> raise @@ Failure "TODO: v128.Load64_splat _"
  | Load64_lane _ -> raise @@ Failure "TODO: v128.Load64_lane _"
  | Store8_lane _ -> raise @@ Failure "TODO: v128.Store8_lane _"
  | Store64_lane _ -> raise @@ Failure "TODO: v128.Store64_lane _"
  | Store32_zero _ -> raise @@ Failure "TODO: v128.Store32_zero _"
  | Store32_lane _ -> raise @@ Failure "TODO: v128.Store32_lane _"
  | Store16_lane _ -> raise @@ Failure "TODO: v128.Store16_lane _"
  | Load32x2_s _ -> raise @@ Failure "TODO: v128.Load32x2_s _"
  | Load32x2_u _ -> raise @@ Failure "TODO: v128.Load32x2_u _"

let write_i8x16_instr buf : Text.i8x16_instr -> _ = function
  | Add -> write_fd buf 0x6E
  | Sub -> write_fd buf 0x71
  | Eq -> write_fd buf 0x23
  | Ne -> write_fd buf 0x24
  | Lt_s -> write_fd buf 0x25
  | Lt_u -> write_fd buf 0x26
  | Gt_s -> write_fd buf 0x27
  | Gt_u -> write_fd buf 0x28
  | Le_s -> write_fd buf 0x29
  | Le_u -> write_fd buf 0x2A
  | Ge_s -> write_fd buf 0x2B
  | Ge_u -> write_fd buf 0x2C
  | Abs -> write_fd buf 0x60
  | Neg -> write_fd buf 0x61
  | Popcnt -> write_fd buf 0x62
  | All_true -> write_fd buf 0x63
  | Bitmask -> write_fd buf 0x64
  | Swizzle -> write_fd buf 0x0E
  | Splat -> write_fd buf 0x0F
  | Shuffle _ -> raise @@ Failure "TODO (i8x16.shuffle)"
  | Shl -> raise @@ Failure "TODO (i8x16.shl)"
  | Min_s -> raise @@ Failure "TODO (i8x16.min_s)"
  | Extract_lane_s _lane_index -> raise @@ Failure "TODO (i8x16.extract_lane_s)"
  | Add_sat_s -> raise @@ Failure "TODO (i8x16.add_sat_s)"
  | Shr_s -> raise @@ Failure "TODO: i8x16.Shr_s"
  | Shr_u -> raise @@ Failure "TODO: i8x16.Shr_u"
  | Min_u -> raise @@ Failure "TODO: i8x16.Min_u"
  | Add_sat_u -> raise @@ Failure "TODO: i8x16.Add_sat_u"
  | Sub_sat_s -> raise @@ Failure "TODO: i8x16.Sub_sat_s"
  | Sub_sat_u -> raise @@ Failure "TODO: i8x16.Sub_sat_u"
  | Max_s -> raise @@ Failure "TODO: i8x16.Max_s"
  | Max_u -> raise @@ Failure "TODO: i8x16.Max_u"
  | Narrow_i16x8_s -> raise @@ Failure "TODO: i8x16.Narrow_i16x8_s"
  | Narrow_i16x8_u -> raise @@ Failure "TODO: i8x16.Narrow_i16x8_u"
  | Avgr_u -> raise @@ Failure "TODO: i8x16.Avgr_u"
  | Extract_lane_u _ -> raise @@ Failure "TODO: i8x16.Extract_lane_u _"
  | Replace_lane _ -> raise @@ Failure "TODO: i8x16.Replace_lane _"

let write_i16x8_instr buf : Text.i16x8_instr -> _ = function
  | Eq -> write_fd buf 0x2D
  | Ne -> write_fd buf 0x2E
  | Lt_s -> write_fd buf 0x2F
  | Lt_u -> write_fd buf 0x30
  | Gt_s -> write_fd buf 0x31
  | Gt_u -> write_fd buf 0x32
  | Le_s -> write_fd buf 0x33
  | Le_u -> write_fd buf 0x34
  | Ge_s -> write_fd buf 0x35
  | Ge_u -> write_fd buf 0x36
  | Add -> write_fd buf 0x8E
  | Sub -> write_fd buf 0x91
  | Mul -> write_fd buf 0x95
  | Splat -> write_fd buf 0x10
  | Extract_lane_s _n -> raise @@ Failure "TODO: i16x8.Extract_lane_s _n"
  | Extract_lane_u _n -> raise @@ Failure "TODO: i16x8.Extract_lane_u _n"
  | Q15mulr_sat_s -> raise @@ Failure "TODO: i16x8.Q15mulr_sat_s"
  | Min_s -> raise @@ Failure "TODO: i16x8.Min_s"
  | Min_u -> raise @@ Failure "TODO: i16x8.Min_u"
  | Extmul_low_i8x16_s -> raise @@ Failure "TODO: i16x8.Extmul_low_i8x16_s"
  | Extmul_low_i8x16_u -> raise @@ Failure "TODO: i16x8.Extmul_low_i8x16_u"
  | Extmul_high_i8x16_s -> raise @@ Failure "TODO: i16x8.Extmul_high_i8x16_s"
  | Extmul_high_i8x16_u -> raise @@ Failure "TODO: i16x8.Extmul_high_i8x16_u"
  | Extend_low_i8x16_s -> raise @@ Failure "TODO: i16x8.Extend_low_i8x16_s"
  | Extend_low_i8x16_u -> raise @@ Failure "TODO: i16x8.Extend_low_i8x16_u"
  | Extend_high_i8x16_s -> raise @@ Failure "TODO: i16x8.Extend_high_i8x16_s"
  | Extend_high_i8x16_u -> raise @@ Failure "TODO: i16x8.Extend_high_i8x16_u"
  | Extadd_pairwise_i8x16_s ->
    raise @@ Failure "TODO: i16x8.Extadd_pairwise_i8x16_s"
  | Extadd_pairwise_i8x16_u ->
    raise @@ Failure "TODO: i16x8.Extadd_pairwise_i8x16_u"
  | Add_sat_s -> raise @@ Failure "TODO: i16x8.Add_sat_s"
  | Add_sat_u -> raise @@ Failure "TODO: i16x8.Add_sat_u"
  | Sub_sat_s -> raise @@ Failure "TODO: i16x8.Sub_sat_s"
  | Sub_sat_u -> raise @@ Failure "TODO: i16x8.Sub_sat_u"
  | Max_s -> raise @@ Failure "TODO: i16x8.Max_s"
  | Max_u -> raise @@ Failure "TODO: i16x8.Max_u"
  | Shl -> raise @@ Failure "TODO: i16x8.Shl"
  | Neg -> raise @@ Failure "TODO: i16x8.Neg"
  | All_true -> raise @@ Failure "TODO: i16x8.All_true"
  | Shr_s -> raise @@ Failure "TODO: i16x8.Shr_s"
  | Shr_u -> raise @@ Failure "TODO: i16x8.Shr_u"
  | Bitmask -> raise @@ Failure "TODO: i16x8.Bitmask"
  | Avgr_u -> raise @@ Failure "TODO: i16x8.Avgr_u"
  | Abs -> raise @@ Failure "TODO: i16x8.Abs"
  | Narrow_i32x4_s -> raise @@ Failure "TODO: i16x8.Narrow_i32x4_s"
  | Narrow_i32x4_u -> raise @@ Failure "TODO: i16x8.Narrow_i32x4_u"
  | Replace_lane _ -> raise @@ Failure "TODO: i16x8.Replace_lane _"

let write_i32x4_instr buf : Text.i32x4_instr -> _ = function
  | Add -> write_fd buf 174
  | Sub -> write_fd buf 177
  | Mul -> write_fd buf 0xB5
  | Shl -> write_fd buf 0xAB
  | Shr_s -> write_fd buf 0xAC
  | Shr_u -> write_fd buf 0xAD
  | Eq -> write_fd buf 0x37
  | Ne -> write_fd buf 0x38
  | Lt_s -> write_fd buf 0x39
  | Lt_u -> write_fd buf 0x3A
  | Gt_s -> write_fd buf 0x3B
  | Gt_u -> write_fd buf 0x3C
  | Le_s -> write_fd buf 0x3D
  | Le_u -> write_fd buf 0x3E
  | Ge_s -> write_fd buf 0x3F
  | Ge_u -> write_fd buf 0x40
  | Splat -> write_fd buf 0x11
  | Extract_lane _n -> raise @@ Failure "TODO"
  | Replace_lane _n -> raise @@ Failure "TODO"
  | Extend_low_i16x8_s -> write_fd buf 0xA7
  | Extend_high_i16x8_s -> write_fd buf 0xA8
  | Extend_low_i16x8_u -> write_fd buf 0xA9
  | Extend_high_i16x8_u -> write_fd buf 0xAA
  | Trunc_sat_f64x2_s_zero ->
    raise @@ Failure "TODO: i32x4.Trunc_sat_f64x2_s_zero"
  | Trunc_sat_f64x2_u_zero ->
    raise @@ Failure "TODO: i32x4.Trunc_sat_f64x2_u_zero"
  | Trunc_sat_f32x4_s -> raise @@ Failure "TODO: i32x4.Trunc_sat_f32x4_s"
  | Trunc_sat_f32x4_u -> raise @@ Failure "TODO: i32x4.Trunc_sat_f32x4_u"
  | Min_s -> raise @@ Failure "TODO: i32x4.Min_s"
  | Min_u -> raise @@ Failure "TODO: i32x4.Min_u"
  | Extmul_low_i16x8_s -> raise @@ Failure "TODO: i32x4.Extmul_low_i16x8_s"
  | Extmul_low_i16x8_u -> raise @@ Failure "TODO: i32x4.Extmul_low_i16x8_u"
  | Extmul_high_i16x8_s -> raise @@ Failure "TODO: i32x4.Extmul_high_i16x8_s"
  | Extmul_high_i16x8_u -> raise @@ Failure "TODO: i32x4.Extmul_high_i16x8_u"
  | Extadd_pairwise_i16x8_s ->
    raise @@ Failure "TODO: i32x4.Extadd_pairwise_i16x8_s"
  | Extadd_pairwise_i16x8_u ->
    raise @@ Failure "TODO: i32x4.Extadd_pairwise_i16x8_u"
  | Dot_i16x8_s -> raise @@ Failure "TODO: i32x4.Dot_i16x8_s"
  | Neg -> raise @@ Failure "TODO: i32x4.Neg"
  | Max_s -> raise @@ Failure "TODO: i32x4.Max_s"
  | Max_u -> raise @@ Failure "TODO: i32x4.Max_u"
  | Abs -> raise @@ Failure "TODO: i32x4.Abs"
  | All_true -> raise @@ Failure "TODO: i32x4.All_true"
  | Bitmask -> raise @@ Failure "TODO: i32x4.Bitmask"

let write_i64x2_instr buf : Text.i64x2_instr -> _ = function
  | Add -> write_fd buf 0xCE
  | Sub -> write_fd buf 0xD1
  | Mul -> write_fd buf 0xD5
  | Eq -> write_fd buf 0xD6
  | Ne -> write_fd buf 0xD7
  | Lt_s -> write_fd buf 0xD8
  | Gt_s -> write_fd buf 0xD9
  | Le_s -> write_fd buf 0xDA
  | Ge_s -> write_fd buf 0xDB
  | Splat -> write_fd buf 0x12
  | Extend_low_i32x4_s -> raise @@ Failure "TODO"
  | Extend_low_i32x4_u -> raise @@ Failure "TODO"
  | Extend_high_i32x4_s -> raise @@ Failure "TODO: i64x2.Extend_high_i32x4_s"
  | Extend_high_i32x4_u -> raise @@ Failure "TODO: i64x2.Extend_high_i32x4_u"
  | Extmul_low_i32x4_s -> raise @@ Failure "TODO: i64x2.Extmul_low_i32x4_s"
  | Extmul_low_i32x4_u -> raise @@ Failure "TODO: i64x2.Extmul_low_i32x4_u"
  | Extmul_high_i32x4_s -> raise @@ Failure "TODO: i64x2.Extmul_high_i32x4_s"
  | Extmul_high_i32x4_u -> raise @@ Failure "TODO: i64x2.Extmul_high_i32x4_u"
  | Abs -> raise @@ Failure "TODO: i64x2.Abs"
  | Neg -> raise @@ Failure "TODO: i64x2.Neg"
  | All_true -> raise @@ Failure "TODO: i64x2.All_true"
  | Bitmask -> raise @@ Failure "TODO: i64x2.Bitmask"
  | Shl -> raise @@ Failure "TODO: i64x2.Shl"
  | Shr_s -> raise @@ Failure "TODO: i64x2.Shr_s"
  | Shr_u -> raise @@ Failure "TODO: i64x2.Shr_u"
  | Extract_lane _ -> raise @@ Failure "TODO: i64x2.Extract_lane _"
  | Replace_lane _ -> raise @@ Failure "TODO: i64x2.Replace_lane _"

let write_f32x4_instr _buf : Text.f32x4_instr -> _ = function
  | Abs -> raise @@ Failure "TODO: f32x4.Abs"
  | Pmin -> raise @@ Failure "TODO: f32x4.Pmin"
  | Min -> raise @@ Failure "TODO: f32x4.Min"
  | Eq -> raise @@ Failure "TODO: f32x4.Eq"
  | Convert_i32x4_s -> raise @@ Failure "TODO: f32x4.Convert_i32x4_s"
  | Convert_i32x4_u -> raise @@ Failure "TODO: f32x4.Convert_i32x4_u"
  | Ceil -> raise @@ Failure "TODO: f32x4.Ceil"
  | Add -> raise @@ Failure "TODO: f32x4.Add"
  | Max -> raise @@ Failure "TODO: f32x4.Max"
  | Floor -> raise @@ Failure "TODO: f32x4.Floor"
  | Pmax -> raise @@ Failure "TODO: f32x4.Pmax"
  | Ne -> raise @@ Failure "TODO: f32x4.Ne"
  | Sub -> raise @@ Failure "TODO: f32x4.Sub"
  | Trunc -> raise @@ Failure "TODO: f32x4.Trunc"
  | Lt -> raise @@ Failure "TODO: f32x4.Lt"
  | Gt -> raise @@ Failure "TODO: f32x4.Gt"
  | Le -> raise @@ Failure "TODO: f32x4.Le"
  | Ge -> raise @@ Failure "TODO: f32x4.Ge"
  | Mul -> raise @@ Failure "TODO: f32x4.Mul"
  | Convert_low_i32x4_s -> raise @@ Failure "TODO: f32x4.Convert_low_i32x4_s"
  | Convert_low_i32x4_u -> raise @@ Failure "TODO: f32x4.Convert_low_i32x4_u"
  | Convert_high_i32x4_s -> raise @@ Failure "TODO: f32x4.Convert_high_i32x4_s"
  | Convert_high_i32x4_u -> raise @@ Failure "TODO: f32x4.Convert_high_i32x4_u"
  | Splat -> raise @@ Failure "TODO: f32x4.Splat"
  | Nearest -> raise @@ Failure "TODO: f32x4.Nearest"
  | Div -> raise @@ Failure "TODO: f32x4.Div"
  | Neg -> raise @@ Failure "TODO: f32x4.Neg"
  | Sqrt -> raise @@ Failure "TODO: f32x4.Sqrt"
  | Demote_f64x2_zero -> raise @@ Failure "TODO: f32x4.Demote_f64x2_zero"
  | Extract_lane _ -> raise @@ Failure "TODO: f32x4.Extract_lane _"
  | Replace_lane _ -> raise @@ Failure "TODO: f32x4.Replace_lane _"

let write_f64x2_instr _buf : Text.f64x2_instr -> _ = function
  | Abs -> raise @@ Failure "TODO: f64x2.Abs"
  | Pmin -> raise @@ Failure "TODO: f64x2.Pmin"
  | Min -> raise @@ Failure "TODO: f64x2.Min"
  | Eq -> raise @@ Failure "TODO: f64x2.Eq"
  | Ceil -> raise @@ Failure "TODO: f64x2.Ceil"
  | Add -> raise @@ Failure "TODO: f64x2.Add"
  | Max -> raise @@ Failure "TODO: f64x2.Max"
  | Floor -> raise @@ Failure "TODO: f64x2.Floor"
  | Pmax -> raise @@ Failure "TODO: f64x2.Pmax"
  | Ne -> raise @@ Failure "TODO: f64x2.Ne"
  | Sub -> raise @@ Failure "TODO: f64x2.Sub"
  | Trunc -> raise @@ Failure "TODO: f64x2.Trunc"
  | Lt -> raise @@ Failure "TODO: f64x2.Lt"
  | Gt -> raise @@ Failure "TODO: f64x2.Gt"
  | Le -> raise @@ Failure "TODO: f64x2.Le"
  | Ge -> raise @@ Failure "TODO: f64x2.Ge"
  | Mul -> raise @@ Failure "TODO: f64x2.Mul"
  | Convert_low_i32x4_s -> raise @@ Failure "TODO: f64x2.Convert_low_i32x4_s"
  | Convert_low_i32x4_u -> raise @@ Failure "TODO: f64x2.Convert_low_i32x4_u"
  | Convert_high_i32x4_s -> raise @@ Failure "TODO: f64x2.Convert_high_i32x4_s"
  | Convert_high_i32x4_u -> raise @@ Failure "TODO: f64x2.Convert_high_i32x4_u"
  | Nearest -> raise @@ Failure "TODO: f64x2.Nearest"
  | Div -> raise @@ Failure "TODO: f64x2.Div"
  | Neg -> raise @@ Failure "TODO: f64x2.Neg"
  | Sqrt -> raise @@ Failure "TODO: f64x2.Sqrt"
  | Splat -> raise @@ Failure "TODO: f64x2.Splat"
  | Promote_low_f32x4 -> raise @@ Failure "TODO: f64x2.Promote_low_f32x4"
  | Extract_lane _ -> raise @@ Failure "TODO: f64x2.Extract_lane _"
  | Replace_lane _ -> raise @@ Failure "TODO: f64x2.Replace_lane _"

let write_ref_instr buf : Binary.ref_instr -> _ =
  let add_char c = Buffer.add_char buf c in
  function
  | Null rt ->
    add_char '\xD0';
    write_reftype buf Text.Null rt
  | Is_null -> add_char '\xD1'
  | Func idx -> write_char_indice buf '\xD2' idx
  | Eq -> add_char '\xD3'
  | As_non_null -> add_char '\xD4'
  | Test (No_null, ht) ->
    add_char '\xFB';
    write_u32 buf 20l;
    write_heaptype buf ht
  | Test (Null, ht) ->
    add_char '\xFB';
    write_u32 buf 21l;
    write_heaptype buf ht
  | Cast (No_null, ht) ->
    add_char '\xFB';
    write_u32 buf 22l;
    write_heaptype buf ht
  | Cast (Null, ht) ->
    add_char '\xFB';
    write_u32 buf 23l;
    write_heaptype buf ht

let write_local_instr buf : Binary.local_instr -> _ = function
  | Get idx -> write_char_indice buf '\x20' idx
  | Set idx -> write_char_indice buf '\x21' idx
  | Tee idx -> write_char_indice buf '\x22' idx

let write_global_instr buf : Binary.global_instr -> _ = function
  | Get idx -> write_char_indice buf '\x23' idx
  | Set idx -> write_char_indice buf '\x24' idx

let write_table_instr buf : Binary.table_instr -> _ = function
  | Get idx -> write_char_indice buf '\x25' idx
  | Set idx -> write_char_indice buf '\x26' idx
  | Init (tableidx, elemidx) ->
    write_fc buf 12;
    write_indice buf elemidx;
    write_indice buf tableidx
  | Copy (idx1, idx2) ->
    write_fc buf 14;
    write_indice buf idx1;
    write_indice buf idx2
  | Grow idx ->
    write_fc buf 15;
    write_indice buf idx
  | Size idx ->
    write_fc buf 16;
    write_indice buf idx
  | Fill idx ->
    write_fc buf 17;
    write_indice buf idx

let write_elem_instr buf : Binary.elem_instr -> _ = function
  | Drop idx ->
    write_fc buf 13;
    write_indice buf idx

let write_memory_instr buf : Binary.memory_instr -> _ = function
  | Size idx -> write_char_indice buf '\x3F' idx
  | Grow idx -> write_char_indice buf '\x40' idx
  | Init (memidx, dataidx) ->
    write_fc buf 8;
    write_indice buf dataidx;
    write_indice buf memidx
  | Copy (id1, id2) ->
    write_fc buf 10;
    write_indice buf id1;
    write_indice buf id2
  | Fill idx ->
    write_fc buf 11;
    write_indice buf idx

let write_data_instr buf : Binary.data_instr -> _ = function
  | Drop idx ->
    write_fc buf 9;
    write_indice buf idx

let write_struct_instr buf : Binary.struct_instr -> _ = function
  | New x ->
    Buffer.add_char buf '\xFB';
    write_u32 buf 0l;
    write_indice buf x
  | New_default x ->
    Buffer.add_char buf '\xFB';
    write_u32 buf 1l;
    write_indice buf x
  | Get (x, i) ->
    Buffer.add_char buf '\xFB';
    write_u32 buf 2l;
    write_indice buf x;
    write_indice buf i
  | Get_s (x, i) ->
    Buffer.add_char buf '\xFB';
    write_u32 buf 3l;
    write_indice buf x;
    write_indice buf i
  | Get_u (x, i) ->
    Buffer.add_char buf '\xFB';
    write_u32 buf 4l;
    write_indice buf x;
    write_indice buf i
  | Set (x, i) ->
    Buffer.add_char buf '\xFB';
    write_u32 buf 5l;
    write_indice buf x;
    write_indice buf i

let write_array_instr buf : Binary.array_instr -> _ = function
  | New x ->
    Buffer.add_char buf '\xFB';
    write_u32 buf 6l;
    write_indice buf x
  | New_default x ->
    Buffer.add_char buf '\xFB';
    write_u32 buf 7l;
    write_indice buf x
  | New_fixed (x, i) ->
    Buffer.add_char buf '\xFB';
    write_u32 buf 8l;
    write_indice buf x;
    write_u32 buf i
  | New_data (x, y) ->
    Buffer.add_char buf '\xFB';
    write_u32 buf 9l;
    write_indice buf x;
    write_indice buf y
  | New_elem (x, y) ->
    Buffer.add_char buf '\xFB';
    write_u32 buf 10l;
    write_indice buf x;
    write_indice buf y
  | Get x ->
    Buffer.add_char buf '\xFB';
    write_u32 buf 11l;
    write_indice buf x
  | Get_s x ->
    Buffer.add_char buf '\xFB';
    write_u32 buf 12l;
    write_indice buf x
  | Get_u x ->
    Buffer.add_char buf '\xFB';
    write_u32 buf 13l;
    write_indice buf x
  | Set x ->
    Buffer.add_char buf '\xFB';
    write_u32 buf 14l;
    write_indice buf x
  | Len ->
    Buffer.add_char buf '\xFB';
    write_u32 buf 15l
  | Fill x ->
    Buffer.add_char buf '\xFB';
    write_u32 buf 16l;
    write_indice buf x
  | Copy (x, y) ->
    Buffer.add_char buf '\xFB';
    write_u32 buf 17l;
    write_indice buf x;
    write_indice buf y
  | Init_data (x, y) ->
    Buffer.add_char buf '\xFB';
    write_u32 buf 18l;
    write_indice buf x;
    write_indice buf y
  | Init_elem (x, y) ->
    Buffer.add_char buf '\xFB';
    write_u32 buf 19l;
    write_indice buf x;
    write_indice buf y

let write_i31_instr buf : Text.i31_instr -> _ = function
  | Ref ->
    Buffer.add_char buf '\xFB';
    write_u32 buf 28l
  | Get_s ->
    Buffer.add_char buf '\xFB';
    write_u32 buf 29l
  | Get_u ->
    Buffer.add_char buf '\xFB';
    write_u32 buf 30l

let rec write_instr buf instr =
  let add_char c = Buffer.add_char buf c in
  match instr.Annotated.raw with
  | I32 i -> write_i32_instr buf i
  | I64 i -> write_i64_instr buf i
  | F32 i -> write_f32_instr buf i
  | F64 i -> write_f64_instr buf i
  | V128 i -> write_v128_instr buf i
  | I8x16 i -> write_i8x16_instr buf i
  | I16x8 i -> write_i16x8_instr buf i
  | I32x4 i -> write_i32x4_instr buf i
  | I64x2 i -> write_i64x2_instr buf i
  | F32x4 i -> write_f32x4_instr buf i
  | F64x2 i -> write_f64x2_instr buf i
  | Ref i -> write_ref_instr buf i
  | Local i -> write_local_instr buf i
  | Global i -> write_global_instr buf i
  | Table i -> write_table_instr buf i
  | Elem i -> write_elem_instr buf i
  | Memory i -> write_memory_instr buf i
  | Data i -> write_data_instr buf i
  | Struct i -> write_struct_instr buf i
  | Array i -> write_array_instr buf i
  | I31 i -> write_i31_instr buf i
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
  | Br_on_null idx -> write_char_indice buf '\xD5' idx
  | Br_on_non_null idx -> write_char_indice buf '\xD6' idx
  | Br_on_cast (id, (n1, ht1), (n2, ht2)) ->
    write_fb buf 24;
    write_castop buf n1 n2;
    write_indice buf id;
    write_heaptype buf ht1;
    write_heaptype buf ht2
  | Br_on_cast_fail (id, (n1, ht1), (n2, ht2)) ->
    write_fb buf 25;
    write_castop buf n1 n2;
    write_indice buf id;
    write_heaptype buf ht1;
    write_heaptype buf ht2
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
  | Any_convert_extern ->
    add_char '\xFB';
    write_u32 buf 26l
  | Extern_convert_any ->
    add_char '\xFB';
    write_u32 buf 27l
  | Return_call _ -> raise @@ Failure "TODO: Return_call _ "
  | Return_call_indirect _ -> raise @@ Failure "TODO: Return_call_indirect _ "
  | Return_call_ref _ -> raise @@ Failure "TODO: Return_call_ref _ "
  | Call_ref _ -> raise @@ Failure "TODO: call_ref"

and write_expr buf expr ~end_op_code =
  List.iter (write_instr buf) expr.Annotated.raw;
  let end_op_code = Option.value end_op_code ~default:'\x0B' in
  Buffer.add_char buf end_op_code

let write_table buf { Table.typ = limits, (nullable, heaptype); init; _ } =
  match init with
  | Some e ->
    Buffer.add_char buf '\x40';
    Buffer.add_char buf '\x00';
    write_reftype buf nullable heaptype;
    write_table_limits buf limits;
    write_expr buf e ~end_op_code:None
  | None ->
    write_reftype buf nullable heaptype;
    write_table_limits buf limits

let write_export buf cid ({ name; id } : Binary.Export.t) =
  write_string buf name;
  Buffer.add_char buf cid;
  write_u32_of_int buf id

let write_global buf ({ typ; init; _ } : Global.t) =
  write_global_type buf typ;
  write_expr buf init ~end_op_code:None

let write_global_import buf
  ({ modul_name; name; typ = mut, valtype; _ } : Global.Type.t Origin.imported)
    =
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

let write_element buf ({ typ = nullable, ht; init; mode; _ } : Elem.t) =
  let write_init buf init =
    let is_ref_func = ref true in
    encode_vector_list buf init (fun buf expr ->
      match expr.Annotated.raw with
      | [ { Annotated.raw = Ref (Func idx); _ } ] -> write_indice buf idx
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
      write_reftype buf nullable ht;
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
      write_reftype buf nullable ht;
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
      write_reftype buf nullable ht;
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

let encode_storage_type buf st =
  match st with
  | Val_type vt -> write_valtype buf vt
  | Pack_type I16 -> Buffer.add_char buf '\x77'
  | Pack_type I8 -> Buffer.add_char buf '\x78'

let encode_mut buf : Text.mut -> unit = function
  | Const -> Buffer.add_char buf '\x00'
  | Var -> Buffer.add_char buf '\x01'

let encode_field_type buf (mut, st) =
  encode_storage_type buf st;
  encode_mut buf mut

let encode_comp_type buf ct =
  match ct with
  | Def_array_t ft ->
    Buffer.add_char buf '\x5E';
    encode_field_type buf ft
  | Def_struct_t ftl ->
    Buffer.add_char buf '\x5F';
    List.iter (fun (_, ft) -> encode_field_type buf ft) ftl
  | Def_func_t (pt, rt) ->
    Buffer.add_char buf '\x60';
    write_paramtype buf pt;
    write_resulttype buf rt

let encode_sub_type buf st =
  match st with
  | { final = true; ids = []; ct } -> encode_comp_type buf ct
  | { final; ids; ct } ->
    Buffer.add_char buf (if final then '\x4F' else '\x50');
    List.iter (write_indice buf) ids;
    encode_comp_type buf ct

(* type: section 1 *)
let encode_types buf types =
  encode_vector_array buf types (fun buf st ->
    match st with
    | Typedef.SimpleType (_, st) -> encode_sub_type buf st
    | Typedef.RecType stl ->
      Buffer.add_char buf '\x4E';
      List.iter (fun (_, st) -> encode_sub_type buf st) stl )

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
let encode_exports buf ({ global; mem; table; func; tag } : Module.Exports.t) =
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
  array_rev_iter (write_export exp_buf '\x04') tag;
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
