open Types

module Input = struct
  type t =
    { bytes : bytes
    ; pt : int
    ; size : int
    }

  let _pp input =
    print_int input.pt;
    print_string " ";
    print_int input.size;
    print_newline ()

  let is_empty input = input.size = 0

  let from_bytes bytes =
    let size = Bytes.length bytes in
    { bytes; pt = 0; size }

  let sub ~pos ~len input =
    if pos <= input.size && len <= input.size - pos then
      { input with pt = input.pt + pos; size = len }
    else failwith "Input.sub error"

  let sub_suffix pos input = sub ~pos ~len:(input.size - pos) input

  let sub_prefix len input = sub ~pos:0 ~len input

  let get n input =
    if n < input.size then Some (Bytes.get input.bytes (input.pt + n)) else None

  let get0 = get 0
end

let read_byte input =
  match Input.get0 input with
  | None -> failwith "read_byte error"
  | Some c ->
    let next_input = Input.sub_suffix 1 input in
    (c, next_input)

(* TODO: U32 spec implem. For now, only 1 byte *)
let read_U32 input =
  match Input.get0 input with
  | None -> failwith "read_U32 error"
  | Some c ->
    let next_input = Input.sub_suffix 1 input in
    (Char.code c, next_input)

let read_U64 = read_U32

let deserialize_indice = read_U32

let deserialize_valtype input =
  let b, input = read_byte input in
  match b with
  | '\x7F' -> (Num_type I32, input)
  | '\x7E' -> (Num_type I64, input)
  | '\x7D' -> (Num_type F32, input)
  | '\x7C' -> (Num_type F64, input)
  (* | '\x7B' -> V128, input *)
  (* | '\x70' -> FUNCREF, input *)
  (* | '\x6F' -> EXTERNREF, input *)
  | _ -> failwith "deserialize_valtype error"

let deserialize_mut input =
  let b, input = read_byte input in
  match b with
  | '\x00' -> (Const, input)
  | '\x01' -> (Var, input)
  | _ -> failwith "deserialize_mut error"

let deserialize_limits input =
  let b, input = read_byte input in
  match b with
  | '\x00' ->
    let min, input = read_U32 input in
    ({ min; max = None }, input)
  | '\x01' ->
    let min, input = read_U32 input in
    let max, input = read_U32 input in
    ({ min; max = Some max }, input)
  | _ -> failwith "deserialize_mut error"

let deserialize_memarg input =
  let align, input = read_U32 input in
  let offset, input = read_U32 input in
  let align = Int32.of_int align in
  let offset = Int32.of_int offset in
  ({ align; offset }, input)

let deserialize_FC input =
  let i, input = read_U32 input in
  match i with
  | 0 -> (Some (I_trunc_sat_f (S32, S32, S)), input)
  | 1 -> (Some (I_trunc_sat_f (S32, S32, U)), input)
  | 2 -> (Some (I_trunc_sat_f (S32, S64, S)), input)
  | 3 -> (Some (I_trunc_sat_f (S32, S64, U)), input)
  | 4 -> (Some (I_trunc_sat_f (S64, S32, S)), input)
  | 5 -> (Some (I_trunc_sat_f (S64, S32, U)), input)
  | 6 -> (Some (I_trunc_sat_f (S64, S64, S)), input)
  | 7 -> (Some (I_trunc_sat_f (S64, S64, U)), input)
  | 8 ->
    let dataidx, input = deserialize_indice input in
    let b, input = read_byte input in
    begin
      match b with
      | '\x00' -> (Some (Memory_init (Raw dataidx)), input)
      | _ -> failwith "deserialize_FC id=8 error"
    end
  | 9 ->
    let dataidx, input = deserialize_indice input in
    (Some (Data_drop (Raw dataidx)), input)
  | 10 ->
    let b, input = read_byte input in
    begin
      match b with
      | '\x00' ->
        let b, input = read_byte input in
        begin
          match b with
          | '\x00' -> (Some Memory_copy, input)
          | _ -> failwith "deserialize_FC id=10 error"
        end
      | _ -> failwith "deserialize_FC id=10 error"
    end
  | 11 ->
    let b, input = read_byte input in
    begin
      match b with
      | '\x00' -> (Some Memory_fill, input)
      | _ -> failwith "deserialize_FC id=11 error"
    end
  | 12 ->
    let elemidx, input = deserialize_indice input in
    let tableidx, input = deserialize_indice input in
    (Some (Table_init (Raw tableidx, Raw elemidx)), input)
  | 13 ->
    let elemidx, input = deserialize_indice input in
    (Some (Elem_drop (Raw elemidx)), input)
  | 14 ->
    let tableidx1, input = deserialize_indice input in
    let tableidx2, input = deserialize_indice input in
    (Some (Table_copy (Raw tableidx1, Raw tableidx2)), input)
  | 15 ->
    let tableidx, input = deserialize_indice input in
    (Some (Table_grow (Raw tableidx)), input)
  | 16 ->
    let tableidx, input = deserialize_indice input in
    (Some (Table_size (Raw tableidx)), input)
  | 17 ->
    let tableidx, input = deserialize_indice input in
    (Some (Table_fill (Raw tableidx)), input)
  | _ -> failwith "deserialize_FC error"

let deserialize_instr input =
  let b, next_input = read_byte input in
  match b with
  | '\x00' -> (Some Unreachable, next_input)
  | '\x01' -> (Some Nop, next_input)
  | '\x0C' ->
    let labelidx, next_input = deserialize_indice next_input in
    (Some (Br (Raw labelidx)), next_input)
  | '\x0D' ->
    let labelidx, next_input = deserialize_indice next_input in
    (Some (Br_if (Raw labelidx)), next_input)
  | '\x0F' -> (Some Return, next_input)
  | '\x10' ->
    let funcidx, next_input = deserialize_indice next_input in
    (Some (Call (Raw funcidx)), next_input)
    (* | '\x11' ->
       let typeidx, next_input = deserialize_indice next_input in
       let tableidx, next_input = deserialize_indice next_input in
       (Some (Call_indirect (Raw tableidx, Raw )), next_input) *)
    (* Call_indirect of 'a indice * 'a block_type *)
  | '\x1A' ->
    (Some Drop, next_input)
    (* | '\x1B' -> (Select, input)
       | '\x1C' -> (Select t, input) *)
  | '\x20' ->
    let localidx, next_input = deserialize_indice next_input in
    (Some (Local_get (Raw localidx)), next_input)
  | '\x21' ->
    let localidx, next_input = deserialize_indice next_input in
    (Some (Local_set (Raw localidx)), next_input)
  | '\x22' ->
    let localidx, next_input = deserialize_indice next_input in
    (Some (Local_tee (Raw localidx)), next_input)
  | '\x23' ->
    let globalidx, next_input = deserialize_indice next_input in
    (Some (Global_get (Raw globalidx)), next_input)
  | '\x24' ->
    let globalidx, next_input = deserialize_indice next_input in
    (Some (Global_set (Raw globalidx)), next_input)
  | '\x25' ->
    let tableidx, next_input = deserialize_indice next_input in
    (Some (Table_get (Raw tableidx)), next_input)
  | '\x26' ->
    let tableidx, next_input = deserialize_indice next_input in
    (Some (Table_set (Raw tableidx)), next_input)
  | '\x28' ->
    let memarg, next_input = deserialize_memarg next_input in
    (Some (I_load (S32, memarg)), next_input)
  | '\x29' ->
    let memarg, next_input = deserialize_memarg next_input in
    (Some (I_load (S64, memarg)), next_input)
  | '\x2A' ->
    let memarg, next_input = deserialize_memarg next_input in
    (Some (F_load (S32, memarg)), next_input)
  | '\x2B' ->
    let memarg, next_input = deserialize_memarg next_input in
    (Some (F_load (S64, memarg)), next_input)
  | '\x2C' ->
    let memarg, next_input = deserialize_memarg next_input in
    (Some (I_load8 (S32, S, memarg)), next_input)
  | '\x2D' ->
    let memarg, next_input = deserialize_memarg next_input in
    (Some (I_load8 (S32, U, memarg)), next_input)
  | '\x2E' ->
    let memarg, next_input = deserialize_memarg next_input in
    (Some (I_load16 (S32, S, memarg)), next_input)
  | '\x2F' ->
    let memarg, next_input = deserialize_memarg next_input in
    (Some (I_load16 (S32, U, memarg)), next_input)
  | '\x30' ->
    let memarg, next_input = deserialize_memarg next_input in
    (Some (I_load8 (S64, S, memarg)), next_input)
  | '\x31' ->
    let memarg, next_input = deserialize_memarg next_input in
    (Some (I_load8 (S64, U, memarg)), next_input)
  | '\x32' ->
    let memarg, next_input = deserialize_memarg next_input in
    (Some (I_load16 (S64, S, memarg)), next_input)
  | '\x33' ->
    let memarg, next_input = deserialize_memarg next_input in
    (Some (I_load16 (S64, U, memarg)), next_input)
  | '\x34' ->
    let memarg, next_input = deserialize_memarg next_input in
    (Some (I64_load32 (S, memarg)), next_input)
  | '\x35' ->
    let memarg, next_input = deserialize_memarg next_input in
    (Some (I64_load32 (U, memarg)), next_input)
  | '\x36' ->
    let memarg, next_input = deserialize_memarg next_input in
    (Some (I_store (S32, memarg)), next_input)
  | '\x37' ->
    let memarg, next_input = deserialize_memarg next_input in
    (Some (I_store (S64, memarg)), next_input)
  | '\x38' ->
    let memarg, next_input = deserialize_memarg next_input in
    (Some (F_store (S32, memarg)), next_input)
  | '\x39' ->
    let memarg, next_input = deserialize_memarg next_input in
    (Some (F_store (S64, memarg)), next_input)
  | '\x3A' ->
    let memarg, next_input = deserialize_memarg next_input in
    (Some (I_store8 (S32, memarg)), next_input)
  | '\x3B' ->
    let memarg, next_input = deserialize_memarg next_input in
    (Some (I_store16 (S32, memarg)), next_input)
  | '\x3C' ->
    let memarg, next_input = deserialize_memarg next_input in
    (Some (I_store8 (S64, memarg)), next_input)
  | '\x3D' ->
    let memarg, next_input = deserialize_memarg next_input in
    (Some (I_store16 (S64, memarg)), next_input)
  | '\x3E' ->
    let memarg, next_input = deserialize_memarg next_input in
    (Some (I64_store32 memarg), next_input)
  | '\x3F' ->
    let b, next_input = read_byte next_input in
    begin
      match b with
      | '\x00' -> (Some Memory_size, next_input)
      | _ -> failwith "deserialize memory.size"
    end
  | '\x40' ->
    let b, next_input = read_byte next_input in
    begin
      match b with
      | '\x00' -> (Some Memory_grow, next_input)
      | _ -> failwith "deserialize memory.grow"
    end
  | '\x41' ->
    let i, next_input = read_U32 next_input in
    (Some (I32_const (Int32.of_int i)), next_input)
  | '\x42' ->
    let i, next_input = read_U64 next_input in
    (Some (I64_const (Int64.of_int i)), next_input)
    (* F32_const *)
    (* F64_const *)
  | '\x45' -> (Some (I_testop (S32, Eqz)), next_input)
  | '\x46' -> (Some (I_relop (S32, Eq)), next_input)
  | '\x47' -> (Some (I_relop (S32, Ne)), next_input)
  | '\x48' -> (Some (I_relop (S32, Lt S)), next_input)
  | '\x49' -> (Some (I_relop (S32, Lt U)), next_input)
  | '\x4A' -> (Some (I_relop (S32, Gt S)), next_input)
  | '\x4B' -> (Some (I_relop (S32, Gt U)), next_input)
  | '\x4C' -> (Some (I_relop (S32, Le S)), next_input)
  | '\x4D' -> (Some (I_relop (S32, Le U)), next_input)
  | '\x4E' -> (Some (I_relop (S32, Ge S)), next_input)
  | '\x4F' -> (Some (I_relop (S32, Ge U)), next_input)
  | '\x50' -> (Some (I_testop (S64, Eqz)), next_input)
  | '\x51' -> (Some (I_relop (S64, Eq)), next_input)
  | '\x52' -> (Some (I_relop (S64, Ne)), next_input)
  | '\x53' -> (Some (I_relop (S64, Lt S)), next_input)
  | '\x54' -> (Some (I_relop (S64, Lt U)), next_input)
  | '\x55' -> (Some (I_relop (S64, Gt S)), next_input)
  | '\x56' -> (Some (I_relop (S64, Gt U)), next_input)
  | '\x57' -> (Some (I_relop (S64, Le S)), next_input)
  | '\x58' -> (Some (I_relop (S64, Le U)), next_input)
  | '\x59' -> (Some (I_relop (S64, Ge S)), next_input)
  | '\x5A' -> (Some (I_relop (S64, Ge U)), next_input)
  | '\x5B' -> (Some (F_relop (S32, Eq)), next_input)
  | '\x5C' -> (Some (F_relop (S32, Ne)), next_input)
  | '\x5D' -> (Some (F_relop (S32, Lt)), next_input)
  | '\x5E' -> (Some (F_relop (S32, Gt)), next_input)
  | '\x5F' -> (Some (F_relop (S32, Le)), next_input)
  | '\x60' -> (Some (F_relop (S32, Ge)), next_input)
  | '\x61' -> (Some (F_relop (S64, Eq)), next_input)
  | '\x62' -> (Some (F_relop (S64, Ne)), next_input)
  | '\x63' -> (Some (F_relop (S64, Lt)), next_input)
  | '\x64' -> (Some (F_relop (S64, Gt)), next_input)
  | '\x65' -> (Some (F_relop (S64, Le)), next_input)
  | '\x66' -> (Some (F_relop (S64, Ge)), next_input)
  | '\x67' -> (Some (I_unop (S32, Clz)), next_input)
  | '\x68' -> (Some (I_unop (S32, Ctz)), next_input)
  | '\x69' -> (Some (I_unop (S32, Popcnt)), next_input)
  | '\x6A' -> (Some (I_binop (S32, Add)), next_input)
  | '\x6B' -> (Some (I_binop (S32, Sub)), next_input)
  | '\x6C' -> (Some (I_binop (S32, Mul)), next_input)
  | '\x6D' -> (Some (I_binop (S32, Div S)), next_input)
  | '\x6E' -> (Some (I_binop (S32, Div U)), next_input)
  | '\x6F' -> (Some (I_binop (S32, Rem S)), next_input)
  | '\x70' -> (Some (I_binop (S32, Rem U)), next_input)
  | '\x71' -> (Some (I_binop (S32, And)), next_input)
  | '\x72' -> (Some (I_binop (S32, Or)), next_input)
  | '\x73' -> (Some (I_binop (S32, Xor)), next_input)
  | '\x74' -> (Some (I_binop (S32, Shl)), next_input)
  | '\x75' -> (Some (I_binop (S32, Shr S)), next_input)
  | '\x76' -> (Some (I_binop (S32, Shr U)), next_input)
  | '\x77' -> (Some (I_binop (S32, Rotl)), next_input)
  | '\x78' -> (Some (I_binop (S32, Rotr)), next_input)
  | '\x79' -> (Some (I_unop (S64, Clz)), next_input)
  | '\x7A' -> (Some (I_unop (S64, Ctz)), next_input)
  | '\x7B' -> (Some (I_unop (S64, Popcnt)), next_input)
  | '\x7C' -> (Some (I_binop (S64, Add)), next_input)
  | '\x7D' -> (Some (I_binop (S64, Sub)), next_input)
  | '\x7E' -> (Some (I_binop (S64, Mul)), next_input)
  | '\x7F' -> (Some (I_binop (S64, Div S)), next_input)
  | '\x80' -> (Some (I_binop (S64, Div U)), next_input)
  | '\x81' -> (Some (I_binop (S64, Rem S)), next_input)
  | '\x82' -> (Some (I_binop (S64, Rem U)), next_input)
  | '\x83' -> (Some (I_binop (S64, And)), next_input)
  | '\x84' -> (Some (I_binop (S64, Or)), next_input)
  | '\x85' -> (Some (I_binop (S64, Xor)), next_input)
  | '\x86' -> (Some (I_binop (S64, Shl)), next_input)
  | '\x87' -> (Some (I_binop (S64, Shr S)), next_input)
  | '\x88' -> (Some (I_binop (S64, Shr U)), next_input)
  | '\x89' -> (Some (I_binop (S64, Rotl)), next_input)
  | '\x8A' -> (Some (I_binop (S64, Rotr)), next_input)
  | '\x8B' -> (Some (F_unop (S32, Abs)), next_input)
  | '\x8C' -> (Some (F_unop (S32, Neg)), next_input)
  | '\x8D' -> (Some (F_unop (S32, Ceil)), next_input)
  | '\x8E' -> (Some (F_unop (S32, Floor)), next_input)
  | '\x8F' -> (Some (F_unop (S32, Trunc)), next_input)
  | '\x90' -> (Some (F_unop (S32, Nearest)), next_input)
  | '\x91' -> (Some (F_unop (S32, Sqrt)), next_input)
  | '\x92' -> (Some (F_binop (S32, Add)), next_input)
  | '\x93' -> (Some (F_binop (S32, Sub)), next_input)
  | '\x94' -> (Some (F_binop (S32, Mul)), next_input)
  | '\x95' -> (Some (F_binop (S32, Div)), next_input)
  | '\x96' -> (Some (F_binop (S32, Min)), next_input)
  | '\x97' -> (Some (F_binop (S32, Max)), next_input)
  | '\x98' -> (Some (F_binop (S32, Copysign)), next_input)
  | '\x99' -> (Some (F_unop (S64, Abs)), next_input)
  | '\x9A' -> (Some (F_unop (S64, Neg)), next_input)
  | '\x9B' -> (Some (F_unop (S64, Ceil)), next_input)
  | '\x9C' -> (Some (F_unop (S64, Floor)), next_input)
  | '\x9D' -> (Some (F_unop (S64, Trunc)), next_input)
  | '\x9E' -> (Some (F_unop (S64, Nearest)), next_input)
  | '\x9F' -> (Some (F_unop (S64, Sqrt)), next_input)
  | '\xA0' -> (Some (F_binop (S64, Add)), next_input)
  | '\xA1' -> (Some (F_binop (S64, Sub)), next_input)
  | '\xA2' -> (Some (F_binop (S64, Mul)), next_input)
  | '\xA3' -> (Some (F_binop (S64, Div)), next_input)
  | '\xA4' -> (Some (F_binop (S64, Min)), next_input)
  | '\xA5' -> (Some (F_binop (S64, Max)), next_input)
  | '\xA6' -> (Some (F_binop (S64, Copysign)), next_input)
  | '\xA7' -> (Some I32_wrap_i64, next_input)
  | '\xA8' -> (Some (I_trunc_f (S32, S32, S)), next_input)
  | '\xA9' -> (Some (I_trunc_f (S32, S32, U)), next_input)
  | '\xAA' -> (Some (I_trunc_f (S32, S64, S)), next_input)
  | '\xAB' -> (Some (I_trunc_f (S32, S64, U)), next_input)
  | '\xAC' -> (Some (I64_extend_i32 S), next_input)
  | '\xAD' -> (Some (I64_extend_i32 U), next_input)
  | '\xAE' -> (Some (I_trunc_f (S64, S32, S)), next_input)
  | '\xAF' -> (Some (I_trunc_f (S64, S32, U)), next_input)
  | '\xB0' -> (Some (I_trunc_f (S64, S64, S)), next_input)
  | '\xB1' -> (Some (I_trunc_f (S64, S64, U)), next_input)
  | '\xB2' -> (Some (F_convert_i (S32, S32, S)), next_input)
  | '\xB3' -> (Some (F_convert_i (S32, S32, U)), next_input)
  | '\xB4' -> (Some (F_convert_i (S32, S64, S)), next_input)
  | '\xB5' -> (Some (F_convert_i (S32, S64, U)), next_input)
  | '\xB6' -> (Some F32_demote_f64, next_input)
  | '\xB7' -> (Some (F_convert_i (S64, S32, S)), next_input)
  | '\xB8' -> (Some (F_convert_i (S64, S32, U)), next_input)
  | '\xB9' -> (Some (F_convert_i (S64, S64, S)), next_input)
  | '\xBA' -> (Some (F_convert_i (S64, S64, U)), next_input)
  | '\xBB' -> (Some F64_promote_f32, next_input)
  | '\xBC' -> (Some (I_reinterpret_f (S32, S32)), next_input)
  | '\xBD' -> (Some (I_reinterpret_f (S64, S64)), next_input)
  | '\xBE' -> (Some (F_reinterpret_i (S32, S32)), next_input)
  | '\xBF' -> (Some (F_reinterpret_i (S64, S64)), next_input)
  | '\xC0' -> (Some (I_extend8_s S32), next_input)
  | '\xC1' -> (Some (I_extend16_s S32), next_input)
  | '\xC2' -> (Some (I_extend8_s S64), next_input)
  | '\xC3' -> (Some (I_extend16_s S64), next_input)
  | '\xC4' -> (Some I64_extend32_s, next_input)
  | '\xFC' -> deserialize_FC input
  | _ -> failwith ("deserialize_instr: code=" ^ Int.to_string (Char.code b))

let rec deserialize_code input instr_list =
  let b, next_input = read_byte input in
  match b with
  | '\x0B' -> (List.rev instr_list, next_input)
  | _ ->
    let instr, input = deserialize_instr input in
    let instr_list, input =
      match instr with
      | None -> (instr_list, input)
      | Some instr -> (instr :: instr_list, input)
    in
    deserialize_code input instr_list
