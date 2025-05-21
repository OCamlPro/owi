(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module Expr = Smtml.Expr
module Choice = Symbolic_choice_with_memory
module Memory = Symbolic.Memory

(* The constraint is used here to make sure we don't forget to define one of the expected FFI functions, this whole file is further constrained such that if one function of M is unused in the FFI module below, an error will be displayed *)
module M : sig
  include
    Wasm_ffi_intf.S0
      with type 'a t = 'a Choice.t
       and type memory = Memory.t
       and module Value = Symbolic_value

  val symbol_i32_constant : Value.int32 -> Value.int32 t
end = struct
  type 'a t = 'a Choice.t

  type memory = Memory.t

  module Value = Symbolic_value

  let covered_labels = Hashtbl.create 16

  let cov_lock = Mutex.create ()

  let add_pc_wrapper e = Choice.add_pc e

  let assume (i : Value.int32) : unit Choice.t =
    add_pc_wrapper @@ Value.I32.to_bool i

  let assert' (i : Value.int32) : unit Choice.t =
    Choice.assertion @@ Value.I32.to_bool i

  let symbol_bool () =
    Choice.with_new_symbol (Ty_bitv 1) (fun sym ->
      Expr.cvtop (Ty_bitv 32) (Zero_extend 31) (Expr.symbol sym) )

  let symbol_invisible_bool () =
    Choice.with_new_invisible_symbol (Ty_bitv 1) (fun sym ->
      Expr.cvtop (Ty_bitv 32) (Zero_extend 31) (Expr.symbol sym) )

  let symbol_i8 () =
    Choice.with_new_symbol (Ty_bitv 8) (fun sym ->
      Expr.cvtop (Ty_bitv 32) (Zero_extend 24) (Expr.symbol sym) )

  let symbol_i16 () =
    Choice.with_new_symbol (Ty_bitv 16) (fun sym ->
      Expr.cvtop (Ty_bitv 32) (Zero_extend 16) (Expr.symbol sym) )

  let symbol_i32 () = Choice.with_new_symbol (Ty_bitv 32) Expr.symbol

  let symbol_i32_constant v =
    let open Choice in
    let* s = Choice.with_new_symbol (Ty_bitv 32) Expr.symbol in
    let eq = Value.I32.eq v s in
    let+ () = Choice.add_pc eq in
    s

  let symbol_i64 () = Choice.with_new_symbol (Ty_bitv 64) Expr.symbol

  let symbol_f32 () = Choice.with_new_symbol (Ty_fp 32) Expr.symbol

  let symbol_f64 () = Choice.with_new_symbol (Ty_fp 64) Expr.symbol

  let symbol_v128 () = Choice.with_new_symbol (Ty_bitv 128) Expr.symbol

  let symbol_range (lo : Value.int32) (hi : Value.int32) =
    let open Choice in
    let* x = symbol_i32 () in
    let* () = add_pc_wrapper (Value.I32.le lo x) in
    let+ () = add_pc_wrapper (Value.I32.gt hi x) in
    x

  let abort () : unit Choice.t = Choice.stop

  let alloc m (base : Value.int32) (size : Value.int32) : Value.int32 Choice.t =
    Choice.lift_mem @@ Memory.realloc m ~ptr:base ~size

  let free m (ptr : Value.int32) : Value.int32 Choice.t =
    Choice.lift_mem @@ Memory.free m ptr

  let exit (_p : Value.int32) : unit Choice.t = abort ()

  let in_replay_mode () = Choice.return @@ Smtml.Expr.value (Smtml.Value.Int 0)

  let print_char (c : Value.int32) =
    let open Choice in
    let* c = select_i32 c in
    Logs.app (fun m -> m "%c@?" (char_of_int (Int32.to_int c)));
    return ()

  let rec make_str m accu i =
    let open Choice in
    let* p = Memory.load_8_u m (Value.const_i32 i) in
    match Smtml.Expr.view p with
    | Val (Bitv bv) when Smtml.Bitvector.numbits bv = 32 ->
      let c = Smtml.Bitvector.to_int32 bv in
      if Int32.gt c 255l || Int32.lt c 0l then trap `Invalid_character_in_memory
      else
        let ch = char_of_int (Int32.to_int c) in
        if Char.equal ch '\x00' then return (List.rev accu |> Array.of_list)
        else make_str m (ch :: accu) (Int32.add i (Int32.of_int 1))
    | _ -> assert false

  let cov_label_is_covered id =
    let open Choice in
    let* id = select_i32 id in
    return @@ Value.const_i32
    @@ Mutex.protect cov_lock (fun () ->
         if Hashtbl.mem covered_labels id then 1l else 0l )

  let cov_label_set m id ptr =
    let open Choice in
    let id = Smtml.Expr.simplify id in
    let ptr = Smtml.Expr.simplify ptr in
    match (Smtml.Expr.view id, Smtml.Expr.view ptr) with
    | Val (Bitv id), Val (Bitv ptr)
      when Smtml.Bitvector.numbits id = 32 && Smtml.Bitvector.numbits ptr = 32
      ->
      let id = Smtml.Bitvector.to_int32 id in
      let ptr = Smtml.Bitvector.to_int32 ptr in
      Mutex.protect cov_lock (fun () ->
        if Hashtbl.mem covered_labels id then abort ()
        else
          let* chars = make_str m [] ptr in
          let str = String.init (Array.length chars) (Array.get chars) in
          Hashtbl.add covered_labels id str;
          let* () = add_label (Int32.to_int id, str) in
          return () )
    | _ ->
      Logs.err (fun m ->
        m "cov_label_set: invalid type id:%a ptr:%a" Smtml.Expr.pp id
          Smtml.Expr.pp ptr );
      assert false

  let open_scope m ptr =
    let open Choice in
    let* ptr = select_i32 ptr in
    let* chars = make_str m [] ptr in
    let str = String.init (Array.length chars) (Array.get chars) in
    open_scope str

  let close_scope = Choice.close_scope
end

type extern_func = Symbolic.Extern_func.extern_func

open M

let symbolic_extern_module =
  let functions =
    [ ( "i8_symbol"
      , Symbolic.Extern_func.Extern_func (Func (UArg Res, R1 I32), symbol_i8) )
    ; ( "i16_symbol"
      , Symbolic.Extern_func.Extern_func (Func (UArg Res, R1 I32), symbol_i16)
      )
    ; ( "i32_symbol"
      , Symbolic.Extern_func.Extern_func (Func (UArg Res, R1 I32), symbol_i32)
      )
    ; ( "i32_symbol_constant"
      , Symbolic.Extern_func.Extern_func
          (Func (Arg (I32, Res), R1 I32), symbol_i32_constant) )
    ; ( "i64_symbol"
      , Symbolic.Extern_func.Extern_func (Func (UArg Res, R1 I64), symbol_i64)
      )
    ; ( "f32_symbol"
      , Symbolic.Extern_func.Extern_func (Func (UArg Res, R1 F32), symbol_f32)
      )
    ; ( "f64_symbol"
      , Symbolic.Extern_func.Extern_func (Func (UArg Res, R1 F64), symbol_f64)
      )
    ; ( "v128_symbol"
      , Symbolic.Extern_func.Extern_func (Func (UArg Res, R1 V128), symbol_v128)
      )
    ; ( "bool_symbol"
      , Symbolic.Extern_func.Extern_func (Func (UArg Res, R1 I32), symbol_bool)
      )
    ; ( "invisible_bool_symbol"
      , Symbolic.Extern_func.Extern_func
          (Func (UArg Res, R1 I32), symbol_invisible_bool) )
    ; ( "range_symbol"
      , Symbolic.Extern_func.Extern_func
          (Func (Arg (I32, Arg (I32, Res)), R1 I32), symbol_range) )
    ; ( "assume"
      , Symbolic.Extern_func.Extern_func (Func (Arg (I32, Res), R0), assume) )
    ; ( "assert"
      , Symbolic.Extern_func.Extern_func (Func (Arg (I32, Res), R0), assert') )
    ; ( "in_replay_mode"
      , Symbolic.Extern_func.Extern_func
          (Func (UArg Res, R1 I32), in_replay_mode) )
    ; ( "print_char"
      , Symbolic.Extern_func.Extern_func (Func (Arg (I32, Res), R0), print_char)
      )
    ; ( "cov_label_set"
      , Symbolic.Extern_func.Extern_func
          (Func (Mem (Arg (I32, Arg (I32, Res))), R0), cov_label_set) )
    ; ( "cov_label_is_covered"
      , Symbolic.Extern_func.Extern_func
          (Func (Arg (I32, Res), R1 I32), cov_label_is_covered) )
    ; ( "open_scope"
      , Symbolic.Extern_func.Extern_func
          (Func (Mem (Arg (I32, Res)), R0), open_scope) )
    ; ( "close_scope"
      , Symbolic.Extern_func.Extern_func (Func (UArg Res, R0), close_scope) )
    ]
  in
  { Link.functions }

let summaries_extern_module =
  let functions =
    [ ( "alloc"
      , Symbolic.Extern_func.Extern_func
          (Func (Mem (Arg (I32, Arg (I32, Res))), R1 I32), alloc) )
    ; ( "dealloc"
      , Symbolic.Extern_func.Extern_func
          (Func (Mem (Arg (I32, Res)), R1 I32), free) )
    ; ("abort", Symbolic.Extern_func.Extern_func (Func (UArg Res, R0), abort))
    ; ( "exit"
      , Symbolic.Extern_func.Extern_func (Func (Arg (I32, Res), R0), exit) )
    ]
  in
  { Link.functions }
