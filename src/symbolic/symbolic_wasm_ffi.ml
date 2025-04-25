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

  let add_pc_wrapper e = Choice.add_pc e

  let assume (i : Value.int32) : unit Choice.t =
    add_pc_wrapper @@ Value.I32.to_bool i

  let assert' (i : Value.int32) : unit Choice.t =
    Choice.assertion @@ Value.I32.to_bool i

  let symbol_bool () =
    Choice.with_new_symbol (Ty_bitv 1) (fun sym ->
      Expr.cvtop (Ty_bitv 32) (Zero_extend 31) (Expr.symbol sym) )

  let symbol_i8 () =
    Choice.with_new_symbol (Ty_bitv 8) (fun sym ->
      Expr.make (Cvtop (Ty_bitv 32, Zero_extend 24, Expr.symbol sym)) )

  let symbol_char = symbol_i8

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

  let label m (id : Value.int32) (str_ptr : Value.int32) =
    let open Choice in
    let rec make_str accu i =
      let* p = Memory.load_8_u m (Expr.value (Num (I32 i))) in
      match Smtml.Expr.view p with
      | Val (Num (I32 c)) ->
        let int = Int32.to_int c in
        if int > 255 || int < 0 then assert false
        else
          let ch = char_of_int int in
          if Char.equal ch '\x00' then return (List.rev accu |> Array.of_list)
          else make_str (ch :: accu) (Int32.add i (Int32.of_int 1))
      | _ -> assert false
    in
    let ptr =
      match Smtml.Expr.view str_ptr with
      | Val (Num (I32 n)) -> n
      | _ -> assert false
    in
    let* chars = make_str [] ptr in
    let str = String.init (Array.length chars) (Array.get chars) in

    match Smtml.Expr.view id with
    | Val (Num (I32 c)) ->
      let* () = add_label (Int32.to_int c, str) in
      return ()
    | _ -> assert false
end

type extern_func = Symbolic.Extern_func.extern_func

open M

let symbolic_extern_module =
  let functions =
    [ ( "i8_symbol"
      , Symbolic.Extern_func.Extern_func (Func (UArg Res, R1 I32), symbol_i8) )
    ; ( "char_symbol"
      , Symbolic.Extern_func.Extern_func (Func (UArg Res, R1 I32), symbol_char)
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
    ; ( "bool_symbol"
      , Symbolic.Extern_func.Extern_func (Func (UArg Res, R1 I32), symbol_bool)
      )
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
    ; ( "label"
      , Symbolic.Extern_func.Extern_func
          (Func (Mem (Arg (I32, Arg (I32, Res))), R0), label) )
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
