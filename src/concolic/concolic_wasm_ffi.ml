(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module Expr = Smtml.Expr

(* The constraint is used here to make sure we don't forget to define one of the expected FFI functions, this whole file is further constrained such that if one function of M is unused in the FFI module below, an error will be displayed *)
module M :
  Wasm_ffi_intf.S0
    with type 'a t = 'a Concolic_choice.t
     and type memory = Concolic_memory.t
     and module Value = Concolic_value = struct
  type 'a t = 'a Concolic_choice.t

  type memory = Concolic_memory.t

  module Value = Concolic_value

  let symbol_i32 () : Value.int32 Concolic_choice.t =
    Concolic_choice.with_new_symbol (Ty_bitv 32) (fun sym forced_value ->
      let n =
        match forced_value with
        | None -> Random.bits32 ()
        | Some (Num (I32 n)) -> n
        | _ -> assert false
      in
      (I32 n, (n, Expr.symbol sym)) )

  let symbol_i8 () : Value.int32 Concolic_choice.t =
    Concolic_choice.with_new_symbol (Ty_bitv 32) (fun sym forced_value ->
      let n =
        match forced_value with
        | None -> Int32.logand 0xFFl (Random.bits32 ())
        | Some (Num (I32 n)) -> n
        | _ -> assert false
      in
      let sym_expr =
        Expr.make (Cvtop (Ty_bitv 32, Zero_extend 24, Expr.symbol sym))
      in
      (I32 n, (n, sym_expr)) )

  let symbol_char = symbol_i8

  let symbol_i64 () : Value.int64 Concolic_choice.t =
    Concolic_choice.with_new_symbol (Ty_bitv 64) (fun sym forced_value ->
      let n =
        match forced_value with
        | None -> Random.bits64 ()
        | Some (Num (I64 n)) -> n
        | _ -> assert false
      in
      (I64 n, (n, Expr.symbol sym)) )

  let symbol_f32 () : Value.float32 Concolic_choice.t =
    Concolic_choice.with_new_symbol (Ty_fp 32) (fun sym forced_value ->
      let n =
        match forced_value with
        | None -> Random.bits32 ()
        | Some (Num (F32 n)) -> n
        | _ -> assert false
      in
      let n = Float32.of_bits n in
      (F32 n, (n, Expr.symbol sym)) )

  let symbol_f64 () : Value.float64 Concolic_choice.t =
    Concolic_choice.with_new_symbol (Ty_fp 64) (fun sym forced_value ->
      let n =
        match forced_value with
        | None -> Random.bits64 ()
        | Some (Num (F64 n)) -> n
        | _ -> assert false
      in
      let n = Float64.of_bits n in
      (F64 n, (n, Expr.symbol sym)) )

  let symbol_bool () : Value.int32 Concolic_choice.t =
    Concolic_choice.with_new_symbol Ty_bool (fun sym forced_value ->
      let b =
        match forced_value with
        | None -> Random.bool ()
        | Some True -> true
        | Some False -> false
        | _ -> assert false
      in
      let n = Concrete_value.Bool.int32 b in
      (I32 n, Value.Bool.int32 (b, Expr.symbol sym)) )

  let add_pc_wrapper e = Concolic_choice.assume e

  let assume (i : Value.int32) : unit Concolic_choice.t =
    let c = Value.I32.to_bool i in
    add_pc_wrapper c

  let assert' (i : Value.int32) : unit Concolic_choice.t =
    let c = Value.I32.to_bool i in
    Concolic_choice.assertion c

  let symbol_range (lo : Value.int32) (hi : Value.int32) :
    Value.int32 Concolic_choice.t =
    let open Concolic_choice in
    let* x = symbol_i32 () in
    let* () = add_pc_wrapper (Value.I32.le lo x) in
    let+ () = add_pc_wrapper (Value.I32.gt hi x) in
    x

  open Expr

  let abort () : unit Concolic_choice.t = Concolic_choice.abort

  let i32 ((_c, s) : Value.int32) : int32 Concolic_choice.t =
    (* TODO: select_i32 ? *)
    (* let+ v = Concolic_choice.select_i32 v in *)
    (* let n = v.c in *)
    (* let x = Concolic_choice.assume (Value.I32.eq v (Value.const_i32 n)) in *)
    match view s with
    | Val (Num (I32 v)) -> Concolic_choice.return v
    | _ ->
      Log.debug2 {|alloc: cannot allocate base pointer "%a"@.|} Expr.pp s;
      Concolic_choice.bind (abort ()) (fun () -> assert false)

  let ptr ((_c, s) : Value.int32) : int32 Concolic_choice.t =
    match view s with
    | Ptr { base; _ } -> Concolic_choice.return base
    | _ ->
      Log.debug2 {|free: cannot fetch pointer base of "%a"@.|} Expr.pp s;
      Concolic_choice.bind (abort ()) (fun () -> assert false)

  let exit (_p : Value.int32) : unit Concolic_choice.t = abort ()

  let alloc _ (base : Value.int32) (_size : Value.int32) :
    Value.int32 Concolic_choice.t =
    Concolic_choice.bind (i32 base) (fun (base : int32) ->
      Concolic_choice.return (base, Expr.ptr base (Symbolic_value.const_i32 0l)) )

  let free _ (p : Value.int32) =
    (* WHAT ???? *)
    let open Concolic_choice in
    let+ base = ptr p in
    Value.const_i32 base

  let in_replay_mode () =
    Concolic_choice.return (0l, Smtml.Expr.value (Smtml.Value.Int 1))

  let print_char ((c, _s) : Value.int32) =
    Concolic_choice.return @@ Fmt.pr "%c" (char_of_int (Int32.to_int c))
end

type extern_func = Concolic.Extern_func.extern_func

open M

let symbolic_extern_module =
  let functions =
    [ ( "i8_symbol"
      , Concolic.Extern_func.Extern_func (Func (UArg Res, R1 I32), symbol_i8) )
    ; ( "i32_symbol"
      , Concolic.Extern_func.Extern_func (Func (UArg Res, R1 I32), symbol_i32)
      )
    ; ( "i64_symbol"
      , Concolic.Extern_func.Extern_func (Func (UArg Res, R1 I64), symbol_i64)
      )
    ; ( "f32_symbol"
      , Concolic.Extern_func.Extern_func (Func (UArg Res, R1 F32), symbol_f32)
      )
    ; ( "f64_symbol"
      , Concolic.Extern_func.Extern_func (Func (UArg Res, R1 F64), symbol_f64)
      )
    ; ( "bool_symbol"
      , Concolic.Extern_func.Extern_func (Func (UArg Res, R1 I32), symbol_bool)
      )
    ; ( "range_symbol"
      , Concolic.Extern_func.Extern_func
          (Func (Arg (I32, Arg (I32, Res)), R1 I32), symbol_range) )
    ; ( "assume"
      , Concolic.Extern_func.Extern_func (Func (Arg (I32, Res), R0), assume) )
    ; ( "assert"
      , Concolic.Extern_func.Extern_func (Func (Arg (I32, Res), R0), assert') )
    ; ( "in_replay_mode"
      , Concolic.Extern_func.Extern_func
          (Func (UArg Res, R1 I32), in_replay_mode) )
    ; ( "print_char"
      , Concolic.Extern_func.Extern_func (Func (Arg (I32, Res), R0), print_char)
      )
    ]
  in
  { Link.functions }

let summaries_extern_module =
  let functions =
    [ ( "alloc"
      , Concolic.Extern_func.Extern_func
          (Func (Mem (Arg (I32, Arg (I32, Res))), R1 I32), alloc) )
    ; ( "dealloc"
      , Concolic.Extern_func.Extern_func
          (Func (Mem (Arg (I32, Res)), R1 I32), free) )
    ; ("abort", Concolic.Extern_func.Extern_func (Func (UArg Res, R0), abort))
    ]
  in
  { Link.functions }
