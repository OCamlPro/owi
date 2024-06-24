(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module Expr = Smtml.Expr
module Choice = Concolic.P.Choice

(* The constraint is used here to make sure we don't forget to define one of the expected FFI functions, this whole file is further constrained such that if one function of M is unused in the FFI module below, an error will be displayed *)
module M :
  Wasm_ffi_intf.S0
    with type 'a t = 'a Choice.t
     and module Value = Concolic_value.V = struct
  type 'a t = 'a Choice.t

  module Value = Concolic_value.V

  let symbol_i32 () : Value.int32 Choice.t =
    Choice.with_new_symbol (Ty_bitv 32) (fun sym forced_value ->
        let n =
          match forced_value with
          | None -> Random.bits32 ()
          | Some (Num (I32 n)) -> n
          | _ -> assert false
        in
        (I32 n, Value.pair n (Expr.mk_symbol sym)) )

  let symbol_i8 () : Value.int32 Choice.t =
    Choice.with_new_symbol (Ty_bitv 32) (fun sym forced_value ->
        let n =
          match forced_value with
          | None -> Int32.logand 0xFFl (Random.bits32 ())
          | Some (Num (I32 n)) -> n
          | _ -> assert false
        in
        let sym_expr =
          Expr.make (Cvtop (Ty_bitv 32, Zero_extend 24, Expr.mk_symbol sym))
        in
        (I32 n, Value.pair n sym_expr) )

  let symbol_i64 () : Value.int64 Choice.t =
    Choice.with_new_symbol (Ty_bitv 64) (fun sym forced_value ->
        let n =
          match forced_value with
          | None -> Random.bits64 ()
          | Some (Num (I64 n)) -> n
          | _ -> assert false
        in
        (I64 n, Value.pair n (Expr.mk_symbol sym)) )

  let symbol_f32 () : Value.float32 Choice.t =
    Choice.with_new_symbol (Ty_fp 32) (fun sym forced_value ->
        let n =
          match forced_value with
          | None -> Random.bits32 ()
          | Some (Num (F32 n)) -> n
          | _ -> assert false
        in
        let n = Float32.of_bits n in
        (F32 n, Value.pair n (Expr.mk_symbol sym)) )

  let symbol_f64 () : Value.float64 Choice.t =
    Choice.with_new_symbol (Ty_fp 64) (fun sym forced_value ->
        let n =
          match forced_value with
          | None -> Random.bits64 ()
          | Some (Num (F64 n)) -> n
          | _ -> assert false
        in
        let n = Float64.of_bits n in
        (F64 n, Value.pair n (Expr.mk_symbol sym)) )

  let assume_i32 (i : Value.int32) : unit Choice.t =
    let c = Value.I32.to_bool i in
    Concolic_choice.assume c

  let assume_positive_i32 (i : Value.int32) : unit Choice.t =
    let c = Value.I32.ge i Value.I32.zero in
    Concolic_choice.assume c

  let assert_i32 (i : Value.int32) : unit Choice.t =
    let c = Value.I32.to_bool i in
    Concolic_choice.assertion c

  open Expr

  let abort () : unit Choice.t = Choice.abort

  let i32 (v : Value.int32) : int32 Choice.t =
    (* TODO: select_i32 ? *)
    (* let+ v = Choice.select_i32 v in *)
    (* let n = v.c in *)
    (* let x = Choice.assume (Value.I32.eq v (Value.const_i32 n)) in *)
    match view v.symbolic with
    | Val (Num (I32 v)) -> Choice.return v
    | _ ->
      Log.debug2 {|alloc: cannot allocate base pointer "%a"|} Expr.pp v.symbolic;
      Choice.bind (abort ()) (fun () -> assert false)

  let ptr (v : Value.int32) : int32 Choice.t =
    match view v.symbolic with
    | Ptr { base; _ } -> Choice.return base
    | _ ->
      Log.debug2 {|free: cannot fetch pointer base of "%a"|} Expr.pp v.symbolic;
      Choice.bind (abort ()) (fun () -> assert false)

  let exit (p : Value.int32) : unit Choice.t =
    ignore p;
    abort ()

  let alloc (base : Value.int32) (_size : Value.int32) : Value.int32 Choice.t =
    Choice.bind (i32 base) (fun (base : int32) ->
        Choice.return
          { Concolic_value.concrete = base
          ; symbolic = Expr.ptr base (Symbolic_value.const_i32 0l)
          } )
  (* WHAT ???? *)
  (* Choice.with_thread (fun t : Value.int32 -> *)
  (*     let memories = t.shared.memories in *)
  (*     Symbolic_memory.iter *)
  (*       (fun tbl -> *)
  (*         Symbolic_memory.ITbl.iter *)
  (*           (fun _ (m : Symbolic_memory.t) -> *)
  (*             Symbolic_memory.replace_size m base size.s ) *)
  (*           tbl ) *)
  (*       memories; *)
  (*     { c = base; s = Expr.make (Ptr (base, Symbolic_value.const_i32 0l)) }) *)

  let free (p : Value.int32) : unit Choice.t =
    (* WHAT ???? *)
    let _base = ptr p in
    (* Choice.with_thread (fun t -> *)
    (*     let memories = t.shared.memories in *)
    (*     Symbolic_memory.iter *)
    (*       (fun tbl -> *)
    (*         Symbolic_memory.ITbl.iter *)
    (*           (fun _ (m : Symbolic_memory.t) -> Symbolic_memory.free m base) *)
    (*           tbl ) *)
    (*       memories ) *)
    Choice.return ()
end

type extern_func = Concolic.P.Extern_func.extern_func

open M

let symbolic_extern_module =
  let functions =
    [ ( "i8_symbol"
      , Concolic.P.Extern_func.Extern_func (Func (UArg Res, R1 I32), symbol_i8)
      )
    ; ( "i32_symbol"
      , Concolic.P.Extern_func.Extern_func (Func (UArg Res, R1 I32), symbol_i32)
      )
    ; ( "i64_symbol"
      , Concolic.P.Extern_func.Extern_func (Func (UArg Res, R1 I64), symbol_i64)
      )
    ; ( "f32_symbol"
      , Concolic.P.Extern_func.Extern_func (Func (UArg Res, R1 F32), symbol_f32)
      )
    ; ( "f64_symbol"
      , Concolic.P.Extern_func.Extern_func (Func (UArg Res, R1 F64), symbol_f64)
      )
    ; ( "assume"
      , Concolic.P.Extern_func.Extern_func
          (Func (Arg (I32, Res), R0), assume_i32) )
    ; ( "assume_positive_i32"
      , Concolic.P.Extern_func.Extern_func
          (Func (Arg (I32, Res), R0), assume_positive_i32) )
    ; ( "assert"
      , Concolic.P.Extern_func.Extern_func
          (Func (Arg (I32, Res), R0), assert_i32) )
    ]
  in
  { Link.functions }

let summaries_extern_module =
  let functions =
    [ ( "alloc"
      , Concolic.P.Extern_func.Extern_func
          (Func (Arg (I32, Arg (I32, Res)), R1 I32), alloc) )
    ; ( "dealloc"
      , Concolic.P.Extern_func.Extern_func (Func (Arg (I32, Res), R0), free) )
    ; ("abort", Concolic.P.Extern_func.Extern_func (Func (UArg Res, R0), abort))
    ]
  in
  { Link.functions }
