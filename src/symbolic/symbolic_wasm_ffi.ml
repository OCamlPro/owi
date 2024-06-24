(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module Expr = Smtml.Expr
module Choice = Symbolic.P.Choice

(* The constraint is used here to make sure we don't forget to define one of the expected FFI functions, this whole file is further constrained such that if one function of M is unused in the FFI module below, an error will be displayed *)
module M :
  Wasm_ffi_intf.S0
    with type 'a t = 'a Choice.t
     and module Value = Symbolic_value = struct
  type 'a t = 'a Choice.t

  module Value = Symbolic_value

  let sym_cnt = Atomic.make 0

  let symbol ty () : Value.int32 Choice.t =
    let id = Atomic.fetch_and_add sym_cnt 1 in
    let sym = Format.kasprintf (Smtml.Symbol.make ty) "symbol_%d" id in
    let sym_expr = Expr.mk_symbol sym in
    Choice.with_thread (fun thread ->
        Thread.add_symbol thread sym;
        match ty with
        | Ty_bitv 8 -> Expr.make (Cvtop (Ty_bitv 32, Zero_extend 24, sym_expr))
        | _ -> sym_expr )

  let assume_i32 (i : Value.int32) : unit Choice.t =
    let c = Value.I32.to_bool i in
    Choice.add_pc c

  let assume_positive_i32 (i : Value.int32) : unit Choice.t =
    let c = Value.I32.ge i Value.I32.zero in
    Choice.add_pc c

  let assert_i32 (i : Value.int32) : unit Choice.t =
    let c = Value.I32.to_bool i in
    Choice.assertion c

  let symbol_i8 = symbol (Ty_bitv 8)

  let symbol_i32 = symbol (Ty_bitv 32)

  let symbol_i64 = symbol (Ty_bitv 64)

  let symbol_f32 = symbol (Ty_fp 32)

  let symbol_f64 = symbol (Ty_fp 64)

  open Expr

  let abort () : unit Choice.t = Choice.add_pc @@ Value.Bool.const false

  let i32 v : int32 Choice.t =
    match view v with
    | Val (Num (I32 v)) -> Choice.return v
    | _ ->
      Log.debug2 {|alloc: cannot allocate base pointer "%a"|} Expr.pp v;
      Choice.bind (abort ()) (fun () -> assert false)

  let ptr v : int32 Choice.t =
    match view v with
    | Ptr { base; _ } -> Choice.return base
    | _ ->
      Log.debug2 {|free: cannot fetch pointer base of "%a"|} Expr.pp v;
      Choice.bind (abort ()) (fun () -> assert false)

  let alloc (base : Value.int32) (size : Value.int32) : Value.int32 Choice.t =
    Choice.bind (i32 base) (fun base ->
        Choice.with_thread (fun t ->
            let memories = Thread.memories t in
            Symbolic_memory.iter
              (fun tbl ->
                Symbolic_memory.ITbl.iter
                  (fun _ (m : Symbolic_memory.t) ->
                    Symbolic_memory.replace_size m base size )
                  tbl )
              memories;
            Expr.ptr base (Value.const_i32 0l) ) )

  let free (p : Value.int32) : unit Choice.t =
    Choice.bind (ptr p) (fun base ->
        Choice.with_thread (fun t ->
            let memories = Thread.memories t in
            Symbolic_memory.iter
              (fun tbl ->
                Symbolic_memory.ITbl.iter
                  (fun _ (m : Symbolic_memory.t) -> Symbolic_memory.free m base)
                  tbl )
              memories ) )

  let exit (p : Value.int32) : unit Choice.t =
    ignore p;
    abort ()
end

type extern_func = Symbolic.P.Extern_func.extern_func

open M

let symbolic_extern_module =
  let functions =
    [ ( "i8_symbol"
      , Symbolic.P.Extern_func.Extern_func (Func (UArg Res, R1 I32), symbol_i8)
      )
    ; ( "i32_symbol"
      , Symbolic.P.Extern_func.Extern_func (Func (UArg Res, R1 I32), symbol_i32)
      )
    ; ( "i64_symbol"
      , Symbolic.P.Extern_func.Extern_func (Func (UArg Res, R1 I64), symbol_i64)
      )
    ; ( "f32_symbol"
      , Symbolic.P.Extern_func.Extern_func (Func (UArg Res, R1 F32), symbol_f32)
      )
    ; ( "f64_symbol"
      , Symbolic.P.Extern_func.Extern_func (Func (UArg Res, R1 F64), symbol_f64)
      )
    ; ( "assume"
      , Symbolic.P.Extern_func.Extern_func
          (Func (Arg (I32, Res), R0), assume_i32) )
    ; ( "assume_positive_i32"
      , Symbolic.P.Extern_func.Extern_func
          (Func (Arg (I32, Res), R0), assume_positive_i32) )
    ; ( "assert"
      , Symbolic.P.Extern_func.Extern_func
          (Func (Arg (I32, Res), R0), assert_i32) )
    ]
  in
  { Link.functions }

let summaries_extern_module =
  let functions =
    [ ( "alloc"
      , Symbolic.P.Extern_func.Extern_func
          (Func (Arg (I32, Arg (I32, Res)), R1 I32), alloc) )
    ; ( "dealloc"
      , Symbolic.P.Extern_func.Extern_func (Func (Arg (I32, Res), R0), free) )
    ; ("abort", Symbolic.P.Extern_func.Extern_func (Func (UArg Res, R0), abort))
    ; ( "exit"
      , Symbolic.P.Extern_func.Extern_func (Func (Arg (I32, Res), R0), exit) )
    ]
  in
  { Link.functions }
