(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module Expr = Smtml.Expr
module Choice = Symbolic_choice_with_memory
module Memory = Symbolic.Memory

(* The constraint is used here to make sure we don't forget to define one of the expected FFI functions, this whole file is further constrained such that if one function of M is unused in the FFI module below, an error will be displayed *)
module M :
  Wasm_ffi_intf.S0
    with type 'a t = 'a Choice.t
     and type memory = Memory.t
     and module Value = Symbolic_value = struct
  type 'a t = 'a Choice.t

  type memory = Memory.t

  module Value = Symbolic_value

  let covered_labels = Hashtbl.create 16

  let cov_lock = Mutex.create ()

  let in_seacoral_store =
    let seacoral_store =
      Option.bind (Bos.OS.Env.var "SC_STORE_MAP_FILE") @@ fun path ->
      let seacoral_size =
        match Bos.OS.Env.var "SC_LABEL_COUNT" with
        | Some s -> begin
          match int_of_string_opt s with
          | Some i -> i
          | None -> Fmt.failwith "SC_LABEL_COUNT should be a number"
        end
        | None ->
          Fmt.failwith "SC_STORE_MAP_FILE specified but not SC_LABEL_COUNT"
      in
      (* we should only read from this fd, but we have to open it in O_RDWR
         because map_file needs it for resizing the array to fit seacoral_size elements *)
      let fd = Unix.openfile path [ O_RDWR ] 0o600 in
      at_exit (fun () -> Unix.close fd);
      let res =
        Unix.map_file fd Int8_unsigned Bigarray.c_layout true
          [| seacoral_size |]
      in
      Log.app (fun m -> m "SC_STORE_MAP_FILE successfully mapped : %s" path);
      Some (fd, res)
    in
    fun i ->
      match seacoral_store with
      | None -> false
      | Some (fd, arr) -> (
        try
          Unix.lockf fd F_RLOCK 0;
          let lbl_status = Bigarray.Genarray.get arr [| Int32.to_int i |] in
          Unix.lockf fd F_ULOCK 0;
          lbl_status <> 0
        with Invalid_argument _ -> false )

  let assume (i : Value.int32) : unit Choice.t =
    Choice.assume @@ Value.I32.to_bool i

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

  let symbol_i64 () = Choice.with_new_symbol (Ty_bitv 64) Expr.symbol

  let symbol_f32 () = Choice.with_new_symbol (Ty_fp 32) Expr.symbol

  let symbol_f64 () = Choice.with_new_symbol (Ty_fp 64) Expr.symbol

  let symbol_v128 () = Choice.with_new_symbol (Ty_bitv 128) Expr.symbol

  let symbol_range (lo : Value.int32) (hi : Value.int32) =
    let open Choice in
    let* x = symbol_i32 () in
    let* () = assume (Value.I32.le lo x) in
    let+ () = assume (Value.I32.gt hi x) in
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
    Log.app (fun m -> m "%c@?" (char_of_int (Int32.to_int c)));
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
    return @@ Value.const_i32 @@ Mutex.protect cov_lock
    @@ fun () ->
    if Hashtbl.mem covered_labels id || in_seacoral_store id then 1l else 0l

  let cov_label_set m id ptr =
    let open Choice in
    let open Smtml in
    let id = Expr.simplify id in
    let ptr = Expr.simplify ptr in
    match (Expr.view id, Expr.view ptr) with
    | Val (Bitv id), Val (Bitv ptr)
      when Bitvector.numbits id = 32 && Bitvector.numbits ptr = 32 ->
      let id = Bitvector.to_int32 id in
      let ptr = Bitvector.to_int32 ptr in
      Mutex.protect cov_lock @@ fun () ->
      if Hashtbl.mem covered_labels id || in_seacoral_store id then abort ()
      else
        let* chars = make_str m [] ptr in
        let str = String.init (Array.length chars) (Array.get chars) in
        Hashtbl.add covered_labels id str;
        add_label (Int32.to_int id, str)
    | _ ->
      Log.err (fun m ->
        m "cov_label_set: invalid type id:%a ptr:%a" Expr.pp id Expr.pp ptr );
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
open Symbolic.Extern_func
open Symbolic.Extern_func.Syntax

let symbolic_extern_module =
  let functions =
    [ ("i8_symbol", Extern_func (unit ^->. i32, symbol_i8))
    ; ("i16_symbol", Extern_func (unit ^->. i32, symbol_i16))
    ; ("i32_symbol", Extern_func (unit ^->. i32, symbol_i32))
    ; ("i64_symbol", Extern_func (unit ^->. i64, symbol_i64))
    ; ("f32_symbol", Extern_func (unit ^->. f32, symbol_f32))
    ; ("f64_symbol", Extern_func (unit ^->. f64, symbol_f64))
    ; ("v128_symbol", Extern_func (unit ^->. v128, symbol_v128))
    ; ("bool_symbol", Extern_func (unit ^->. i32, symbol_bool))
    ; ( "invisible_bool_symbol"
      , Extern_func (unit ^->. i32, symbol_invisible_bool) )
    ; ("range_symbol", Extern_func (i32 ^-> i32 ^->. i32, symbol_range))
    ; ("assume", Extern_func (i32 ^->. unit, assume))
    ; ("assert", Extern_func (i32 ^->. unit, assert'))
    ; ("in_replay_mode", Extern_func (unit ^->. i32, in_replay_mode))
    ; ("print_char", Extern_func (i32 ^->. unit, print_char))
    ; ( "cov_label_set"
      , Extern_func (memory ^-> i32 ^-> i32 ^->. unit, cov_label_set) )
    ; ("cov_label_is_covered", Extern_func (i32 ^->. i32, cov_label_is_covered))
    ; ("open_scope", Extern_func (memory ^-> i32 ^->. unit, open_scope))
    ; ("close_scope", Extern_func (unit ^->. unit, close_scope))
    ; ("alloc", Extern_func (memory ^-> i32 ^-> i32 ^->. i32, alloc))
    ; ("dealloc", Extern_func (memory ^-> i32 ^->. i32, free))
    ; ("abort", Extern_func (unit ^->. unit, abort))
    ; ("exit", Extern_func (i32 ^->. unit, exit))
    ]
  in
  { Link.functions }

let fd_write _ _ _ _ = assert false

let proc_exit _ =
  Log.warn (fun m -> m "used dummy proc_exit implementation");
  Choice.return ()

let random_get _ _ =
  Log.warn (fun m -> m "used dummy random_get implementation");
  Choice.return @@ Symbolic_value.const_i32 0l

let wasi_snapshot_preview1 =
  let functions =
    [ ("fd_write", Extern_func (i32 ^-> i32 ^-> i32 ^-> i32 ^->. i32, fd_write))
    ; ("proc_exit", Extern_func (i32 ^->. unit, proc_exit))
    ; ("random_get", Extern_func (i32 ^-> i32 ^->. i32, random_get))
    ]
  in
  { Link.functions }
