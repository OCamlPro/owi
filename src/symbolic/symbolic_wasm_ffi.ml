(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module Expr = Smtml.Expr

(* The constraint is used here to make sure we don't forget to define one of the expected FFI functions, this whole file is further constrained such that if one function of M is unused in the FFI module below, an error will be displayed *)
module M :
  Wasm_ffi_intf.S0
    with type 'a t := 'a Symbolic_choice.t
     and type memory := Symbolic_memory.t
     and module Value := Symbolic_value = struct
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

  let assume (b : Symbolic_i32.t) : unit Symbolic_choice.t =
    Symbolic_choice.assume (Symbolic_i32.to_boolean b) None

  let assert' (b : Symbolic_i32.t) : unit Symbolic_choice.t =
    Symbolic_choice.assertion @@ Symbolic_i32.to_boolean b

  let symbol_invisible_bool () =
    (* TODO: should we change this to an i32 too? *)
    Symbolic_choice.with_new_invisible_symbol (Ty_bitv 1) (fun sym ->
      Expr.cvtop (Ty_bitv 32) (Zero_extend 31) (Expr.symbol sym) )

  let symbol_i32 () = Symbolic_choice.with_new_symbol (Ty_bitv 32) Expr.symbol

  let symbol_i64 () = Symbolic_choice.with_new_symbol (Ty_bitv 64) Expr.symbol

  let symbol_f32 () = Symbolic_choice.with_new_symbol (Ty_fp 32) Expr.symbol

  let symbol_f64 () = Symbolic_choice.with_new_symbol (Ty_fp 64) Expr.symbol

  let symbol_v128 () = Symbolic_choice.with_new_symbol (Ty_bitv 128) Expr.symbol

  let symbol_range (lo : Symbolic_i32.t) (hi : Symbolic_i32.t) =
    let open Symbolic_choice in
    let* x = symbol_i32 () in
    let* () = assume (Symbolic_i32.le lo x) None in
    let+ () = assume (Symbolic_i32.gt hi x) None in
    x

  let abort () : unit Symbolic_choice.t = Symbolic_choice.stop

  let alloc m (base : Symbolic_i32.t) (size : Symbolic_i32.t) :
    Symbolic_i32.t Symbolic_choice.t =
    Symbolic_memory.realloc m ~ptr:base ~size

  let free m (ptr : Symbolic_i32.t) : Symbolic_i32.t Symbolic_choice.t =
    Symbolic_memory.free m ptr

  let exit (_p : Symbolic_i32.t) : unit Symbolic_choice.t = abort ()

  let in_replay_mode () =
    Symbolic_choice.return @@ Symbolic_i32.of_boolean Symbolic_boolean.false_

  let print_char (c : Symbolic_i32.t) =
    let open Symbolic_choice in
    let* c = select_i32 c in
    Log.app (fun m -> m "%c@?" (char_of_int (Int32.to_int c)));
    return ()

  let rec make_str m accu i =
    let open Symbolic_choice in
    let* p = Symbolic_memory.load_8_u m (Symbolic_i32.of_concrete i) in
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
    let open Symbolic_choice in
    let* id = select_i32 id in
    return @@ Symbolic_i32.of_concrete @@ Mutex.protect cov_lock
    @@ fun () ->
    if Hashtbl.mem covered_labels id || in_seacoral_store id then 1l else 0l

  let cov_label_set m id ptr =
    let open Symbolic_choice in
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
    let open Symbolic_choice in
    let* ptr = select_i32 ptr in
    let* chars = make_str m [] ptr in
    let str = String.init (Array.length chars) (Array.get chars) in
    open_scope str

  let close_scope = Symbolic_choice.close_scope
end

type extern_func = Symbolic_extern_func.extern_func

open M
open Symbolic_extern_func
open Symbolic_extern_func.Syntax

let symbolic_extern_module =
  let functions =
    [ ("i32_symbol", Extern_func (unit ^->. i32, symbol_i32))
    ; ("i64_symbol", Extern_func (unit ^->. i64, symbol_i64))
    ; ("f32_symbol", Extern_func (unit ^->. f32, symbol_f32))
    ; ("f64_symbol", Extern_func (unit ^->. f64, symbol_f64))
    ; ("v128_symbol", Extern_func (unit ^->. v128, symbol_v128))
    ; ( "invisible_bool_symbol"
      , Extern_func (unit ^->. i32, symbol_invisible_bool) )
    ; ("range_symbol", Extern_func (i32 ^-> i32 ^->. i32, symbol_range))
    ; ("assume", Extern_func (i32 ^->. unit, assume))
    ; ("assert", Extern_func (i32 ^->. unit, assert'))
    ; ("in_replay_mode", Extern_func (unit ^->. i32, in_replay_mode))
    ; ("print_char", Extern_func (i32 ^->. unit, print_char))
    ; ( "cov_label_set"
      , Extern_func (memory 0 ^-> i32 ^-> i32 ^->. unit, cov_label_set) )
    ; ("cov_label_is_covered", Extern_func (i32 ^->. i32, cov_label_is_covered))
    ; ("open_scope", Extern_func (memory 0 ^-> i32 ^->. unit, open_scope))
    ; ("close_scope", Extern_func (unit ^->. unit, close_scope))
    ; ("alloc", Extern_func (memory 0 ^-> i32 ^-> i32 ^->. i32, alloc))
    ; ("dealloc", Extern_func (memory 0 ^-> i32 ^->. i32, free))
    ; ("abort", Extern_func (unit ^->. unit, abort))
    ; ("exit", Extern_func (i32 ^->. unit, exit))
    ]
  in
  { Extern.Module.functions; func_type = Symbolic_extern_func.extern_type }

let fd_write _ _ _ _ = assert false

let proc_exit _ =
  Log.warn (fun m -> m "used dummy proc_exit implementation");
  Symbolic_choice.return ()

let random_get _ _ =
  Log.warn (fun m -> m "used dummy random_get implementation");
  Symbolic_choice.return @@ Symbolic_i32.of_concrete 0l

let wasi_snapshot_preview1 =
  let functions =
    [ ("fd_write", Extern_func (i32 ^-> i32 ^-> i32 ^-> i32 ^->. i32, fd_write))
    ; ("proc_exit", Extern_func (i32 ^->. unit, proc_exit))
    ; ("random_get", Extern_func (i32 ^-> i32 ^->. i32, random_get))
    ]
  in
  { Extern.Module.functions; func_type = Symbolic_extern_func.extern_type }
