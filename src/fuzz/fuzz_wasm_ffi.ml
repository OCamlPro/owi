(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module M :
  Wasm_ffi_intf.S0
    with type 'a t := 'a Result.t
     and type memory := Concrete_memory.t
     and type i32 := Concrete_value.i32
     and type i64 := Concrete_value.i64
     and type f32 := Concrete_value.f32
     and type f64 := Concrete_value.f64
     and type v128 := Concrete_value.v128 = struct
  let assume b =
    if not @@ Prelude.Int32.equal 0l (Concrete_i32.to_int32 b) then Ok ()
    else
      (* TODO: stop current round properly *)
      raise @@ Failure "TODO"

  let assert' n =
    if Prelude.Int32.equal 0l n then Error (`Msg "I found a bug") else Ok ()

  let symbol_invisible_bool () = Ok (if Random.bool () then 1l else 0l)

  let symbol_i32 () = Ok (Fuzz_gen.i32 ())

  let symbol_i64 () = Ok (Fuzz_gen.i64 ())

  let symbol_f32 () = Ok (Fuzz_gen.f32 ())

  let symbol_f64 () = Ok (Fuzz_gen.f64 ())

  let symbol_v128 () = Ok (Fuzz_gen.v128 ())

  let abort () =
    (* TODO: stop the round properly *)
    Error (`Msg "abort")

  let alloc _m _addr size =
    let r = !Fuzz_state.brk in
    Fuzz_state.brk := Int32.add !Fuzz_state.brk size;
    Ok r

  let free (_ : Concrete_memory.t) adr = Ok adr

  let exit (n : Concrete_value.i32) = exit (Int32.to_int n)

  let symbol_range min max =
    (* TODO: ensure min <= max *)
    let n = Random.int32_in_range ~min ~max in
    Fuzz_state.model := Concrete_value.I32 n :: !Fuzz_state.model;
    Ok n

  let print_char c =
    Log.app (fun m -> m "%c" (char_of_int (Int32.to_int c)));
    Ok ()

  let in_replay_mode () = Ok 0l

  let _make_str_null_terminated _m _accu _i = raise @@ Failure "TODO"

  let _make_str_of_length _m _accu _i _len = raise @@ Failure "TODO"

  let cov_label_is_covered _id = raise @@ Failure "TODO"

  let cov_label_set _m _id _str_ptr = raise @@ Failure "TODO"

  let open_scope_null_terminated _m _strptr = raise @@ Failure "TODO"

  let open_scope_of_length _m _strptr _length = raise @@ Failure "TODO"

  let close_scope () = raise @@ Failure "TODO"
end

let extern_module =
  let open M in
  let open Concrete_extern_func in
  let open Concrete_extern_func.Syntax in
  let functions =
    [ ("i32_symbol", Extern_func (unit ^->. i32, symbol_i32))
    ; ("i64_symbol", Extern_func (unit ^->. i64, symbol_i64))
    ; ("f32_symbol", Extern_func (unit ^->. f32, symbol_f32))
    ; ("f64_symbol", Extern_func (unit ^->. f64, symbol_f64))
    ; ("v128_symbol", Extern_func (unit ^->. v128, symbol_v128))
    ; ("range_symbol", Extern_func (i32 ^-> i32 ^->. i32, symbol_range))
    ; ("assume", Extern_func (i32 ^->. unit, assume))
    ; ("assert", Extern_func (i32 ^->. unit, assert'))
    ; ("in_replay_mode", Extern_func (unit ^->. i32, in_replay_mode))
    ; ("print_char", Extern_func (i32 ^->. unit, print_char))
    ; ("cov_label_is_covered", Extern_func (i32 ^->. i32, cov_label_is_covered))
    ; ( "cov_label_set"
      , Extern_func (memory 0 ^-> i32 ^-> i32 ^->. unit, cov_label_set) )
    ; ( "open_scope_null_terminated"
      , Extern_func (memory 0 ^-> i32 ^->. unit, open_scope_null_terminated) )
    ; ( "open_scope_of_length"
      , Extern_func (memory 0 ^-> i32 ^-> i32 ^->. unit, open_scope_of_length)
      )
    ; ("close_scope", Extern_func (unit ^->. unit, close_scope))
    ; ("alloc", Extern_func (memory 0 ^-> i32 ^-> i32 ^->. i32, alloc))
    ; ("dealloc", Extern_func (memory 0 ^-> i32 ^->. i32, free))
    ; ("abort", Extern_func (unit ^->. unit, abort))
    ; ("exit", Extern_func (i32 ^->. unit, exit))
    ]
  in
  { Extern.Module.functions; func_type = Concrete_extern_func.extern_type }
