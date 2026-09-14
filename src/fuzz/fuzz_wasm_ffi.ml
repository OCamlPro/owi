(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

exception Abort

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
    Log.debug (fun m -> m "ASSUME");
    if not @@ Prelude.Int32.equal 0l (Concrete_i32.to_int32 b) then Ok ()
    else raise Abort

  let assert' n =
    Log.debug (fun m -> m "ASSERT");
    if Prelude.Int32.equal 0l n then Error `Assert_failure else Ok ()

  let symbol_invisible_bool () = Ok (if Random.bool () then 1l else 0l)

  let symbol_i32 () = Ok (Fuzz_gen.i32 ())

  let symbol_i64 () = Ok (Fuzz_gen.i64 ())

  let symbol_f32 () = Ok (Fuzz_gen.f32 ())

  let symbol_f64 () = Ok (Fuzz_gen.f64 ())

  let symbol_v128 () = Ok (Fuzz_gen.v128 ())

  let abort () =
    Log.debug (fun m -> m "ABORT");
    raise Abort

  let exit (_n : Concrete_value.i32) =
    (* TODO: handle n as a potential error? *)
    Log.debug (fun m -> m "EXIT");
    raise Abort

  let symbol_range min max =
    (* TODO: ensure min <= max *)
    let n = Random.int32_in_range ~min ~max in
    Fuzz_state.model := Concrete_value.I32 n :: !Fuzz_state.model;
    Ok n

  let print_char c =
    Log.app (fun m -> m "%c" (char_of_int (Int32.to_int c)));
    Ok ()

  let in_replay_mode () = Ok 0l

  let cov_label_is_covered _id =
    (* TODO: implement properly *)
    Ok 0l

  let cov_label_set _m _id _str_ptr =
    (* TODO: implement properly *)
    Ok ()

  let open_scope_null_terminated _m _strptr =
    Log.debug (fun m -> m "OPEN SCOPE (NULL TERMINATED)");
    (* TODO: implement properly *)
    Ok ()

  let open_scope_of_length _m _strptr _length =
    Log.debug (fun m -> m "OPEN SCOPE (LENGTH)");
    (* TODO: implement properly *)
    Ok ()

  let close_scope () =
    Log.debug (fun m -> m "CLOSE SCOPE");
    (* TODO: implement properly *)
    Ok ()
end

let owi =
  let open M in
  let open Concrete_extern.Func in
  let open Concrete_extern.Func.Syntax in
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
    , Extern_func (memory 0 ^-> i32 ^-> i32 ^->. unit, open_scope_of_length) )
  ; ("close_scope", Extern_func (unit ^->. unit, close_scope))
  ; ("abort", Extern_func (unit ^->. unit, abort))
  ; ("exit", Extern_func (i32 ^->. unit, exit))
  ]
