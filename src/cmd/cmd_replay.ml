(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let run_file ~unsafe ~optimize filename model =
  let next =
    let next = ref ~-1 in
    fun () ->
      incr next;
      !next
  in

  let assume_i32 _ = () in

  let assume_positive_i32 _ = () in

  let assert_i32 n = assert (not @@ Prelude.Int32.equal n 0l) in

  let symbol_i32 () =
    match model.(next ()) with
    | Concrete_value.I32 n -> n
    | v ->
      Fmt.epr "Got value %a but expected a i32 value." Concrete_value.pp v;
      assert false
  in

  let symbol_char () =
    match model.(next ()) with
    | Concrete_value.I32 n -> n
    | v ->
      Fmt.epr "Got value %a but expected a char (i32) value." Concrete_value.pp
        v;
      assert false
  in

  let symbol_i8 () =
    match model.(next ()) with
    | Concrete_value.I32 n -> n
    | v ->
      Fmt.epr "Got value %a but expected a i8 (i32) value." Concrete_value.pp v;
      assert false
  in

  let symbol_i64 () =
    match model.(next ()) with
    | Concrete_value.I64 n -> n
    | v ->
      Fmt.epr "Got value %a but expected a i64 value." Concrete_value.pp v;
      assert false
  in

  let symbol_f32 () =
    match model.(next ()) with
    | Concrete_value.F32 n -> n
    | v ->
      Fmt.epr "Got value %a but expected a f32 value." Concrete_value.pp v;
      assert false
  in

  let symbol_f64 () =
    match model.(next ()) with
    | Concrete_value.F64 n -> n
    | v ->
      Fmt.epr "Got value %a but expected a f64 value." Concrete_value.pp v;
      assert false
  in

  let replay_extern_module =
    let functions =
      [ ( "i8_symbol"
        , Concrete_value.Func.Extern_func (Func (UArg Res, R1 I32), symbol_i8)
        )
      ; ( "char_symbol"
        , Concrete_value.Func.Extern_func (Func (UArg Res, R1 I32), symbol_char)
        )
      ; ( "i32_symbol"
        , Concrete_value.Func.Extern_func (Func (UArg Res, R1 I32), symbol_i32)
        )
      ; ( "i64_symbol"
        , Concrete_value.Func.Extern_func (Func (UArg Res, R1 I64), symbol_i64)
        )
      ; ( "f32_symbol"
        , Concrete_value.Func.Extern_func (Func (UArg Res, R1 F32), symbol_f32)
        )
      ; ( "f64_symbol"
        , Concrete_value.Func.Extern_func (Func (UArg Res, R1 F64), symbol_f64)
        )
      ; ( "assume"
        , Concrete_value.Func.Extern_func (Func (Arg (I32, Res), R0), assume_i32)
        )
      ; ( "assume_positive_i32"
        , Concrete_value.Func.Extern_func
            (Func (Arg (I32, Res), R0), assume_positive_i32) )
      ; ( "assert"
        , Concrete_value.Func.Extern_func (Func (Arg (I32, Res), R0), assert_i32)
        )
      ]
    in
    { Link.functions }
  in

  let link_state =
    Link.extern_module Link.empty_state ~name:"symbolic" replay_extern_module
  in

  let* m =
    Compile.File.until_binary_validate ~rac:false ~srac:false ~unsafe filename
  in
  let* m = Cmd_utils.add_main_as_start m in
  let* m, link_state =
    Compile.Binary.until_link ~unsafe link_state ~optimize ~name:None m
  in
  let c = Interpret.Concrete.modul link_state.envs m in
  c

let cmd profiling debug unsafe optimize replay_file file =
  if profiling then Log.profiling_on := true;
  if debug then Log.debug_on := true;
  let* model =
    match Smtml.Model.Parse.Scfg.from_file replay_file with
    | Error msg -> Error (`Invalid_model msg)
    | Ok model ->
      let+ model =
        list_map
          (fun (_sym, v) ->
            match v with
            | Smtml.Value.False -> Ok (Concrete_value.I32 0l)
            | True -> Ok (Concrete_value.I32 1l)
            | Num (I8 n) -> Ok (Concrete_value.I32 (Int32.of_int n))
            | Num (I32 n) -> Ok (Concrete_value.I32 n)
            | Num (I64 n) -> Ok (Concrete_value.I64 n)
            | Num (F32 n) -> Ok (Concrete_value.F32 (Float32.of_bits n))
            | Num (F64 n) -> Ok (Concrete_value.F64 (Float64.of_bits n))
            | Unit | Int _ | Real _ | Str _ | List _ | App _ | Nothing ->
              Error
                (`Invalid_model
                   (Fmt.str "unexpected value type: %a" Smtml.Value.pp v) ) )
          (Smtml.Model.get_bindings model)
      in
      Array.of_list model
  in
  let+ () = run_file ~unsafe ~optimize file model in
  Fmt.pr "All OK@."
