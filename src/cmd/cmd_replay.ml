(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let run_file ~unsafe ~optimize ~entry_point ~invoke_with_symbols filename model
    =
  let next =
    let next = ref ~-1 in
    fun () ->
      incr next;
      !next
  in
  let brk = ref @@ Int32.of_int 0 in
  let covered_labels = Hashtbl.create 16 in
  let opened_scopes = ref [] in

  let module M :
    Wasm_ffi_intf.S0
      with type 'a t = 'a Result.t
       and type memory = Concrete_memory.t
       and module Value = Concrete_value = struct
    type 'a t = 'a Result.t

    type memory = Concrete_memory.t

    module Value = Concrete_value

    let assume _ = Ok ()

    let assert' n =
      if Prelude.Int32.equal n 0l then begin
        Logs.app (fun m -> m "Assertion failure was correctly reached!");
        exit 0
      end;
      Ok ()

    let symbol_i32 () =
      match model.(next ()) with
      | V.I32 n -> Ok n
      | v ->
        Logs.err (fun m -> m "Got value %a but expected a i32 value." V.pp v);
        assert false

    let symbol_char () =
      match model.(next ()) with
      | V.I32 n -> Ok n
      | v ->
        Logs.err (fun m ->
          m "Got value %a but expected a char (i32) value." V.pp v );
        assert false

    let symbol_invisible_bool () = Ok 0l

    let symbol_bool = symbol_char

    let abort () =
      Logs.err (fun m -> m "Unexpected abort call.");
      exit 121

    let alloc _m _addr size =
      let r = !brk in
      brk := Int32.add !brk size;
      Ok r

    let free (_ : memory) adr = Ok adr

    let exit (n : Value.int32) = exit (Int32.to_int n)

    let symbol_i8 () =
      match model.(next ()) with
      | V.I32 n -> Ok n
      | v ->
        Logs.err (fun m ->
          m "Got value %a but expected a i8 (i32) value." V.pp v );
        assert false

    let symbol_i64 () =
      match model.(next ()) with
      | V.I64 n -> Ok n
      | v ->
        Logs.err (fun m -> m "Got value %a but expected a i64 value." V.pp v);
        assert false

    let symbol_f32 () =
      match model.(next ()) with
      | V.F32 n -> Ok n
      | v ->
        Logs.err (fun m -> m "Got value %a but expected a f32 value." V.pp v);
        assert false

    let symbol_f64 () =
      match model.(next ()) with
      | V.F64 n -> Ok n
      | v ->
        Logs.err (fun m -> m "Got value %a but expected a f64 value." V.pp v);
        assert false

    let symbol_range _ _ =
      match model.(next ()) with
      | V.I32 n -> Ok n
      | v ->
        Logs.err (fun m -> m "Got value %a but expected a i32 value." V.pp v);
        assert false

    let print_char c =
      Logs.app (fun m -> m "%c" (char_of_int (Int32.to_int c)));
      Ok ()

    let in_replay_mode () = Ok 1l

    let rec make_str m accu i =
      let open Concrete_choice in
      let* p = Concrete_memory.load_8_u m i in
      if Int32.gt p 255l || Int32.lt p 0l then trap `Invalid_character_in_memory
      else
        let ch = char_of_int (Int32.to_int p) in
        if Char.equal ch '\x00' then return (List.rev accu |> Array.of_list)
        else make_str m (ch :: accu) (Int32.add i (Int32.of_int 1))

    let cov_label_is_covered id =
      let open Concrete_choice in
      let* id = select_i32 id in
      return @@ Value.const_i32
      @@ if Hashtbl.mem covered_labels id then 1l else 0l

    let cov_label_set m id str_ptr =
      let* chars = make_str m [] str_ptr in
      let str = String.init (Array.length chars) (Array.get chars) in
      Hashtbl.add covered_labels id str;
      Logs.debug (fun m -> m "reached %ld@." id);
      Ok ()

    let open_scope m strptr =
      let* chars = make_str m [] strptr in
      let str = String.init (Array.length chars) (Array.get chars) in
      opened_scopes := str :: !opened_scopes;
      Concrete_choice.return ()

    let close_scope () =
      match !opened_scopes with
      | [] ->
        Logs.err (fun m -> m "Trying to close a non existing scope");
        assert false
      | _ :: t ->
        opened_scopes := t;
        Concrete_choice.return ()
  end in
  let replay_extern_module =
    let open M in
    let functions =
      [ ( "i8_symbol"
        , Concrete_extern_func.Extern_func (Func (UArg Res, R1 I32), symbol_i8)
        )
      ; ( "char_symbol"
        , Concrete_extern_func.Extern_func (Func (UArg Res, R1 I32), symbol_char)
        )
      ; ( "i32_symbol"
        , Concrete_extern_func.Extern_func (Func (UArg Res, R1 I32), symbol_i32)
        )
      ; ( "i64_symbol"
        , Concrete_extern_func.Extern_func (Func (UArg Res, R1 I64), symbol_i64)
        )
      ; ( "f32_symbol"
        , Concrete_extern_func.Extern_func (Func (UArg Res, R1 F32), symbol_f32)
        )
      ; ( "f64_symbol"
        , Concrete_extern_func.Extern_func (Func (UArg Res, R1 F64), symbol_f64)
        )
      ; ( "bool_symbol"
        , Concrete_extern_func.Extern_func (Func (UArg Res, R1 I32), symbol_bool)
        )
      ; ( "range_symbol"
        , Concrete_extern_func.Extern_func
            (Func (Arg (I32, Arg (I32, Res)), R1 I32), symbol_range) )
      ; ( "assume"
        , Concrete_extern_func.Extern_func (Func (Arg (I32, Res), R0), assume)
        )
      ; ( "assert"
        , Concrete_extern_func.Extern_func (Func (Arg (I32, Res), R0), assert')
        )
      ; ( "in_replay_mode"
        , Concrete_extern_func.Extern_func
            (Func (UArg Res, R1 I32), in_replay_mode) )
      ; ( "print_char"
        , Concrete_extern_func.Extern_func
            (Func (Arg (I32, Res), R0), print_char) )
      ; ( "cov_label_is_covered"
        , Concrete_extern_func.Extern_func
            (Func (Arg (I32, Res), R1 I32), cov_label_is_covered) )
      ; ( "cov_label_set"
        , Concrete_extern_func.Extern_func
            (Func (Mem (Arg (I32, Arg (I32, Res))), R0), cov_label_set) )
      ]
    in
    { Link.functions }
  in

  let summaries_extern_module =
    let open M in
    let functions =
      [ ( "alloc"
        , Concrete.Extern_func.Extern_func
            (Func (Mem (Arg (I32, Arg (I32, Res))), R1 I32), alloc) )
      ; ( "dealloc"
        , Concrete.Extern_func.Extern_func
            (Func (Mem (Arg (I32, Res)), R1 I32), free) )
      ; ("abort", Concrete.Extern_func.Extern_func (Func (UArg Res, R0), abort))
      ; ( "exit"
        , Concrete.Extern_func.Extern_func (Func (Arg (I32, Res), R0), exit) )
      ]
    in
    { Link.functions }
  in

  let link_state =
    Link.extern_module Link.empty_state ~name:"symbolic" replay_extern_module
  in
  let link_state =
    Link.extern_module link_state ~name:"summaries" summaries_extern_module
  in

  let* m =
    Compile.File.until_binary_validate ~rac:false ~srac:false ~unsafe filename
  in
  let* m = Cmd_utils.set_entry_point entry_point invoke_with_symbols m in
  let* m, link_state =
    Compile.Binary.until_link ~unsafe link_state ~optimize ~name:None m
  in
  let* () = Interpret.Concrete.modul link_state.envs m in
  Ok ()

let cmd ~unsafe ~optimize ~replay_file ~source_file ~entry_point
  ~invoke_with_symbols =
  let* parse_fn =
    let ext = Fpath.get_ext replay_file in
    match String.lowercase_ascii ext with
    | ".json" -> Ok Smtml.Model.Parse.Json.from_file
    | ".scfg" -> Ok Smtml.Model.Parse.Scfg.from_file
    | _ -> Error (`Unsupported_file_extension ext)
  in
  let* model =
    match parse_fn replay_file with
    | Error (`Msg msg) -> Error (`Invalid_model msg)
    | Ok model ->
      let bindings = Smtml.Model.get_bindings model in
      let+ model =
        list_map
          (fun (_sym, v) ->
            match v with
            | Smtml.Value.False -> Ok (V.I32 0l)
            | True -> Ok (V.I32 1l)
            | Num (I8 n) -> Ok (V.I32 (Int32.of_int n))
            | Num (I32 n) -> Ok (V.I32 n)
            | Num (I64 n) -> Ok (V.I64 n)
            | Num (F32 n) -> Ok (V.F32 (Float32.of_bits n))
            | Num (F64 n) -> Ok (V.F64 (Float64.of_bits n))
            | Unit | Int _ | Real _ | Str _ | List _ | App _ | Nothing ->
              Error
                (`Invalid_model
                   (Fmt.str "unexpected value type: %a" Smtml.Value.pp v) ) )
          bindings
      in
      Array.of_list model
  in
  let+ () =
    run_file ~unsafe ~optimize ~entry_point ~invoke_with_symbols source_file
      model
  in
  Logs.app (fun m -> m "All OK!")
