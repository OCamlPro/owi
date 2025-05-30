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
  let scopes = ref Symbol_scope.empty in

  let add_sym i =
    (* the type doesn't matter, we just want the name for now *)
    let sym = Smtml.Symbol.(Fmt.str "symbol_%d" i @: Smtml.Ty.Ty_bitv 0) in
    scopes := Symbol_scope.symbol sym !scopes
  in

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
        Logs.info (fun m -> m "scopes : [%a]" Symbol_scope.pp !scopes);
        Logs.app (fun m -> m "Assertion failure was correctly reached!");
        exit 0
      end;
      Ok ()

    let symbol_i32 () =
      let i = next () in
      match model.(i) with
      | V.I32 n ->
        add_sym i;
        Ok n
      | v ->
        Logs.err (fun m -> m "Got value %a but expected a i32 value." V.pp v);
        assert false

    let symbol_char () =
      let i = next () in
      match model.(i) with
      | V.I32 n ->
        add_sym i;
        Ok n
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
      let i = next () in
      match model.(i) with
      | V.I32 n ->
        add_sym i;
        Ok n
      | v ->
        Logs.err (fun m ->
          m "Got value %a but expected a i8 (i32) value." V.pp v );
        assert false

    let symbol_i64 () =
      let i = next () in
      match model.(i) with
      | V.I64 n ->
        add_sym i;
        Ok n
      | v ->
        Logs.err (fun m -> m "Got value %a but expected a i64 value." V.pp v);
        assert false

    let symbol_f32 () =
      let i = next () in
      match model.(i) with
      | V.F32 n ->
        add_sym i;
        Ok n
      | v ->
        Logs.err (fun m -> m "Got value %a but expected a f32 value." V.pp v);
        assert false

    let symbol_f64 () =
      let i = next () in
      match model.(i) with
      | V.F64 n ->
        add_sym i;
        Ok n
      | v ->
        Logs.err (fun m -> m "Got value %a but expected a f64 value." V.pp v);
        assert false

    let symbol_range _ _ =
      let i = next () in
      match model.(i) with
      | V.I32 n ->
        add_sym i;
        Ok n
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
      scopes := Symbol_scope.open_scope str !scopes;
      Concrete_choice.return ()

    let close_scope () =
      scopes := Symbol_scope.close_scope !scopes;
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
      ; ( "open_scope"
        , Concrete_extern_func.Extern_func
            (Func (Mem (Arg (I32, Res)), R0), open_scope) )
      ; ( "close_scope"
        , Concrete_extern_func.Extern_func (Func (UArg Res, R0), close_scope) )
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
  let file_ext = Fpath.get_ext replay_file in
  let* parse_fn =
    match String.lowercase_ascii file_ext with
    | ".json" -> Ok Symbol_scope.model_of_json
    | ".scfg" -> Ok Symbol_scope.model_of_scfg
    | _ -> Error (`Unsupported_file_extension file_ext)
  in
  let* file_content = Bos.OS.File.read replay_file in
  let* model_entry_point, model =
    match parse_fn file_content with
    | Error (`Msg msg) -> Error (`Invalid_model msg)
    | Error err -> Error err
    | Ok (entry_point, model) ->
      let bindings = Smtml.Model.get_bindings model in
      let+ model =
        list_map
          (fun (_sym, v) ->
            match v with
            | Smtml.Value.False -> Ok (V.I32 0l)
            | True -> Ok (V.I32 1l)
            | Bitv bv when Smtml.Bitvector.numbits bv <= 32 ->
              Ok (V.I32 (Smtml.Bitvector.to_int32 bv))
            | Bitv bv when Smtml.Bitvector.numbits bv <= 64 ->
              Ok (V.I64 (Smtml.Bitvector.to_int64 bv))
            | Num (F32 n) -> Ok (V.F32 (Float32.of_bits n))
            | Num (F64 n) -> Ok (V.F64 (Float64.of_bits n))
            | Bitv bv ->
              Error
                (`Invalid_model
                   (Fmt.str "can not handle bitvectors of size %d"
                      (Smtml.Bitvector.numbits bv) ) )
            | Unit | Int _ | Real _ | Str _ | List _ | App _ | Nothing ->
              Error
                (`Invalid_model
                   (Fmt.str "unexpected value type: %a" Smtml.Value.pp v) ) )
          bindings
      in
      (entry_point, Array.of_list model)
  in
  let entry_point =
    if Option.is_none entry_point then model_entry_point else entry_point
  in
  Logs.debug (fun m -> m "%s" (Option.value ~default:"na" entry_point));
  Logs.debug (fun m -> m "%s" (Option.value ~default:"na" model_entry_point));
  let+ () =
    run_file ~unsafe ~optimize ~entry_point ~invoke_with_symbols source_file
      model
  in
  Logs.app (fun m -> m "All OK!")
