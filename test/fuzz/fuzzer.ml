let () = Random.self_init ()

let timeout_count = ref 0

let global_count = ref 0

let write_module (fpath : Fpath.t) m =
  match Bos.OS.File.writef fpath "%a@." Owi.Text.pp_modul m with
  | Ok () -> Ok ()
  | Error (`Msg err) ->
    Fmt.error_msg "Failed to write module to %a: %s" Fpath.pp fpath err

let compare (module I1 : Interprets.INTERPRET)
  (module I2 : Interprets.INTERPRET) m =
  if Param.debug then begin
    Fmt.epr "comparing %s and %s@\n    @[<v>" I1.name I2.name;
    Fmt.epr "running %s@\n" I1.name;
    Fmt.flush Fmt.stderr ()
  end;
  let r1 = I1.parse_and_run m in
  if Param.debug then begin
    Fmt.epr "running %s@\n" I2.name
  end;
  let r2 = I2.parse_and_run m in
  Fmt.epr "@]";
  match (r1, r2) with
  | Ok (), Ok () -> true
  | Error `Timeout, Error `Timeout ->
    incr timeout_count;
    true
  | Error `Timeout, Ok () ->
    Param.allow_partial_timeout
    ||
    ( Fmt.epr "timeout for `%s` but not for `%s`" I1.name I2.name;
      false )
  | Ok (), Error `Timeout ->
    Param.allow_partial_timeout
    ||
    ( Fmt.epr "timeout for `%s` but not for `%s`" I2.name I1.name;
      false )
  | Error `Timeout, Error msg ->
    let msg = Owi.Result.err_to_string msg in
    Param.allow_partial_timeout
    ||
    ( Fmt.epr "timeout for `%s` but error `%s` for `%s`" I1.name msg I2.name;
      false )
  | Error msg, Error `Timeout ->
    let msg = Owi.Result.err_to_string msg in
    Param.allow_partial_timeout
    ||
    ( Fmt.epr "timeout for `%s` but error `%s` for `%s`" I2.name msg I1.name;
      false )
  | Error msg1, Error msg2 ->
    let msg1 = Owi.Result.err_to_string msg1 in
    let msg2 = Owi.Result.err_to_string msg2 in
    true (* TODO: fixme *) || String.equal msg1 msg2
    ||
    ( Fmt.epr "`%s` gave error `%s` but `%s` gave error `%s`" I1.name msg1
        I2.name msg2;
      false )
  | Ok (), Error msg ->
    let msg = Owi.Result.err_to_string msg in
    Fmt.epr "`%s` was OK but `%s` gave error `%s`" I1.name I2.name msg;
    false
  | Error msg, Ok () ->
    let msg = Owi.Result.err_to_string msg in
    Fmt.epr "`%s` was OK but `%s` gave error `%s`" I2.name I1.name msg;
    false

let check (module I1 : Interprets.INTERPRET) (module I2 : Interprets.INTERPRET)
  m =
  let open Owi.Syntax in
  let+ () =
    if Param.save_modules then begin
      let outdir = Param.output_dir in
      let* _exist = Bos.OS.Dir.create ~mode:0o755 outdir in
      let filename =
        Fpath.(Param.output_dir / Fmt.str "gen_do_module_%d.wat" !global_count)
      in
      let* () = write_module filename m in
      if Param.debug then Fmt.epr "Saved module to %a@\n" Fpath.pp filename;
      Ok ()
    end
    else Ok ()
  in

  compare (module I1) (module I2) m

let add_test name gen (module I1 : Interprets.INTERPRET)
  (module I2 : Interprets.INTERPRET) =
  Crowbar.add_test ~name [ gen ] (fun m ->
    incr global_count;
    if Param.debug then Fmt.epr "%a@\n" Owi.Text.pp_modul m;
    Fmt.epr "test module %d [got %d timeouts...]@\n@[<v>" !global_count
      !timeout_count;
    Fmt.flush Fmt.stderr ();
    match check (module I1) (module I2) m with
    | Error (`Msg s) -> Fmt.failwith "%s" s
    | Ok v ->
      Crowbar.check v;
      Fmt.epr "@]" )

let gen (conf : Env.conf) =
  Crowbar.with_printer Owi.Text.pp_modul (Gen.modul conf)

let prepare_for_sym (modul : Owi.Text.modul) =
  let open Owi.Text in
  let open Owi.Types in
  let process_inst inst inst_list =
    match inst.Owi.Annotated.raw with
    | I32_const i ->
      let l =
        [ I32_const i; Call (Text "constant_symbol") ] |> Owi.Annotated.dummies
      in
      l @ inst_list
    | _ -> inst :: inst_list
  in
  let process_func fnc =
    let updated_body =
      List.fold_right process_inst fnc.body.raw [] |> Owi.Annotated.dummy
    in
    { fnc with body = updated_body }
  in
  let process_field = function
    | MFunc fnc -> MFunc (process_func fnc)
    | fld -> fld
  in
  let import_sym_cst_i32 =
    MImport
      { modul = "owi"
      ; name = "symbol_i32_constant"
      ; desc =
          Import_func
            ( Some "symbol_i32_constant"
            , Bt_raw (None, ([ (None, Num_type I32) ], [ Num_type I32 ])) )
      }
  in
  let updated_fields =
    import_sym_cst_i32 :: List.map process_field modul.fields
  in
  { modul with fields = updated_fields }

module S = Set.Make (Set.Make (Int))

let () =
  let open Interprets in
  if Param.reference_fuzzing then
    add_test "reference_fuzzing" (gen Env.Concrete)
      (module Owi_regular)
      (module Reference);
  if Param.symbolic_fuzzing then
    add_test "minimalist_symbolic_fuzzing" (gen Env.Symbolic)
      (module Owi_regular)
      (module Owi_minimalist_symbolic);
  if Param.full_symbolic_fuzzing then
    add_test "full_symbolic_fuzzing" (gen Env.Symbolic)
      (module Owi_regular)
      ( module Owi_full_symbolic (struct
        let symbolize = prepare_for_sym
      end) );
  if Param.call_graph_fuzzing then
    Crowbar.add_test ~name:"call_graph_fuzzing"
      [ gen Env.Concrete ]
      (fun m ->
        let open Owi.Cmd_call_graph in
        let graph_complete =
          build_call_graph_from_text_module Complete m None
        in
        let graph_sound = build_call_graph_from_text_module Sound m None in
        Crowbar.check (Owi.Graph.is_subgraph graph_complete graph_sound) );
  if Param.call_graph_scc_fuzzing then
    Crowbar.add_test ~name:"call_graph_scc_fuzzing"
      [ gen Env.Concrete ]
      (fun m ->
        let open Owi.Cmd_call_graph in
        let graph_complete =
          build_call_graph_from_text_module Complete m None
        in
        let scc_kosaraju = Owi.Graph.kosaraju graph_complete in
        let scc_tarjan = Owi.Graph.tarjan graph_complete in
        Crowbar.check (S.equal scc_kosaraju scc_tarjan) );
  if Param.control_flow_graph_scc_fuzzing then
    Crowbar.add_test ~name:"control_flow_graph_scc_fuzzing"
      [ gen Env.Concrete ]
      (fun m ->
        let open Owi.Cmd_cfg in
        let graph_complete = build_cfg_from_text_module m in
        let scc_kosaraju = Owi.Graph.kosaraju graph_complete in
        let scc_tarjan = Owi.Graph.tarjan graph_complete in
        Crowbar.check (S.equal scc_kosaraju scc_tarjan) )
