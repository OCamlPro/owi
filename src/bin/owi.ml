(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

open Owi
open Cmdliner

(* Helpers *)

let call_graph_mode_conv =
  let of_string s =
    match String.lowercase_ascii s with
    | "complete" -> Ok Cmd_wasm_inspect_cg.Complete
    | "sound" -> Ok Cmd_wasm_inspect_cg.Sound
    | _ -> Fmt.error_msg {|Expected "complete" or "sound" but got "%s"|} s
  in
  let pp fmt = function
    | Cmd_wasm_inspect_cg.Complete -> Fmt.string fmt "complete"
    | Cmd_wasm_inspect_cg.Sound -> Fmt.string fmt "sound"
  in
  Arg.conv (of_string, pp)

let coverage_criteria_conv =
  let open Label.Coverage_criteria in
  Arg.conv (of_string, pp)

let existing_file_conv =
  let open Prelude.Result.Syntax in
  let parse s =
    let* path = Fpath.of_string s in
    let* exists = Bos.OS.File.exists path in
    if exists then Ok path else Fmt.error_msg "no file '%a'" Fpath.pp path
  in
  Arg.conv (parse, Fpath.pp)

let existing_dir_conv =
  let open Prelude.Result.Syntax in
  let parse s =
    let* path = Fpath.of_string s in
    let* exists = Bos.OS.Dir.exists path in
    if exists then Ok path else Fmt.error_msg "no directory '%a'" Fpath.pp path
  in
  Arg.conv (parse, Fpath.pp)

let path_conv = Arg.conv (Fpath.of_string, Fpath.pp)

let solver_conv = Arg.conv (Smtml.Solver_type.of_string, Smtml.Solver_type.pp)

let exploration_conv =
  Arg.conv
    ( Symbolic_parameters.Exploration_strategy.of_string
    , Symbolic_parameters.Exploration_strategy.pp )

let model_format_conv =
  let of_string s =
    match String.lowercase_ascii s with
    | "scfg" -> Ok Model.Scfg
    | "json" -> Ok Json
    | _ -> Fmt.error_msg {|Expected "json" or "scfg" but got "%s"|} s
  in
  let pp fmt = function
    | Model.Scfg -> Fmt.string fmt "scfg"
    | Json -> Fmt.string fmt "json"
  in
  Arg.conv (of_string, pp)

(* Common options *)

let copts_t = Term.(const [])

let sdocs = Manpage.s_common_options

let shared_man =
  [ `S Manpage.s_bugs; `P "Email them to <owi.wildcat119@passmail.com>." ]

let version = Cmd_version.owi_version ()

let log_level =
  let env = Cmd.Env.info "OWI_VERBOSITY" in
  Logs_cli.level ~env ~docs:sdocs ()

let bench =
  let doc = "enable benchmarks" in
  Arg.(value & flag & info [ "bench" ] ~doc ~docs:sdocs)

(* Common terms *)

open Term.Syntax

let arch =
  let doc = "data model" in
  Arg.(value & opt int 32 & info [ "arch"; "m" ] ~doc)

let deterministic_result_order =
  let doc =
    "Guarantee a fixed deterministic order of found failures. This implies \
     --no-stop-at-failure."
  in
  Arg.(value & flag & info [ "deterministic-result-order" ] ~doc)

let call_graph_mode =
  let doc = {| The call graph is either "complete" or "sound" |} in
  Arg.(value & opt call_graph_mode_conv Sound & info [ "call-graph-mode" ] ~doc)

let coverage_criteria =
  let doc = {|Coverage criteria to use ("fc", "sc" or "dc").|} in
  Arg.(
    value
    & opt coverage_criteria_conv Label.Coverage_criteria.Statement_coverage
    & info [ "criteria" ] ~doc )

let eacsl =
  let doc =
    "e-acsl mode, refer to \
     https://frama-c.com/download/e-acsl/e-acsl-implementation.pdf for \
     Frama-C's current language feature implementations"
  in
  Arg.(value & flag & info [ "e-acsl" ] ~doc)

let entry_point default =
  let doc = "entry point of the executable" in
  Arg.(
    value
    & opt (some string) default
    & info [ "entry-point" ] ~doc ~docv:"FUNCTION" )

let exploration_strategy =
  let doc =
    {|exploration strategy to use ("fifo", "lifo", "random", "random-unseen-then-random", "rarity", "hot-path-penalty", "rarity-aging", "rarity-depth-aging", "rarity-depth-loop-aging", "rarity-depth-loop-aging-random")|}
  in
  Arg.(
    value
    & opt exploration_conv Symbolic_parameters.Exploration_strategy.FIFO
    & info [ "exploration" ] ~doc )

let fail_mode =
  let trap_doc = "ignore assertion violations and only report traps" in
  let assert_doc = "ignore traps and only report assertion violations" in
  Arg.(
    value
    & vflag Symbolic_parameters.Both
        [ (Trap_only, info [ "fail-on-trap-only" ] ~doc:trap_doc)
        ; (Assertion_only, info [ "fail-on-assertion-only" ] ~doc:assert_doc)
        ] )

let files =
  let doc = "source files" in
  Arg.(non_empty & pos_all existing_file_conv [] (info [] ~doc ~docv:"FILE"))

let generate_abstract_invariant =
  let doc =
    "Generate invariants by running the abstract interpretation engine."
  in
  Arg.(value & flag & info [ "generate-abstract-invariant" ] ~doc)

let includes =
  let doc = "headers path" in
  Arg.(value & opt_all existing_dir_conv [] & info [ "I" ] ~doc)

let invoke_with_symbols =
  let doc =
    "Invoke the entry point of the program with symbolic values instead of \
     dummy constants."
  in
  Arg.(value & flag & info [ "invoke-with-symbols" ] ~doc)

let model_format =
  let doc = {| The format of the model ("json" or "scfg") |} in
  Arg.(value & opt model_format_conv Scfg & info [ "model-format" ] ~doc)

let no_assert_failure_expression_printing =
  let doc = "do not display the expression in the assert failure" in
  Arg.(value & flag & info [ "no-assert-failure-expression-printing" ] ~doc)

let no_stop_at_failure =
  let doc = "do not stop when a program failure is encountered" in
  Arg.(value & flag & info [ "no-stop-at-failure" ] ~doc)

let no_value =
  let doc = "do not display a value for each symbol" in
  Arg.(value & flag & info [ "no-value" ] ~doc)

let no_worker_isolation =
  let doc = "Do not force each worker to run on an isolated physical core." in
  Arg.(value & flag & info [ "no-worker-isolation" ] ~doc)

let opt_lvl =
  let doc = "specify which optimization level to use" in
  Arg.(value & opt string "3" & info [ "O" ] ~doc)

let out_file =
  let doc = "Output the generated .wasm or .wat to FILE." in
  Arg.(
    value & opt (some path_conv) None & info [ "o"; "output" ] ~docv:"FILE" ~doc )

let model_out_file =
  let doc =
    "Output the generated model to FILE. if --no-stop-at-failure is given this \
     is used as a prefix and the ouputed files would have PREFIX_%d."
  in
  Arg.(
    value
    & opt (some path_conv) None
    & info [ "model-out-file" ] ~docv:"FILE" ~doc )

let property =
  let doc = "property file" in
  Arg.(
    value
    & opt (some existing_file_conv) None
    & info [ "property" ] ~doc ~docv:"FILE" )

let rounds =
  let doc = "Stop after a number of fuzzing rounds." in
  Arg.(value & opt (some int) None & info [ "rounds" ] ~doc ~docv:"I")

let seed =
  let doc = "Initial seed for the PRNG state" in
  Arg.(value & opt (some int) None & info [ "seed" ] ~doc ~docv:"I")

let solver =
  let docv = Arg.conv_docv solver_conv in
  let doc =
    let pp_bold_solver fmt ty = Fmt.pf fmt "$(b,%a)" Smtml.Solver_type.pp ty in
    let supported_solvers = Smtml.Solver_type.supported_solvers in
    Fmt.str
      "SMT solver to use. $(i,%s) must be one of the %d available solvers: %a"
      docv
      (List.length supported_solvers)
      (Fmt.list ~sep:Fmt.comma pp_bold_solver)
      supported_solvers
  in
  Arg.(
    value
    & opt solver_conv Smtml.Solver_type.Z3_solver
    & info [ "solver"; "s" ] ~doc ~docv )

let source_file =
  let doc = "source file" in
  Arg.(
    required & pos 0 (some existing_file_conv) None (info [] ~doc ~docv:"FILE") )

let setup_log =
  let+ bench
  and+ log_level
  and+ style_renderer = Fmt_cli.style_renderer ~docs:sdocs () in
  Log.setup ~bench style_renderer log_level

let testcomp =
  let doc = "test-comp mode" in
  Arg.(value & flag & info [ "testcomp" ] ~doc)

let timeout =
  let doc = "Stop execution after S seconds." in
  Arg.(value & opt (some float) None & info [ "timeout" ] ~doc ~docv:"S")

let timeout_instr =
  let doc = "Stop execution after running I instructions." in
  Arg.(value & opt (some int) None & info [ "timeout-instr" ] ~doc ~docv:"I")

let unsafe =
  let doc = "skip typechecking pass" in
  Arg.(value & flag & info [ "unsafe"; "u" ] ~doc)

let workers =
  let doc =
    "Number of workers for symbolic execution. Defaults to the number of \
     physical cores."
  in
  Arg.(value & opt (some int) None & info [ "workers"; "w" ] ~doc ~absent:"n")

let workspace : Fpath.t Cmdliner.Term.t =
  let doc = "write results and intermediate compilation artifacts to dir" in
  let aux =
    Arg.(
      value & opt (some path_conv) None & info [ "workspace" ] ~doc ~docv:"DIR" )
  in
  let open Term.Syntax in
  Term.cli_parse_result
  @@
  let+ workspace = aux in
  let open Prelude.Result.Syntax in
  let* workspace =
    match workspace with
    | Some workspace -> Ok workspace
    | None -> Bos.OS.Dir.tmp "owi_%s"
  in
  let* _created =
    Bos.OS.Dir.create ~path:true ~mode:0o755 Fpath.(workspace / "test-suite")
  in
  Ok workspace

let with_breadcrumbs =
  let doc = "add breadcrumbs to the generated model" in
  Arg.(value & flag & info [ "with-breadcrumbs" ] ~doc)

let no_ite_for_select =
  let doc = "do not use ite for select" in
  Arg.(value & flag & info [ "no-ite-for-select" ] ~doc)

let debug_trace =
  let doc = "output debug traces to use with the debug GUI" in
  Arg.(value & opt (some string) None & info [ "debug-trace" ] ~docv:"FILE" ~doc)

(* shared symbolic parameters *)

let symbolic_parameters =
  let+ deterministic_result_order
  and+ exploration_strategy
  and+ fail_mode
  and+ generate_abstract_invariant
  and+ model_format
  and+ model_out_file
  and+ invoke_with_symbols
  and+ no_assert_failure_expression_printing
  and+ no_ite_for_select
  and+ no_stop_at_failure
  and+ no_value
  and+ no_worker_isolation
  and+ seed
  and+ solver
  and+ timeout
  and+ timeout_instr
  and+ unsafe
  and+ with_breadcrumbs
  and+ workers in
  let use_ite_for_select = not no_ite_for_select in
  { Symbolic_parameters.deterministic_result_order
  ; exploration_strategy
  ; fail_mode
  ; generate_abstract_invariant
  ; invoke_with_symbols
  ; model_format
  ; model_out_file
  ; no_assert_failure_expression_printing
  ; no_stop_at_failure
  ; no_value
  ; no_worker_isolation
  ; seed
  ; solver
  ; timeout
  ; timeout_instr
  ; unsafe
  ; use_ite_for_select
  ; with_breadcrumbs
  ; workers
  }

(* owi c *)
module C = struct
  let entry_point = entry_point (Some "main")

  (* owi c abs *)
  let abs =
    let+ arch
    and+ debug_trace
    and+ eacsl
    and+ entry_point
    and+ files
    and+ includes
    and+ opt_lvl
    and+ out_file
    and+ property
    and+ () = setup_log
    and+ unsafe
    and+ workspace in
    Cmd_c.abs ~arch ~debug_trace ~eacsl ~entry_point ~files ~includes ~opt_lvl
      ~out_file ~property ~unsafe ~workspace

  (* owi c fuzz *)
  let fuzz =
    let+ arch
    and+ eacsl
    and+ entry_point
    and+ files
    and+ includes
    and+ opt_lvl
    and+ out_file
    and+ property
    and+ rounds
    and+ testcomp
    and+ timeout_instr
    and+ timeout
    and+ seed
    and+ () = setup_log
    and+ unsafe
    and+ workspace in

    Cmd_c.fuzz ~arch ~eacsl ~entry_point ~files ~includes ~opt_lvl ~out_file
      ~property ~rounds ~seed ~testcomp ~timeout ~timeout_instr ~unsafe
      ~workspace

  (* owi c hunt *)
  let hunt =
    let+ arch
    and+ eacsl
    and+ entry_point
    and+ files
    and+ includes
    and+ opt_lvl
    and+ out_file
    and+ property
    and+ rounds
    and+ seed
    and+ () = setup_log
    and+ symbolic_parameters
    and+ workspace in
    Cmd_c.hunt ~arch ~eacsl ~entry_point ~files ~includes ~opt_lvl ~out_file
      ~property ~rounds ~seed ~symbolic_parameters ~workspace

  (* owi c run *)
  let run =
    let+ arch
    and+ eacsl
    and+ entry_point
    and+ files
    and+ includes
    and+ opt_lvl
    and+ out_file
    and+ property
    and+ () = setup_log
    and+ timeout
    and+ timeout_instr
    and+ unsafe
    and+ workspace in
    Cmd_c.run ~arch ~eacsl ~entry_point ~files ~includes ~opt_lvl ~out_file
      ~property ~timeout ~timeout_instr ~unsafe ~workspace

  (* owi c sym *)
  let sym =
    let+ arch
    and+ eacsl
    and+ entry_point
    and+ files
    and+ includes
    and+ opt_lvl
    and+ out_file
    and+ property
    and+ () = setup_log
    and+ testcomp
    and+ symbolic_parameters
    and+ workspace in

    Cmd_c.sym ~arch ~eacsl ~entry_point ~files ~includes ~opt_lvl ~out_file
      ~property ~symbolic_parameters ~testcomp ~workspace
end

(* owi c++ *)
module Cpp = struct
  let entry_point = entry_point (Some "main")

  (* owi c++ abs *)
  let abs =
    let+ debug_trace
    and+ entry_point
    and+ files
    and+ includes
    and+ opt_lvl
    and+ out_file
    and+ unsafe
    and+ workspace in
    Cmd_cpp.abs ~debug_trace ~entry_point ~files ~includes ~opt_lvl ~out_file
      ~unsafe ~workspace

  (* owi c++ fuzz *)
  let fuzz =
    let+ entry_point
    and+ files
    and+ includes
    and+ opt_lvl
    and+ out_file
    and+ rounds
    and+ seed
    and+ timeout
    and+ timeout_instr
    and+ unsafe
    and+ workspace in
    Cmd_cpp.fuzz ~entry_point ~files ~includes ~opt_lvl ~out_file ~rounds ~seed
      ~timeout ~timeout_instr ~unsafe ~workspace

  (* owi c++ hunt *)
  let hunt =
    let+ entry_point
    and+ files
    and+ includes
    and+ opt_lvl
    and+ out_file
    and+ rounds
    and+ seed
    and+ symbolic_parameters
    and+ workspace in
    Cmd_cpp.hunt ~entry_point ~files ~includes ~opt_lvl ~out_file ~rounds ~seed
      ~symbolic_parameters ~workspace

  (* owi c++ run *)
  let run =
    let+ entry_point
    and+ files
    and+ includes
    and+ opt_lvl
    and+ out_file
    and+ () = setup_log
    and+ timeout
    and+ timeout_instr
    and+ unsafe
    and+ workspace in
    Cmd_cpp.run ~entry_point ~files ~includes ~opt_lvl ~out_file ~timeout
      ~timeout_instr ~unsafe ~workspace

  (* owi c++ sym *)
  let sym =
    let+ arch
    and+ entry_point
    and+ files
    and+ includes
    and+ opt_lvl
    and+ out_file
    and+ () = setup_log
    and+ symbolic_parameters
    and+ workspace in

    Cmd_cpp.sym ~arch ~entry_point ~files ~includes ~opt_lvl ~out_file
      ~symbolic_parameters ~workspace
end

(* owi go *)
module Go = struct
  let entry_point = entry_point (Some "_start")

  (* owi go abs *)
  let abs =
    let+ debug_trace
    and+ entry_point
    and+ files
    and+ out_file
    and+ () = setup_log
    and+ unsafe
    and+ workspace in
    Cmd_go.abs ~debug_trace ~entry_point ~files ~out_file ~unsafe ~workspace

  (* owi go fuzz *)
  let fuzz =
    let+ entry_point
    and+ files
    and+ out_file
    and+ rounds
    and+ seed
    and+ () = setup_log
    and+ timeout
    and+ timeout_instr
    and+ unsafe
    and+ workspace in
    Cmd_go.fuzz ~entry_point ~files ~out_file ~rounds ~seed ~timeout
      ~timeout_instr ~unsafe ~workspace

  (* owi go hunt *)
  let hunt =
    let+ entry_point
    and+ files
    and+ out_file
    and+ rounds
    and+ seed
    and+ () = setup_log
    and+ symbolic_parameters
    and+ workspace in
    Cmd_go.hunt ~entry_point ~files ~out_file ~rounds ~seed ~symbolic_parameters
      ~workspace

  (* owi go run *)
  let run =
    let+ files
    and+ out_file
    and+ () = setup_log
    and+ timeout
    and+ timeout_instr
    and+ unsafe
    and+ workspace in
    Cmd_go.run ~files ~out_file ~timeout ~timeout_instr ~unsafe ~workspace

  (* owi go sym *)
  let sym =
    let+ entry_point
    and+ files
    and+ out_file
    and+ () = setup_log
    and+ symbolic_parameters
    and+ workspace in
    Cmd_go.sym ~entry_point ~files ~out_file ~symbolic_parameters ~workspace
end

(* owi haskell *)
module Haskell = struct
  let entry_point = entry_point (Some "_start")

  (* owi haskell abs *)
  let abs =
    let+ debug_trace
    and+ entry_point
    and+ files
    and+ out_file
    and+ () = setup_log
    and+ unsafe
    and+ workspace in
    Cmd_haskell.abs ~debug_trace ~entry_point ~files ~out_file ~unsafe
      ~workspace

  (* owi haskell fuzz *)
  let fuzz =
    let+ entry_point
    and+ files
    and+ out_file
    and+ rounds
    and+ seed
    and+ () = setup_log
    and+ timeout
    and+ timeout_instr
    and+ unsafe
    and+ workspace in
    Cmd_haskell.fuzz ~entry_point ~files ~out_file ~rounds ~seed ~timeout
      ~timeout_instr ~unsafe ~workspace

  (* owi haskell hunt*)
  let hunt =
    let+ entry_point
    and+ files
    and+ out_file
    and+ rounds
    and+ seed
    and+ () = setup_log
    and+ symbolic_parameters
    and+ workspace in
    Cmd_haskell.hunt ~entry_point ~files ~out_file ~rounds ~seed
      ~symbolic_parameters ~workspace

  (* owi haskell run *)
  let run =
    let+ files
    and+ out_file
    and+ () = setup_log
    and+ timeout
    and+ timeout_instr
    and+ unsafe
    and+ workspace in
    Cmd_haskell.run ~files ~out_file ~timeout ~timeout_instr ~unsafe ~workspace

  (* owi haskell sym *)
  let sym =
    let+ entry_point
    and+ files
    and+ out_file
    and+ () = setup_log
    and+ symbolic_parameters
    and+ workspace in
    Cmd_haskell.sym ~entry_point ~files ~out_file ~symbolic_parameters
      ~workspace
end

(* owi llvm *)
module Llvm = struct
  let entry_point = entry_point None

  (* owi llvm run *)
  let run =
    let+ entry_point
    and+ files
    and+ out_file
    and+ () = setup_log
    and+ timeout
    and+ timeout_instr
    and+ unsafe
    and+ workspace in
    Cmd_llvm.run ~entry_point ~files ~out_file ~timeout ~timeout_instr ~unsafe
      ~workspace

  (* owi llvm sym *)
  let sym =
    let+ entry_point
    and+ files
    and+ out_file
    and+ () = setup_log
    and+ symbolic_parameters
    and+ workspace in
    Cmd_llvm.sym ~entry_point ~files ~out_file ~symbolic_parameters ~workspace
end

(* owi rust *)
module Rust = struct
  let entry_point = entry_point (Some "main")

  (* owi rust run *)
  let run =
    let+ entry_point
    and+ files
    and+ includes
    and+ opt_lvl
    and+ out_file
    and+ () = setup_log
    and+ timeout
    and+ timeout_instr
    and+ unsafe in
    Cmd_rust.run ~entry_point ~files ~includes ~opt_lvl ~out_file ~timeout
      ~timeout_instr ~unsafe

  (* owi rust sym *)
  let sym =
    let+ arch
    and+ entry_point
    and+ files
    and+ includes
    and+ opt_lvl
    and+ out_file
    and+ () = setup_log
    and+ symbolic_parameters
    and+ workspace in

    Cmd_rust.sym ~arch ~entry_point ~files ~includes ~opt_lvl ~out_file
      ~symbolic_parameters ~workspace
end

(* owi version *)
module Version = struct
  let cmd =
    let+ () = Term.const ()
    and+ () = setup_log in
    Cmd_version.cmd ()
end

(* owi wasm *)

module Wasm = struct
  let entry_point = entry_point None

  (* owi wasm abs *)
  let abs =
    let+ source_file
    and+ () = setup_log
    and+ entry_point
    and+ unsafe
    and+ debug_trace in
    Cmd_wasm_abs.cmd ~source_file ~entry_point ~unsafe ~debug_trace

  (* owi wasm inspect *)
  module Inspect = struct
    (* owi wasm inspect cfg *)
    let cfg =
      let+ source_file
      and+ entry_point
      and+ () = setup_log in
      Cmd_wasm_inspect_cfg.cmd ~source_file ~entry_point

    (* owi wasm inspect cg *)
    let cg =
      let+ call_graph_mode
      and+ source_file
      and+ entry_point
      and+ () = setup_log in
      Cmd_wasm_inspect_cg.cmd ~call_graph_mode ~source_file ~entry_point
  end

  (* owi wasm fmt *)
  let fmt =
    let+ inplace =
      let doc = "Format in-place, overwriting input file" in
      Arg.(value & flag & info [ "inplace"; "i" ] ~doc)
    and+ files
    and+ () = setup_log in
    Cmd_wasm_fmt.cmd ~inplace ~files

  (* owi wasm fuzz *)
  let fuzz =
    let+ unsafe
    and+ entry_point
    and+ rounds
    and+ timeout
    and+ timeout_instr
    and+ () = setup_log
    and+ seed
    and+ source_file in
    Cmd_wasm_fuzz.cmd ~entry_point ~rounds ~seed ~source_file ~timeout
      ~timeout_instr ~unsafe

  (* owi wasm instrument *)
  module Instrument = struct
    (* owi wasm instrument label *)
    let label =
      let+ unsafe
      and+ coverage_criteria
      and+ () = setup_log
      and+ source_file in
      Cmd_wasm_instrument_label.cmd ~unsafe ~source_file ~coverage_criteria
  end

  (* owi wasm hunt *)
  let hunt =
    let+ entry_point
    and+ rounds
    and+ seed
    and+ () = setup_log
    and+ source_file
    and+ symbolic_parameters
    and+ timeout
    and+ timeout_instr
    and+ unsafe
    and+ workspace in
    Cmd_wasm_hunt.cmd ~entry_point ~symbolic_parameters ~rounds ~seed
      ~source_file ~timeout ~timeout_instr ~unsafe ~workspace

  (* owi wasm iso *)
  let iso =
    (* TODO: this is actually almost `symbolic_parameters` (with `entry_point` removed), we should use it... it'll simplify the signature a lot! *)
    let+ deterministic_result_order
    and+ fail_mode
    and+ exploration_strategy
    and+ files
    and+ model_format
    and+ no_assert_failure_expression_printing
    and+ no_stop_at_failure
    and+ no_value
    and+ () = setup_log
    and+ seed
    and+ solver
    and+ unsafe
    and+ workers
    and+ no_worker_isolation
    and+ model_out_file
    and+ with_breadcrumbs
    and+ workspace in

    Cmd_wasm_iso.cmd ~deterministic_result_order ~fail_mode
      ~exploration_strategy ~files ~model_format
      ~no_assert_failure_expression_printing ~no_stop_at_failure ~no_value ~seed
      ~solver ~unsafe ~workers ~no_worker_isolation ~workspace ~model_out_file
      ~with_breadcrumbs

  (* owi wasm replay *)
  let replay =
    let+ unsafe
    and+ replay_file =
      let doc = "Which replay file to use" in
      Arg.(
        required
        & opt (some existing_file_conv) None
        & info [ "replay-file" ] ~doc ~docv:"FILE" )
    and+ () = setup_log
    and+ source_file
    and+ invoke_with_symbols
    and+ entry_point in
    Cmd_wasm_replay.cmd ~unsafe ~replay_file ~source_file ~entry_point
      ~invoke_with_symbols

  (* owi wasm run *)
  let run =
    let+ unsafe
    and+ timeout
    and+ timeout_instr
    and+ () = setup_log
    and+ source_file in
    Cmd_wasm_run.cmd ~unsafe ~timeout ~timeout_instr ~source_file

  (* owi wasm script *)
  module Script = struct
    (* owi wasm script abstract *)
    let abstract =
      let+ files
      and+ () = setup_log
      and+ no_exhaustion =
        let doc = "no exhaustion tests" in
        Arg.(value & flag & info [ "no-exhaustion" ] ~doc)
      and+ debug_trace in
      Cmd_wasm_script.cmd_abstract ~files ~no_exhaustion ~debug_trace

    (* owi wasm script concrete *)
    let concrete =
      let+ files
      and+ () = setup_log
      and+ no_exhaustion =
        let doc = "no exhaustion tests" in
        Arg.(value & flag & info [ "no-exhaustion" ] ~doc)
      in
      Cmd_wasm_script.cmd_concrete ~files ~no_exhaustion

    (* owi wasm script symbolic *)
    let symbolic =
      let+ files
      and+ () = setup_log
      and+ no_exhaustion =
        let doc = "no exhaustion tests" in
        Arg.(value & flag & info [ "no-exhaustion" ] ~doc)
      in
      Cmd_wasm_script.cmd_symbolic ~files ~no_exhaustion
  end

  (* owi wasm sym *)
  let sym =
    let+ entry_point
    and+ () = setup_log
    and+ source_file
    and+ symbolic_parameters
    and+ workspace in
    Cmd_wasm_sym.cmd ~entry_point ~source_file ~symbolic_parameters ~workspace

  (* owi wasm to_wat *)
  let to_wat =
    let+ source_file
    and+ emit_file =
      let doc = "Emit (.wat) files from corresponding (.wasm) files." in
      Arg.(value & flag & info [ "emit-file" ] ~doc)
    and+ () = setup_log
    and+ out_file in
    Cmd_wasm_to_wat.cmd ~source_file ~emit_file ~out_file

  (* owi wasm of_wat *)
  let of_wat =
    let+ unsafe
    and+ out_file
    and+ () = setup_log
    and+ source_file in
    Cmd_wasm_of_wat.cmd ~unsafe ~out_file ~source_file

  (* owi wasm validate *)
  let validate =
    let+ files
    and+ () = setup_log in
    Cmd_wasm_validate.cmd ~files
end

(* owi zig *)
module Zig = struct
  let entry_point = entry_point (Some "_start")

  let run =
    let+ entry_point
    and+ files
    and+ includes
    and+ out_file
    and+ timeout
    and+ timeout_instr
    and+ unsafe
    and+ workspace in
    Cmd_zig.run ~entry_point ~files ~includes ~out_file ~timeout ~timeout_instr
      ~unsafe ~workspace

  (* owi zig sym *)
  let sym =
    let+ entry_point
    and+ includes
    and+ files
    and+ out_file
    and+ () = setup_log
    and+ symbolic_parameters
    and+ workspace in
    Cmd_zig.sym ~entry_point ~files ~includes ~out_file ~symbolic_parameters
      ~workspace
end

(* owi *)

let info name doc = Cmd.info name ~doc ~version ~sdocs ~man:shared_man

let default =
  Term.(ret (const (fun (_ : _ list) -> `Help (`Plain, None)) $ copts_t))

let group name doc group = Cmd.group ~default (info name doc) group

let cmd name doc cmd = Cmd.v (info name doc) cmd

let cli =
  let owi_info =
    let doc =
      "Seamless program analysis for C, C++, Go, Haskell, LLVM, Rust, Wasm and \
       Zig."
    in
    let man =
      [ `S Manpage.s_bugs; `P "Email them to <owi.wildcat119@passmail.com>." ]
    in
    Cmd.info "owi" ~version ~doc ~sdocs ~man
  in

  Cmd.group ~default owi_info
    [ group "c" "Work with C programs."
        [ cmd "abs" "Run the abstract interpreter." C.abs
        ; cmd "fuzz" "Run the fuzzer." C.fuzz
        ; cmd "hunt"
            "Hunt bugs by combining the fuzzer and the symbolic execution \
             engine."
            C.hunt
        ; cmd "run" "Run the concrete interpreter." C.run
        ; cmd "sym" "Run the symbolic execution engine on a C program." C.sym
        ]
    ; group "c++" "Work with C++ programs."
        [ cmd "abs" "Run the abstract interpreter." Cpp.abs
        ; cmd "fuzz" "Run the fuzzer." Cpp.fuzz
        ; cmd "hunt"
            "Hunt bugs by combining the fuzzer and the symbolic execution \
             engine."
            Cpp.hunt
        ; cmd "run" "Run the concrete interpreter." Cpp.run
        ; cmd "sym" "Run the symbolic execution engine on a C++ program."
            Cpp.sym
        ]
    ; group "go" "Work with Go programs."
        [ cmd "abs" "Run the abstract interpreter." Go.abs
        ; cmd "fuzz" "Run the fuzzer." Go.fuzz
        ; cmd "hunt"
            "Hunt bugs by combining the fuzzer and the symbolic execution \
             engine."
            Go.hunt
        ; cmd "run" "Run the concrete interpreter." Go.run
        ; cmd "sym" "Run the symbolic execution engine on a Go program." Go.sym
        ]
    ; group "haskell" "Work with Haskell programs."
        [ cmd "abs" "Run the abstract interpreter." Haskell.abs
        ; cmd "fuzz" "Run the fuzzer." Haskell.fuzz
        ; cmd "hunt"
            "Hunt bugs by combining the fuzzer and the symbolic execution \
             engine."
            Haskell.hunt
        ; cmd "run" "Run the concrete interpreter." Haskell.run
        ; cmd "sym" "Run the symbolic execution engine on a Haskell program."
            Haskell.sym
        ]
    ; group "llvm" "Work with LLVM programs."
        [ cmd "run" "Run the concrete interpreter." Llvm.run
        ; cmd "sym" "Run the symbolic execution engine on a LLVM program."
            Llvm.sym
        ]
    ; group "rust" "Work with Rust programs."
        [ cmd "run" "Run the concrete interpreter." Rust.run
        ; cmd "sym" "Run the symbolic execution engine on a Rust program."
            Rust.sym
        ]
    ; cmd "version" "Print some version informations." Version.cmd
    ; group "wasm" "Work with Wasm programs."
        [ cmd "abs" "Run the abstract interpreter." Wasm.abs
        ; cmd "fmt" "Format a .wat or .wast file." Wasm.fmt
        ; cmd "fuzz" "Run the fuzzer." Wasm.fuzz
        ; cmd "hunt"
            "Hunt bugs by combining the fuzzer and the symbolic execution \
             engine."
            Wasm.hunt
        ; group "inspect" "Visualize and get statistics."
            [ cmd "cg" "Build a call graph." Wasm.Inspect.cg
            ; cmd "cfg" "Build a control-flow graph." Wasm.Inspect.cfg
            ]
        ; group "instrument" "Instrument a program in various ways."
            [ cmd "label"
                "Generate an instrumented file with labels corresponding to \
                 test objectives for a given coverage criteria."
                Wasm.Instrument.label
            ]
        ; cmd "iso"
            "Check the iso-functionnality of two modules by comparing the \
             output when calling their exports."
            Wasm.iso
        ; cmd "replay"
            "Replay a module by replacing symbols with concrete values from a \
             model."
            Wasm.replay
        ; cmd "run" "Run the concrete interpreter." Wasm.run
        ; group "script" "Run a reference test suite script (.wast)."
            [ cmd "concrete"
                "Run a reference test suite (.wast) using the concrete \
                 interpreter."
                Wasm.Script.concrete
            ; cmd "symbolic"
                "Run a reference test suite (.wast) using the symbolic \
                 interpreter."
                Wasm.Script.symbolic
            ; cmd "abstract"
                "Run a reference test suite (.wast) using the abstract \
                 interpreter."
                Wasm.Script.abstract
            ]
        ; cmd "sym" "Run the symbolic execution engine." Wasm.sym
        ; cmd "validate" "Validate a module." Wasm.validate
        ; cmd "to_wat" "Generate a text file (.wat) from a binary file (.wasm)."
            Wasm.to_wat
        ; cmd "of_wat" "Generate a binary file (.wasm) from a text file (.wat)."
            Wasm.of_wat
        ]
    ; group "zig" "Work with Zig programs."
        [ cmd "run" "Run the concrete interpreter." Zig.run
        ; cmd "sym" "Run the symbolic execution engine on a Zig program."
            Zig.sym
        ]
    ]

let exit_code =
  let open Cmd.Exit in
  match Cmd.eval_value cli with
  | Ok (`Help | `Version) -> ok
  | Ok (`Ok result) ->
    begin match result with
    | Ok () -> ok
    | Error e -> begin
      Log.err (fun m -> m "%s" (Result.err_to_string e));
      Result.err_to_exit_code e
      end
    end
  | Error (`Parse | `Term) -> cli_error
  | Error `Exn -> internal_error

let () = exit exit_code
