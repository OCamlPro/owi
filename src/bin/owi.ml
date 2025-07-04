(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Owi
open Cmdliner

(* Helpers *)

let call_graph_mode_conv =
  let of_string s =
    match String.lowercase_ascii s with
    | "complete" -> Ok Cmd_call_graph.Complete
    | "sound" -> Ok Cmd_call_graph.Sound
    | _ -> Fmt.error_msg {|Expected "complete" or "sound" but got "%s"|} s
  in
  let pp fmt = function
    | Cmd_call_graph.Complete -> Fmt.string fmt "complete"
    | Cmd_call_graph.Sound -> Fmt.string fmt "sound"
  in
  Arg.conv (of_string, pp)

let existing_file_conv =
  let parse s =
    match Fpath.of_string s with
    | Error _ as e -> e
    | Ok path -> begin
      match Bos.OS.File.exists path with
      | Ok true -> Ok path
      | Ok false -> Fmt.error_msg "no file '%a'" Fpath.pp path
      | Error _ as e -> e
    end
  in
  Arg.conv (parse, Fpath.pp)

let existing_dir_conv =
  let parse s =
    match Fpath.of_string s with
    | Error _ as e -> e
    | Ok path -> begin
      match Bos.OS.Dir.exists path with
      | Ok true -> Ok path
      | Ok false -> Fmt.error_msg "no directory '%a'" Fpath.pp path
      | Error _ as e -> e
    end
  in
  Arg.conv (parse, Fpath.pp)

let path_conv = Arg.conv (Fpath.of_string, Fpath.pp)

let solver_conv = Arg.conv (Smtml.Solver_type.of_string, Smtml.Solver_type.pp)

let exploration_conv =
  let of_string s =
    match String.lowercase_ascii s with
    | "fifo" -> Ok Cmd_sym.FIFO
    | "lifo" -> Ok Cmd_sym.LIFO
    | "random" -> Ok Cmd_sym.Random
    | _ -> Fmt.error_msg {|Expected "fifo", "lifo" or "random" but got "%s"|} s
  in
  let pp fmt = function
    | Cmd_sym.FIFO -> Fmt.string fmt "fifo"
    | Cmd_sym.LIFO -> Fmt.string fmt "lifo"
    | Cmd_sym.Random -> Fmt.string fmt "random"
  in
  Arg.conv (of_string, pp)

let model_format_conv =
  let of_string s =
    match String.lowercase_ascii s with
    | "scfg" -> Ok Cmd_utils.Scfg
    | "json" -> Ok Json
    | _ -> Fmt.error_msg {|Expected "json" or "scfg" but got "%s"|} s
  in
  let pp fmt = function
    | Cmd_utils.Scfg -> Fmt.string fmt "scfg"
    | Json -> Fmt.string fmt "json"
  in
  Arg.conv (of_string, pp)

(* Common options *)

let copts_t = Term.(const [])

let sdocs = Manpage.s_common_options

let shared_man = [ `S Manpage.s_bugs; `P "Email them to <contact@ndrs.fr>." ]

let version = Cmd_version.owi_version ()

(* Common terms *)

open Term.Syntax

let arch =
  let doc = "data model" in
  Arg.(value & opt int 32 & info [ "arch"; "m" ] ~doc)

let concolic =
  let doc = "concolic mode" in
  Arg.(value & flag & info [ "concolic" ] ~doc)

let deterministic_result_order =
  let doc =
    "Guarantee a fixed deterministic order of found failures. This implies \
     --no-stop-at-failure."
  in
  Arg.(value & flag & info [ "deterministic-result-order" ] ~doc)

let call_graph_mode =
  let doc = {| The call graph is either "complete" or "sound" |} in
  Arg.(value & opt call_graph_mode_conv Sound & info [ "call-graph-mode" ] ~doc)

let entry_point default =
  let doc = "entry point of the executable" in
  Arg.(
    value
    & opt (some string) default
    & info [ "entry-point" ] ~doc ~docv:"FUNCTION" )

let fail_mode =
  let trap_doc = "ignore assertion violations and only report traps" in
  let assert_doc = "ignore traps and only report assertion violations" in
  Arg.(
    value
    & vflag Cmd_sym.Both
        [ (Trap_only, info [ "fail-on-trap-only" ] ~doc:trap_doc)
        ; (Assertion_only, info [ "fail-on-assertion-only" ] ~doc:assert_doc)
        ] )

let exploration_strategy =
  let doc = {|exploration strategy to use ("fifo", "lifo" or "random")|} in
  Arg.(value & opt exploration_conv Cmd_sym.LIFO & info [ "exploration" ] ~doc)

let files =
  let doc = "source files" in
  Arg.(non_empty & pos_all existing_file_conv [] (info [] ~doc ~docv:"FILE"))

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

let rac =
  let doc = "runtime assertion checking mode" in
  Arg.(value & flag & info [ "rac" ] ~doc)

let solver =
  let doc = "SMT solver to use" in
  Arg.(
    value
    & opt solver_conv Smtml.Solver_type.Z3_solver
    & info [ "solver"; "s" ] ~doc )

let source_file =
  let doc = "source file" in
  Arg.(
    required & pos 0 (some existing_file_conv) None (info [] ~doc ~docv:"FILE") )

(* TODO: move this as a common option ? *)
let setup_log =
  let setup_log style_renderer level =
    Fmt_tty.setup_std_outputs ?style_renderer ();
    Logs.set_level level;
    Logs.set_reporter (Logs_fmt.reporter ())
  in
  let env = Cmd.Env.info "OWI_VERBOSITY" in
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ~env ())

let srac =
  let doc = "symbolic runtime assertion checking mode" in
  Arg.(value & flag & info [ "srac" ] ~doc)

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
    "number of workers for symbolic execution. Defaults to the number of \
     physical cores."
  in
  Arg.(
    value
    & opt int Processor.Query.core_count
    & info [ "workers"; "w" ] ~doc ~absent:"n" )

let workspace =
  let doc = "write results and intermediate compilation artifacts to dir" in
  Arg.(value & opt (some path_conv) None & info [ "workspace" ] ~doc ~docv:"DIR")

let with_breadcrumbs =
  let doc = "add breadcrumbs to the generated model" in
  Arg.(value & flag & info [ "with-breadcrumbs" ] ~doc)

(* shared symbolic parameters *)

let symbolic_parameters default_entry_point =
  let+ unsafe
  and+ rac
  and+ srac
  and+ workers
  and+ no_stop_at_failure
  and+ no_value
  and+ no_assert_failure_expression_printing
  and+ deterministic_result_order
  and+ fail_mode
  and+ exploration_strategy
  and+ workspace
  and+ solver
  and+ model_format
  and+ entry_point = entry_point default_entry_point
  and+ model_out_file
  and+ with_breadcrumbs
  and+ invoke_with_symbols in
  { Cmd_sym.unsafe
  ; rac
  ; srac
  ; workers
  ; no_stop_at_failure
  ; no_value
  ; no_assert_failure_expression_printing
  ; deterministic_result_order
  ; fail_mode
  ; exploration_strategy
  ; workspace
  ; solver
  ; model_format
  ; entry_point
  ; model_out_file
  ; with_breadcrumbs
  ; invoke_with_symbols
  }

(* owi analyze *)

let analyze_info =
  let doc = "Analyze a program in different possible ways" in
  let man = [] @ shared_man in
  Cmd.info "analyze" ~version ~doc ~sdocs ~man

(* owi c *)

let c_info =
  let doc = "Compile a C file to Wasm and run the symbolic interpreter on it" in
  let man = [] @ shared_man in
  Cmd.info "c" ~version ~doc ~sdocs ~man

let c_cmd =
  let+ arch
  and+ property =
    let doc = "property file" in
    Arg.(
      value
      & opt (some existing_file_conv) None
      & info [ "property" ] ~doc ~docv:"FILE" )
  and+ includes
  and+ opt_lvl
  and+ testcomp =
    let doc = "test-comp mode" in
    Arg.(value & flag & info [ "testcomp" ] ~doc)
  and+ concolic
  and+ files
  and+ () = setup_log
  and+ eacsl =
    let doc =
      "e-acsl mode, refer to \
       https://frama-c.com/download/e-acsl/e-acsl-implementation.pdf for \
       Frama-C's current language feature implementations"
    in
    Arg.(value & flag & info [ "e-acsl" ] ~doc)
  and+ out_file
  and+ symbolic_parameters = symbolic_parameters (Some "main") in

  Cmd_c.cmd ~symbolic_parameters ~arch ~property ~includes ~opt_lvl ~out_file
    ~testcomp ~concolic ~files ~eacsl

(* owi analyze cg *)

let cg_info =
  let doc = "Build a call graph" in

  let man = [] @ shared_man in

  Cmd.info "cg" ~version ~doc ~sdocs ~man

let cg_cmd =
  let+ call_graph_mode
  and+ source_file
  and+ entry_point = entry_point None
  and+ () = setup_log in

  Cmd_call_graph.cmd ~call_graph_mode ~source_file ~entry_point

(* owi cpp *)

let cpp_info =
  let doc =
    "Compile a C++ file to Wasm and run the symbolic interpreter on it"
  in
  let man = [] @ shared_man in
  Cmd.info "c++" ~version ~doc ~sdocs ~man

let cpp_cmd =
  let+ arch
  and+ includes
  and+ opt_lvl
  and+ concolic
  and+ files
  and+ out_file
  and+ () = setup_log
  and+ symbolic_parameters = symbolic_parameters (Some "main") in

  Cmd_cpp.cmd ~symbolic_parameters ~out_file ~arch ~includes ~opt_lvl ~concolic
    ~files

(* owi conc *)

let conc_info =
  let doc = "Run the concolic interpreter" in
  let man = [] @ shared_man in
  Cmd.info "conc" ~version ~doc ~sdocs ~man

let conc_cmd =
  let+ () = setup_log
  and+ source_file
  and+ parameters = symbolic_parameters None in

  Cmd_conc.cmd ~parameters ~source_file

(* owi fmt *)

let fmt_info =
  let doc = "Format a .wat or .wast file" in
  let man = [] @ shared_man in
  Cmd.info "fmt" ~version ~doc ~sdocs ~man

let fmt_cmd =
  let+ inplace =
    let doc = "Format in-place, overwriting input file" in
    Arg.(value & flag & info [ "inplace"; "i" ] ~doc)
  and+ files
  and+ () = setup_log in
  Cmd_fmt.cmd ~inplace ~files

(* owi instrument *)

let instrument_info =
  let doc =
    "Generate an instrumented file with runtime assertion checking coming from \
     Weasel specifications"
  in
  let man = [] @ shared_man in
  Cmd.info "instrument" ~version ~doc ~sdocs ~man

let instrument_cmd =
  let+ unsafe
  and+ symbolic =
    let doc =
      "generate instrumented module that depends on symbolic execution"
    in
    Arg.(value & flag & info [ "symbolic" ] ~doc)
  and+ () = setup_log
  and+ files in
  Cmd_instrument.cmd ~unsafe ~symbolic ~files

(* owi iso *)

let iso_info =
  let doc =
    "Check the iso-functionnality of two Wasm modules by comparing the output \
     when calling their exports."
  in
  let man = [] @ shared_man in
  Cmd.info "iso" ~version ~doc ~sdocs ~man

let iso_cmd =
  let+ deterministic_result_order
  and+ fail_mode
  and+ exploration_strategy
  and+ files
  and+ model_format
  and+ no_assert_failure_expression_printing
  and+ no_stop_at_failure
  and+ no_value
  and+ () = setup_log
  and+ solver
  and+ unsafe
  and+ workers
  and+ model_out_file
  and+ with_breadcrumbs
  and+ workspace in

  Cmd_iso.cmd ~deterministic_result_order ~fail_mode ~exploration_strategy
    ~files ~model_format ~no_assert_failure_expression_printing
    ~no_stop_at_failure ~no_value ~solver ~unsafe ~workers ~workspace
    ~model_out_file ~with_breadcrumbs

(* owi replay *)

let replay_info =
  let doc =
    "Replay a module containing symbols with concrete values in a replay file \
     containing a model"
  in
  let man = [] @ shared_man in
  Cmd.info "replay" ~version ~doc ~sdocs ~man

let replay_cmd =
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
  and+ entry_point = entry_point None in
  Cmd_replay.cmd ~unsafe ~replay_file ~source_file ~entry_point
    ~invoke_with_symbols

(* owi run *)

let run_info =
  let doc = "Run the concrete interpreter" in
  let man = [] @ shared_man in
  Cmd.info "run" ~version ~doc ~sdocs ~man

let run_cmd =
  let+ unsafe
  and+ timeout
  and+ timeout_instr
  and+ rac
  and+ () = setup_log
  and+ source_file in
  Cmd_run.cmd ~unsafe ~timeout ~timeout_instr ~rac ~source_file

(* owi rust *)

let rust_info =
  let doc =
    "Compile a Rust file to Wasm and run the symbolic interpreter on it"
  in
  let man = [] @ shared_man in
  Cmd.info "rust" ~version ~doc ~sdocs ~man

let rust_cmd =
  let+ arch
  and+ includes
  and+ opt_lvl
  and+ concolic
  and+ files
  and+ out_file
  and+ () = setup_log
  and+ symbolic_parameters = symbolic_parameters (Some "main") in

  Cmd_rust.cmd ~symbolic_parameters ~arch ~opt_lvl ~includes ~files ~concolic
    ~out_file

(* owi script *)

let script_info =
  let doc = "Run a reference test suite script" in
  let man = [] @ shared_man in
  Cmd.info "script" ~version ~doc ~sdocs ~man

let script_cmd =
  let+ files
  and+ () = setup_log
  and+ no_exhaustion =
    let doc = "no exhaustion tests" in
    Arg.(value & flag & info [ "no-exhaustion" ] ~doc)
  in
  Cmd_script.cmd ~files ~no_exhaustion

(* owi sym *)

let sym_info =
  let doc = "Run the symbolic interpreter" in
  let man = [] @ shared_man in
  Cmd.info "sym" ~version ~doc ~sdocs ~man

let sym_cmd =
  let+ source_file
  and+ () = setup_log
  and+ parameters = symbolic_parameters None in

  Cmd_sym.cmd ~parameters ~source_file

(* owi tinygo *)

let tinygo_info =
  let doc =
    "Compile a TinyGo file to Wasm and run the symbolic interpreter on it"
  in
  let man = [] @ shared_man in
  Cmd.info "tinygo" ~version ~doc ~sdocs ~man

let tinygo_cmd =
  let+ concolic
  and+ files
  and+ out_file
  and+ () = setup_log
  and+ symbolic_parameters = symbolic_parameters (Some "_start") in
  Cmd_tinygo.cmd ~symbolic_parameters ~files ~concolic ~out_file

(* owi validate *)

let validate_info =
  let doc = "Validate a module" in
  let man = [] @ shared_man in
  Cmd.info "validate" ~version ~doc ~sdocs ~man

let validate_cmd =
  let+ files
  and+ () = setup_log in
  Cmd_validate.cmd ~files

(* owi version *)

let version_info =
  let doc = "Print some version informations" in
  let man = [] @ shared_man in
  Cmd.info "version" ~version ~doc ~sdocs ~man

let version_cmd =
  let+ () = Term.const ()
  and+ () = setup_log in
  Cmd_version.cmd ()

(* owi wasm2wat *)

let wasm2wat_info =
  let doc =
    "Generate a text format file (.wat) from a binary format file (.wasm)"
  in
  let man = [] @ shared_man in
  Cmd.info "wasm2wat" ~version ~doc ~sdocs ~man

let wasm2wat_cmd =
  let+ source_file
  and+ emit_file =
    let doc = "Emit (.wat) files from corresponding (.wasm) files." in
    Arg.(value & flag & info [ "emit-file" ] ~doc)
  and+ () = setup_log
  and+ out_file in
  Cmd_wasm2wat.cmd ~source_file ~emit_file ~out_file

(* owi wat2wasm *)

let wat2wasm_info =
  let doc =
    "Generate a binary format file (.wasm) from a text format file (.wat)"
  in
  let man = [] @ shared_man in
  Cmd.info "wat2wasm" ~version ~doc ~sdocs ~man

let wat2wasm_cmd =
  let+ unsafe
  and+ out_file
  and+ () = setup_log
  and+ source_file in
  Cmd_wat2wasm.cmd ~unsafe ~out_file ~source_file

(* owi zig *)

let zig_info =
  let doc =
    "Compile a Zig file to Wasm and run the symbolic interpreter on it"
  in
  let man = [] @ shared_man in
  Cmd.info "zig" ~version ~doc ~sdocs ~man

let zig_cmd =
  let+ concolic
  and+ includes
  and+ files
  and+ out_file
  and+ () = setup_log
  and+ symbolic_parameters = symbolic_parameters (Some "_start") in
  Cmd_zig.cmd ~symbolic_parameters ~includes ~files ~concolic ~out_file

(* owi *)

let cli =
  let info =
    let doc = "OCaml WebAssembly Interpreter" in
    let sdocs = Manpage.s_common_options in
    let man = [ `S Manpage.s_bugs; `P "Email them to <contact@ndrs.fr>." ] in
    Cmd.info "owi" ~version ~doc ~sdocs ~man
  in
  let default =
    Term.(ret (const (fun (_ : _ list) -> `Help (`Plain, None)) $ copts_t))
  in
  Cmd.group info ~default
    [ Cmd.group analyze_info [Cmd.v cg_info cg_cmd]
    ; Cmd.v c_info c_cmd
    ; Cmd.v conc_info conc_cmd
    ; Cmd.v cpp_info cpp_cmd
    ; Cmd.v fmt_info fmt_cmd
    ; Cmd.v instrument_info instrument_cmd
    ; Cmd.v iso_info iso_cmd
    ; Cmd.v replay_info replay_cmd
    ; Cmd.v run_info run_cmd
    ; Cmd.v rust_info rust_cmd
    ; Cmd.v script_info script_cmd
    ; Cmd.v sym_info sym_cmd
    ; Cmd.v tinygo_info tinygo_cmd
    ; Cmd.v validate_info validate_cmd
    ; Cmd.v version_info version_cmd
    ; Cmd.v wasm2wat_info wasm2wat_cmd
    ; Cmd.v wat2wasm_info wat2wasm_cmd
    ; Cmd.v zig_info zig_cmd
    ]

let exit_code =
  let open Cmd.Exit in
  match Cmd.eval_value cli with
  | Ok (`Help | `Version) -> ok
  | Ok (`Ok result) -> begin
    match result with
    | Ok () -> ok
    | Error e -> begin
      Logs.err (fun m -> m "%s" (Result.err_to_string e));
      Result.err_to_exit_code e
    end
  end
  | Error e -> (
    match e with `Term -> 122 | `Parse -> cli_error | `Exn -> internal_error )

let () = exit exit_code
