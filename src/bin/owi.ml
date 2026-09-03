(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
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
    & vflag Symbolic_parameters.Both
        [ (Trap_only, info [ "fail-on-trap-only" ] ~doc:trap_doc)
        ; (Assertion_only, info [ "fail-on-assertion-only" ] ~doc:assert_doc)
        ] )

let exploration_strategy =
  let doc =
    {|exploration strategy to use ("fifo", "lifo", "random", "random-unseen-then-random", "rarity", "hot-path-penalty", "rarity-aging", "rarity-depth-aging", "rarity-depth-loop-aging", "rarity-depth-loop-aging-random")|}
  in
  Arg.(
    value
    & opt exploration_conv Symbolic_parameters.Exploration_strategy.FIFO
    & info [ "exploration" ] ~doc )

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

let workspace =
  let doc = "write results and intermediate compilation artifacts to dir" in
  Arg.(value & opt (some path_conv) None & info [ "workspace" ] ~doc ~docv:"DIR")

let with_breadcrumbs =
  let doc = "add breadcrumbs to the generated model" in
  Arg.(value & flag & info [ "with-breadcrumbs" ] ~doc)

let no_ite_for_select =
  let doc = "do not use ite for select" in
  Arg.(value & flag & info [ "no-ite-for-select" ] ~doc)

(* shared symbolic parameters *)

let symbolic_parameters default_entry_point =
  let+ deterministic_result_order
  and+ entry_point = entry_point default_entry_point
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
  and+ workers
  and+ workspace in
  let use_ite_for_select = not no_ite_for_select in
  { Symbolic_parameters.deterministic_result_order
  ; entry_point
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
  ; workspace
  }

(* owi c *)
module C = struct
  let cmd =
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
      ~testcomp ~files ~eacsl
end

(* owi cpp *)
module Cpp = struct
  let cmd =
    let+ arch
    and+ includes
    and+ opt_lvl
    and+ files
    and+ out_file
    and+ () = setup_log
    and+ symbolic_parameters = symbolic_parameters (Some "main") in

    Cmd_cpp.cmd ~symbolic_parameters ~out_file ~arch ~includes ~opt_lvl ~files
end

(* owi haskell *)
module Haskell = struct
  let cmd =
    let+ files
    and+ out_file
    and+ () = setup_log
    and+ symbolic_parameters = symbolic_parameters (Some "_start") in
    Cmd_haskell.cmd ~symbolic_parameters ~files ~out_file
end

(* owi llvm *)
module Llvm = struct
  let cmd =
    let+ files
    and+ out_file
    and+ () = setup_log
    and+ symbolic_parameters = symbolic_parameters None in
    Cmd_llvm.cmd ~symbolic_parameters ~files ~out_file
end

(* owi rust *)
module Rust = struct
  let cmd =
    let+ arch
    and+ includes
    and+ opt_lvl
    and+ files
    and+ out_file
    and+ () = setup_log
    and+ symbolic_parameters = symbolic_parameters (Some "main") in

    Cmd_rust.cmd ~symbolic_parameters ~arch ~opt_lvl ~includes ~files ~out_file
end

(* owi go *)
module Go = struct
  let cmd =
    let+ files
    and+ out_file
    and+ () = setup_log
    and+ symbolic_parameters = symbolic_parameters (Some "_start") in
    Cmd_tinygo.cmd ~symbolic_parameters ~files ~out_file
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
  (* owi wasm abs *)
  module Abs = struct
    let cmd =
      let+ source_file
      and+ () = setup_log
      and+ entry_point = entry_point None
      and+ unsafe in
      Cmd_abs.cmd ~source_file ~entry_point ~unsafe
  end

  (* owi wasm analyze *)
  module Analyze = struct
    (* owi wasm analyze cfg *)
    module Cfg = struct
      let cmd =
        let+ source_file
        and+ entry_point = entry_point None
        and+ () = setup_log in
        Cmd_cfg.cmd ~source_file ~entry_point
    end

    (* owi wasm analyze cg *)
    module Cg = struct
      let cmd =
        let+ call_graph_mode
        and+ source_file
        and+ entry_point = entry_point None
        and+ () = setup_log in
        Cmd_call_graph.cmd ~call_graph_mode ~source_file ~entry_point
    end
  end

  (* owi wasm fmt *)
  module Fmt = struct
    let cmd =
      let+ inplace =
        let doc = "Format in-place, overwriting input file" in
        Arg.(value & flag & info [ "inplace"; "i" ] ~doc)
      and+ files
      and+ () = setup_log in
      Cmd_fmt.cmd ~inplace ~files
  end

  (* owi wasm fuzz *)
  module Fuzz = struct
    let cmd =
      let+ unsafe
      and+ rounds
      and+ timeout
      and+ timeout_instr
      and+ () = setup_log
      and+ seed
      and+ source_file in
      Cmd_fuzz.cmd ~rounds ~seed ~source_file ~timeout ~timeout_instr ~unsafe
  end

  (* owi wasm instrument *)
  module Instrument = struct
    (* owi wasm instrument label *)
    module Label = struct
      let cmd =
        let+ unsafe
        and+ coverage_criteria
        and+ () = setup_log
        and+ source_file in
        Cmd_instrument_label.cmd ~unsafe ~source_file ~coverage_criteria
    end
  end

  (* owi wasm iso *)
  module Iso = struct
    let cmd =
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

      Cmd_iso.cmd ~deterministic_result_order ~fail_mode ~exploration_strategy
        ~files ~model_format ~no_assert_failure_expression_printing
        ~no_stop_at_failure ~no_value ~seed ~solver ~unsafe ~workers
        ~no_worker_isolation ~workspace ~model_out_file ~with_breadcrumbs
  end

  (* owi wasm replay *)
  module Replay = struct
    let cmd =
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
  end

  (* owi wasm run *)
  module Run = struct
    let cmd =
      let+ unsafe
      and+ timeout
      and+ timeout_instr
      and+ () = setup_log
      and+ source_file in
      Cmd_run.cmd ~unsafe ~timeout ~timeout_instr ~source_file
  end

  (* owi wasm script *)
  module Script = struct
    (* owi wasm script abstract *)
    module Abstract = struct
      let cmd =
        let+ files
        and+ () = setup_log
        and+ no_exhaustion =
          let doc = "no exhaustion tests" in
          Arg.(value & flag & info [ "no-exhaustion" ] ~doc)
        in
        Cmd_script.cmd_abstract ~files ~no_exhaustion
    end

    (* owi wasm script concrete *)
    module Concrete = struct
      let cmd =
        let+ files
        and+ () = setup_log
        and+ no_exhaustion =
          let doc = "no exhaustion tests" in
          Arg.(value & flag & info [ "no-exhaustion" ] ~doc)
        in
        Cmd_script.cmd_concrete ~files ~no_exhaustion
    end

    (* owi wasm script symbolic *)
    module Symbolic = struct
      let cmd =
        let+ files
        and+ () = setup_log
        and+ no_exhaustion =
          let doc = "no exhaustion tests" in
          Arg.(value & flag & info [ "no-exhaustion" ] ~doc)
        in
        Cmd_script.cmd_symbolic ~files ~no_exhaustion
    end
  end

  (* owi wasm sym *)
  module Sym = struct
    let cmd =
      let+ source_file
      and+ () = setup_log
      and+ parameters = symbolic_parameters None in
      Cmd_sym.cmd ~parameters ~source_file
  end

  (* owi wasm to_wat *)
  module To_wat = struct
    let cmd =
      let+ source_file
      and+ emit_file =
        let doc = "Emit (.wat) files from corresponding (.wasm) files." in
        Arg.(value & flag & info [ "emit-file" ] ~doc)
      and+ () = setup_log
      and+ out_file in
      Cmd_wasm2wat.cmd ~source_file ~emit_file ~out_file
  end

  (* owi wasm of_wat *)
  module Of_wat = struct
    let cmd =
      let+ unsafe
      and+ out_file
      and+ () = setup_log
      and+ source_file in
      Cmd_wat2wasm.cmd ~unsafe ~out_file ~source_file
  end

  (* owi wasm validate *)
  module Validate = struct
    let cmd =
      let+ files
      and+ () = setup_log in
      Cmd_validate.cmd ~files
  end
end

(* owi zig *)
module Zig = struct
  let cmd =
    let+ includes
    and+ files
    and+ out_file
    and+ () = setup_log
    and+ symbolic_parameters = symbolic_parameters (Some "_start") in
    Cmd_zig.cmd ~symbolic_parameters ~includes ~files ~out_file
end

(* owi *)

let info name ~doc = Cmd.info name ~doc ~version ~sdocs ~man:shared_man

let cli =
  let owi_info =
    let doc = "Seamless program analysis for C, C++, Go, Rust, Wasm and Zig." in
    let man =
      [ `S Manpage.s_bugs; `P "Email them to <owi.wildcat119@passmail.com>." ]
    in
    Cmd.info "owi" ~version ~doc ~sdocs ~man
  in
  let default =
    Term.(ret (const (fun (_ : _ list) -> `Help (`Plain, None)) $ copts_t))
  in

  Cmd.group owi_info ~default
    [ Cmd.v
        (info "c" ~doc:"Run the symbolic execution engine on a C program.")
        C.cmd
    ; Cmd.v
        (info "c++" ~doc:"Run the symbolic execution engine on a C++ program.")
        Cpp.cmd
    ; Cmd.v
        (info "go" ~doc:"Run the symbolic execution engine on a Go program.")
        Go.cmd
    ; Cmd.v
        (info "haskell"
           ~doc:"Run the symbolic execution engine on a Haskell program." )
        Haskell.cmd
    ; Cmd.v
        (info "llvm" ~doc:"Run the symbolic execution engine on a LLVM program.")
        Llvm.cmd
    ; Cmd.v
        (info "rust" ~doc:"Run the symbolic execution engine on a Rust program.")
        Rust.cmd
    ; Cmd.v (info "version" ~doc:"Print some version informations.") Version.cmd
    ; Cmd.group
        (info "wasm" ~doc:"Work with Wasm programs.")
        ~default
        [ Cmd.v (info "abs" ~doc:"Run the abstract interpreter.") Wasm.Abs.cmd
        ; Cmd.group
            (info "analyze" ~doc:"Visualize and get statistics.")
            [ Cmd.v (info "cg" ~doc:"Build a call graph.") Wasm.Analyze.Cg.cmd
            ; Cmd.v
                (info "cfg" ~doc:"Build a control-flow graph.")
                Wasm.Analyze.Cfg.cmd
            ]
        ; Cmd.v (info "fmt" ~doc:"Format a .wat or .wast file.") Wasm.Fmt.cmd
        ; Cmd.v (info "fuzz" ~doc:"Run the fuzzer.") Wasm.Fuzz.cmd
        ; Cmd.group
            (info "instrument" ~doc:"Instrument a program in various ways.")
            [ Cmd.v
                (info "label"
                   ~doc:
                     "Generate an instrumented file with labels corresponding \
                      to test objectives for a given coverage criteria." )
                Wasm.Instrument.Label.cmd
            ]
        ; Cmd.v
            (info "iso"
               ~doc:
                 "Check the iso-functionnality of two modules by comparing the \
                  output when calling their exports." )
            Wasm.Iso.cmd
        ; Cmd.v
            (info "replay"
               ~doc:
                 "Replay a module by replacing symbols with concrete values \
                  from a model." )
            Wasm.Replay.cmd
        ; Cmd.v (info "run" ~doc:"Run the concrete interpreter.") Wasm.Run.cmd
        ; Cmd.group
            (info "script" ~doc:"Run a reference test suite script (.wast).")
            [ Cmd.v
                (info "concrete"
                   ~doc:
                     "Run a reference test suite (.wast) using the concrete \
                      interpreter." )
                Wasm.Script.Concrete.cmd
            ; Cmd.v
                (info "symbolic"
                   ~doc:
                     "Run a reference test suite (.wast) using the symbolic \
                      interpreter." )
                Wasm.Script.Symbolic.cmd
            ; Cmd.v
                (info "abstract"
                   ~doc:
                     "Run a reference test suite (.wast) using the abstract \
                      interpreter." )
                Wasm.Script.Abstract.cmd
            ]
        ; Cmd.v
            (info "sym" ~doc:"Run the symbolic execution engine.")
            Wasm.Sym.cmd
        ; Cmd.v (info "validate" ~doc:"Validate a module.") Wasm.Validate.cmd
        ; Cmd.v
            (info "to_wat"
               ~doc:"Generate a text file (.wat) from a binary file (.wasm)." )
            Wasm.To_wat.cmd
        ; Cmd.v
            (info "of_wat"
               ~doc:"Generate a binary file (.wasm) from a text file (.wat)." )
            Wasm.Of_wat.cmd
        ]
    ; Cmd.v
        (info "zig" ~doc:"Run the symbolic execution engine on a Zig program.")
        Zig.cmd
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
