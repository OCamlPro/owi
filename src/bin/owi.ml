(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Owi
open Cmdliner

(* Helpers *)

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

(* Common options *)

let copts_t = Term.(const [])

let sdocs = Manpage.s_common_options

let shared_man = [ `S Manpage.s_bugs; `P "Email them to <contact@ndrs.fr>." ]

let version = "%%VERSION%%"

(* Common terms *)

open Term.Syntax

let debug =
  let doc = "debug mode" in
  Arg.(value & flag & info [ "debug"; "d" ] ~doc)

let deterministic_result_order =
  let doc =
    "Guarantee a fixed deterministic order of found failures. This implies \
     --no-stop-at-failure."
  in
  Arg.(value & flag & info [ "deterministic-result-order" ] ~doc)

let files =
  let doc = "source files" in
  Arg.(value & pos_all existing_file_conv [] (info [] ~doc))

let source_file =
  let doc = "source file" in
  Arg.(required & pos 0 (some existing_file_conv) None (info [] ~doc))

let out_file =
  let doc = "Write output to a file." in
  Arg.(
    value & opt (some path_conv) None & info [ "o"; "output" ] ~docv:"FILE" ~doc )

let profile =
  let doc = "Profile file." in
  Arg.(value & opt (some path_conv) None & info [ "profile" ] ~docv:"FILE" ~doc)

let no_stop_at_failure =
  let doc = "do not stop when a program failure is encountered" in
  Arg.(value & flag & info [ "no-stop-at-failure" ] ~doc)

let no_value =
  let doc = "do not display a value for each symbol" in
  Arg.(value & flag & info [ "no-value" ] ~doc)

let no_assert_failure_expression_printing =
  let doc = "do not display the expression in the assert failure" in
  Arg.(value & flag & info [ "no-assert-failure-expression-printing" ] ~doc)

let fail_mode =
  let trap_doc = "ignore assertion violations and only report traps" in
  let assert_doc = "ignore traps and only report assertion violations" in
  Arg.(
    value
    & vflag `Both
        [ (`Trap_only, info [ "fail-on-trap-only" ] ~doc:trap_doc)
        ; (`Assertion_only, info [ "fail-on-assertion-only" ] ~doc:assert_doc)
        ] )

let optimize =
  let doc = "optimize mode" in
  Arg.(value & flag & info [ "optimize" ] ~doc)

let profiling =
  let doc = "profiling mode" in
  Arg.(value & flag & info [ "profiling"; "p" ] ~doc)

let rac =
  let doc = "runtime assertion checking mode" in
  Arg.(value & flag & info [ "rac" ] ~doc)

let srac =
  let doc = "symbolic runtime assertion checking mode" in
  Arg.(value & flag & info [ "srac" ] ~doc)

let solver =
  let doc = "SMT solver to use" in
  Arg.(
    value
    & opt solver_conv Smtml.Solver_type.Z3_solver
    & info [ "solver"; "s" ] ~doc )

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
  let doc = "write results to dir" in
  Arg.(value & opt path_conv (Fpath.v "owi-out") & info [ "outpt"; "o" ] ~doc)

(* owi cpp *)

let cpp_info =
  let doc =
    "Compile a C++ file to Wasm and run the symbolic interpreter on it"
  in
  let man = [] @ shared_man in
  Cmd.info "c++" ~version ~doc ~sdocs ~man

let cpp_cmd =
  let+ arch =
    let doc = "data model" in
    Arg.(value & opt int 32 & info [ "arch"; "m" ] ~doc)
  and+ includes =
    let doc = "headers path" in
    Arg.(value & opt_all existing_dir_conv [] & info [ "I" ] ~doc)
  and+ opt_lvl =
    let doc = "specify which optimization level to use" in
    Arg.(value & opt string "3" & info [ "O" ] ~doc)
  and+ concolic =
    let doc = "concolic mode" in
    Arg.(value & flag & info [ "concolic" ] ~doc)
  and+ debug
  and+ workers
  and+ files
  and+ profiling
  and+ unsafe
  and+ optimize
  and+ no_stop_at_failure
  and+ no_value
  and+ no_assert_failure_expression_printing
  and+ deterministic_result_order
  and+ fail_mode
  and+ solver
  and+ profile in
  Cmd_cpp.cmd ~debug ~arch ~workers ~opt_lvl ~includes ~files ~profiling ~unsafe
    ~optimize ~no_stop_at_failure ~no_value
    ~no_assert_failure_expression_printing ~deterministic_result_order
    ~fail_mode ~concolic ~solver ~profile

(* owi c *)

let c_info =
  let doc = "Compile a C file to Wasm and run the symbolic interpreter on it" in
  let man = [] @ shared_man in
  Cmd.info "c" ~version ~doc ~sdocs ~man

let c_cmd =
  let+ arch =
    let doc = "data model" in
    Arg.(value & opt int 32 & info [ "arch"; "m" ] ~doc)
  and+ property =
    let doc = "property file" in
    Arg.(value & opt (some existing_file_conv) None & info [ "property" ] ~doc)
  and+ includes =
    let doc = "headers path" in
    Arg.(value & opt_all existing_dir_conv [] & info [ "I" ] ~doc)
  and+ opt_lvl =
    let doc = "specify which optimization level to use" in
    Arg.(value & opt string "3" & info [ "O" ] ~doc)
  and+ testcomp =
    let doc = "test-comp mode" in
    Arg.(value & flag & info [ "testcomp" ] ~doc)
  and+ workspace
  and+ concolic =
    let doc = "concolic mode" in
    Arg.(value & flag & info [ "concolic" ] ~doc)
  and+ debug
  and+ workers
  and+ files
  and+ profiling
  and+ unsafe
  and+ optimize
  and+ no_stop_at_failure
  and+ no_value
  and+ no_assert_failure_expression_printing
  and+ deterministic_result_order
  and+ fail_mode
  and+ profile
  and+ eacsl =
    let doc =
      "e-acsl mode, refer to \
       https://frama-c.com/download/e-acsl/e-acsl-implementation.pdf for \
       Frama-C's current language feature implementations"
    in
    Arg.(value & flag & info [ "e-acsl" ] ~doc)
  and+ solver in
  Cmd_c.cmd ~debug ~arch ~property ~testcomp ~workspace ~workers ~opt_lvl
    ~includes ~files ~profiling ~unsafe ~optimize ~no_stop_at_failure ~no_value
    ~no_assert_failure_expression_printing ~deterministic_result_order
    ~fail_mode ~concolic ~eacsl ~solver ~profile

(* owi fmt *)

let fmt_info =
  let doc = "Format a .wat or .wast file" in
  let man = [] @ shared_man in
  Cmd.info "fmt" ~version ~doc ~sdocs ~man

let fmt_cmd =
  let+ inplace =
    let doc = "Format in-place, overwriting input file" in
    Arg.(value & flag & info [ "inplace"; "i" ] ~doc)
  and+ files in
  Cmd_fmt.cmd ~inplace ~files

(* owi opt *)

let opt_info =
  let doc = "Optimize a module" in
  let man = [] @ shared_man in
  Cmd.info "opt" ~version ~doc ~sdocs ~man

let opt_cmd =
  let+ debug
  and+ unsafe
  and+ source_file
  and+ out_file in
  Cmd_opt.cmd ~debug ~unsafe ~source_file ~out_file

(* owi instrument *)

let instrument_info =
  let doc =
    "Generate an instrumented file with runtime assertion checking coming from \
     Weasel specifications"
  in
  let man = [] @ shared_man in
  Cmd.info "instrument" ~version ~doc ~sdocs ~man

let instrument_cmd =
  let+ debug
  and+ unsafe
  and+ symbolic =
    let doc =
      "generate instrumented module that depends on symbolic execution"
    in
    Arg.(value & flag & info [ "symbolic" ] ~doc)
  and+ files in
  Cmd_instrument.cmd ~debug ~unsafe ~symbolic ~files

(* owi run *)

let run_info =
  let doc = "Run the concrete interpreter" in
  let man = [] @ shared_man in
  Cmd.info "run" ~version ~doc ~sdocs ~man

let run_cmd =
  let+ profiling
  and+ debug
  and+ unsafe
  and+ rac
  and+ optimize
  and+ files in
  Cmd_run.cmd ~profiling ~debug ~unsafe ~rac ~optimize ~files

(* owi rust *)

let rust_info =
  let doc =
    "Compile a Rust file to Wasm and run the symbolic interpreter on it"
  in
  let man = [] @ shared_man in
  Cmd.info "rust" ~version ~doc ~sdocs ~man

let rust_cmd =
  let+ arch =
    let doc = "data model" in
    Arg.(value & opt int 32 & info [ "arch"; "m" ] ~doc)
  and+ includes =
    let doc = "headers path" in
    Arg.(value & opt_all existing_dir_conv [] & info [ "I" ] ~doc)
  and+ opt_lvl =
    let doc = "specify which optimization level to use" in
    Arg.(value & opt string "3" & info [ "O" ] ~doc)
  and+ concolic =
    let doc = "concolic mode" in
    Arg.(value & flag & info [ "concolic" ] ~doc)
  and+ debug
  and+ workers
  and+ files
  and+ profiling
  and+ unsafe
  and+ optimize
  and+ no_stop_at_failure
  and+ no_value
  and+ no_assert_failure_expression_printing
  and+ deterministic_result_order
  and+ fail_mode
  and+ solver
  and+ profile in
  Cmd_rust.cmd ~debug ~arch ~workers ~opt_lvl ~includes ~files ~profiling
    ~unsafe ~optimize ~no_stop_at_failure ~no_value
    ~no_assert_failure_expression_printing ~deterministic_result_order
    ~fail_mode ~concolic ~solver ~profile

(* owi validate *)

let validate_info =
  let doc = "Validate a module" in
  let man = [] @ shared_man in
  Cmd.info "validate" ~version ~doc ~sdocs ~man

let validate_cmd =
  let+ debug
  and+ files in
  Cmd_validate.cmd ~debug ~files

(* owi script *)

let script_info =
  let doc = "Run a reference test suite script" in
  let man = [] @ shared_man in
  Cmd.info "script" ~version ~doc ~sdocs ~man

let script_cmd =
  let+ profiling
  and+ debug
  and+ optimize
  and+ files
  and+ no_exhaustion =
    let doc = "no exhaustion tests" in
    Arg.(value & flag & info [ "no-exhaustion" ] ~doc)
  in
  Cmd_script.cmd ~profiling ~debug ~optimize ~files ~no_exhaustion

(* owi sym *)

let sym_info =
  let doc = "Run the symbolic interpreter" in
  let man = [] @ shared_man in
  Cmd.info "sym" ~version ~doc ~sdocs ~man

let sym_cmd =
  let+ profiling
  and+ debug
  and+ unsafe
  and+ rac
  and+ srac
  and+ optimize
  and+ workers
  and+ no_stop_at_failure
  and+ no_value
  and+ no_assert_failure_expression_printing
  and+ deterministic_result_order
  and+ fail_mode
  and+ workspace
  and+ solver
  and+ files
  and+ profile in
  Cmd_sym.cmd ~profiling ~debug ~unsafe ~rac ~srac ~optimize ~workers
    ~no_stop_at_failure ~no_value ~no_assert_failure_expression_printing
    ~deterministic_result_order ~fail_mode ~workspace ~solver ~files ~profile

(* owi replay *)

let replay_info =
  let doc =
    "Replay a module containing symbols with concrete values in a replay file \
     containing a model"
  in
  let man = [] @ shared_man in
  Cmd.info "replay" ~version ~doc ~sdocs ~man

let replay_cmd =
  let+ profiling
  and+ debug
  and+ unsafe
  and+ optimize
  and+ replay_file =
    let doc = "Which replay file to use" in
    Arg.(
      required
      & opt (some existing_file_conv) None
      & info [ "replay-file" ] ~doc )
  and+ source_file in
  Cmd_replay.cmd ~profiling ~debug ~unsafe ~optimize ~replay_file ~source_file

(* owi conc *)

let conc_info =
  let doc = "Run the concolic interpreter" in
  let man = [] @ shared_man in
  Cmd.info "conc" ~version ~doc ~sdocs ~man

let conc_cmd =
  let+ profiling
  and+ debug
  and+ unsafe
  and+ rac
  and+ srac
  and+ optimize
  and+ workers
  and+ no_stop_at_failure
  and+ no_value
  and+ no_assert_failure_expression_printing
  and+ deterministic_result_order
  and+ fail_mode
  and+ workspace
  and+ solver
  and+ files
  and+ profile in
  Cmd_conc.cmd ~profiling ~debug ~unsafe ~rac ~srac ~optimize ~workers
    ~no_stop_at_failure ~no_value ~no_assert_failure_expression_printing
    ~deterministic_result_order ~fail_mode ~workspace ~solver ~files ~profile

(* owi version *)

let version_info =
  let doc = "Print some version informations" in
  let man = [] @ shared_man in
  Cmd.info "version" ~version ~doc ~sdocs ~man

let version_cmd =
  let+ () = Term.const () in
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
  let+ profiling
  and+ debug
  and+ unsafe
  and+ optimize
  and+ out_file
  and+ source_file in
  Cmd_wat2wasm.cmd ~profiling ~debug ~unsafe ~optimize ~out_file ~source_file

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
    [ Cmd.v c_info c_cmd
    ; Cmd.v cpp_info cpp_cmd
    ; Cmd.v fmt_info fmt_cmd
    ; Cmd.v opt_info opt_cmd
    ; Cmd.v instrument_info instrument_cmd
    ; Cmd.v replay_info replay_cmd
    ; Cmd.v run_info run_cmd
    ; Cmd.v rust_info rust_cmd
    ; Cmd.v script_info script_cmd
    ; Cmd.v sym_info sym_cmd
    ; Cmd.v conc_info conc_cmd
    ; Cmd.v validate_info validate_cmd
    ; Cmd.v version_info version_cmd
    ; Cmd.v wasm2wat_info wasm2wat_cmd
    ; Cmd.v wat2wasm_info wat2wasm_cmd
    ]

let exit_code =
  let open Cmd.Exit in
  match Cmd.eval_value cli with
  | Ok (`Help | `Version) -> ok
  | Ok (`Ok result) -> begin
    match result with
    | Ok () -> ok
    | Error e -> begin
      Fmt.epr "%s" (Result.err_to_string e);
      match e with
      | `No_error -> ok
      | `Alignment_too_large -> 1
      | `Assert_failure -> 2
      | `Bad_result -> 3
      | `Call_stack_exhausted -> 4
      | `Constant_expression_required -> 5
      | `Constant_out_of_range -> 6
      | `Did_not_fail_but_expected _ -> 7
      | `Duplicate_export_name -> 8
      | `Duplicate_global _id -> 9
      | `Duplicate_local _id -> 10
      | `Duplicate_memory _id -> 11
      | `Duplicate_table _id -> 12
      | `Found_bug _count -> 13
      | `Global_is_immutable -> 14
      | `Illegal_escape _txt -> 15
      | `Import_after_function -> 16
      | `Import_after_global -> 17
      | `Import_after_memory -> 18
      | `Import_after_table -> 19
      | `Incompatible_import_type -> 20
      | `Inline_function_type -> 21
      | `Invalid_result_arity -> 22
      | `Lexer_illegal_character _c -> 23
      | `Lexer_unknown_operator _op -> 23
      | `Malformed_utf8_encoding _txt -> 24
      | `Memory_size_too_large -> 25
      | `Msg _msg -> 26
      | `Multiple_memories -> 27
      | `Multiple_start_sections -> 28
      | `Parse_fail _txt -> 30
      | `Size_minimum_greater_than_maximum -> 31
      | `Start_function -> 32
      | `Timeout -> 33
      | `Trap _t -> 34
      | `Type_mismatch _msg -> 35
      | `Unbound_last_module -> 36
      | `Unbound_module _id -> 37
      | `Unbound_name _id -> 38
      | `Undeclared_function_reference -> 39
      | `Unexpected_token _token -> 40
      | `Unknown_data _id -> 41
      | `Unknown_elem _id -> 42
      | `Unknown_func _id -> 43
      | `Unknown_global _id -> 44
      | `Unknown_import _ -> 45
      | `Unknown_label _id -> 46
      | `Unknown_local _id -> 47
      | `Unknown_memory _id -> 48
      | `Unknown_module _id -> 49
      | `Unknown_operator -> 50
      | `Unknown_table _id -> 51
      | `Unknown_type _id -> 52
      | `Unsupported_file_extension _ext -> 53
      | `Failed_with_but_expected (_got, _expected) -> 54
      | `Spec_invalid_int32 _i32 -> 56
      | `Spec_invalid_int64 _i64 -> 57
      | `Spec_invalid_float32 _f32 -> 58
      | `Spec_invalid_float64 _f64 -> 59
      | `Spec_invalid_indice _id -> 60
      | `Spec_invalid_text_indice _id -> 61
      | `Unknown_annotation_clause _s -> 62
      | `Unknown_annotation_object _s -> 63
      | `Spec_unknown_binder _id -> 64
      | `Spec_unknown_param _id -> 65
      | `Spec_unknown_variable _id -> 66
      | `Spec_unknown_binder_type _s -> 67
      | `Spec_unknown_prop _pr -> 68
      | `Spec_unknown_term _tm -> 69
      | `Spec_type_error _str -> 70
      | `Contract_unknown_func _id -> 71
      | `Empty_annotation_id -> 72
      | `Empty_identifier -> 73
      | `Unclosed_annotation -> 74
      | `Unclosed_comment -> 75
      | `Unclosed_string -> 76
      | `Unbounded_quantification -> 77
      | `Invalid_model _msg -> 78
    end
  end
  | Error e -> (
    match e with `Term -> 122 | `Parse -> cli_error | `Exn -> internal_error )

let () = exit exit_code
