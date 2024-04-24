(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Owi

let debug =
  let doc = "debug mode" in
  Cmdliner.Arg.(value & flag & info [ "debug"; "d" ] ~doc)

let existing_non_dir_file =
  let parse s =
    let path = Fpath.v s in
    match Bos.OS.File.exists path with
    | Ok true -> `Ok path
    | Ok false -> `Error (Format.asprintf "no file '%a'" Fpath.pp path)
    | Error (`Msg s) -> `Error s
  in
  (parse, Fpath.pp)

let dir_file =
  let parse s = `Ok (Fpath.v s) in
  (parse, Fpath.pp)

let files =
  let doc = "source files" in
  let f = existing_non_dir_file in
  Cmdliner.Arg.(value & pos_all f [] (info [] ~doc))

let no_exhaustion =
  let doc = "no exhaustion tests" in
  Cmdliner.Arg.(value & flag & info [ "no-exhaustion" ] ~doc)

let no_stop_at_failure =
  let doc = "do not stop when a program failure is encountered" in
  Cmdliner.Arg.(value & flag & info [ "no-stop-at-failure" ] ~doc)

let no_values =
  let doc = "do not display a value for each symbol" in
  Cmdliner.Arg.(value & flag & info [ "no-value" ] ~doc)

let deterministic_result_order =
  let doc =
    "Guarantee a fixed deterministic order of found failures. This implies \
     --no-stop-at-failure."
  in
  Cmdliner.Arg.(value & flag & info [ "deterministic-result-order" ] ~doc)

let optimize =
  let doc = "optimize mode" in
  Cmdliner.Arg.(value & flag & info [ "optimize" ] ~doc)

let profiling =
  let doc = "profiling mode" in
  Cmdliner.Arg.(value & flag & info [ "profiling"; "p" ] ~doc)

let unsafe =
  let doc = "skip typechecking pass" in
  Cmdliner.Arg.(value & flag & info [ "unsafe"; "u" ] ~doc)

let workers =
  let doc =
    "number of workers for symbolic execution. Defaults to a machine-specific \
     value given by the OCaml Domain.recommended_domain_count function."
  in
  Cmdliner.Arg.(
    value
    & opt int (Domain.recommended_domain_count ())
    & info [ "workers"; "w" ] ~doc ~absent:"n" )

let workspace =
  let doc = "path to the workspace directory" in
  Cmdliner.Arg.(
    value & opt dir_file (Fpath.v "owi-out") & info [ "workspace" ] ~doc )

let copts_t = Cmdliner.Term.(const [])

let sdocs = Cmdliner.Manpage.s_common_options

let shared_man =
  [ `S Cmdliner.Manpage.s_bugs; `P "Email them to <contact@ndrs.fr>." ]

let version = "%%VERSION%%"

let c_cmd =
  let open Cmdliner in
  let info =
    let doc =
      "Compile a C file to Wasm and run the symbolic interpreter on it"
    in
    let man = [] @ shared_man in
    Cmd.info "c" ~version ~doc ~sdocs ~man
  in
  let arch =
    let doc = "data model" in
    Arg.(value & opt int 32 & info [ "arch"; "m" ] ~doc)
  in
  let property =
    let doc = "property file" in
    Arg.(value & opt (some string) None & info [ "property" ] ~doc)
  in
  let includes =
    let doc = "headers path" in
    Arg.(value & opt_all dir_file [] & info [ "I" ] ~doc)
  in
  let opt_lvl =
    let doc = "specify which optimization level to use" in
    Arg.(value & opt string "0" & info [ "O" ] ~doc)
  in
  let testcomp =
    let doc = "test-comp mode" in
    Arg.(value & flag & info [ "testcomp" ] ~doc)
  in
  let output =
    let doc = "write results to dir" in
    Arg.(value & opt string "owi-out" & info [ "output"; "o" ] ~doc)
  in
  Cmd.v info
    Term.(
      const Cmd_c.cmd $ debug $ arch $ property $ testcomp $ output $ workers
      $ opt_lvl $ includes $ files $ profiling $ unsafe $ optimize
      $ no_stop_at_failure $ no_values $ deterministic_result_order )

let fmt_cmd =
  let open Cmdliner in
  let info =
    let doc = "Format a .wat or .wast file" in
    let man = [] @ shared_man in
    Cmd.info "fmt" ~version ~doc ~sdocs ~man
  in
  let inplace =
    let doc = "Format in-place, overwriting input file" in
    Cmdliner.Arg.(value & flag & info [ "inplace"; "i" ] ~doc)
  in
  Cmd.v info Term.(const Cmd_fmt.cmd $ inplace $ files)

let opt_cmd =
  let open Cmdliner in
  let info =
    let doc = "Optimize a module" in
    let man = [] @ shared_man in
    Cmd.info "opt" ~version ~doc ~sdocs ~man
  in
  Cmd.v info Term.(const Cmd_opt.cmd $ debug $ unsafe $ files)

let run_cmd =
  let open Cmdliner in
  let info =
    let doc = "Run the concrete interpreter" in
    let man = [] @ shared_man in
    Cmd.info "run" ~version ~doc ~sdocs ~man
  in
  Cmd.v info
    Term.(const Cmd_run.cmd $ profiling $ debug $ unsafe $ optimize $ files)

let validate_cmd =
  let open Cmdliner in
  let info =
    let doc = "Validate a module" in
    let man = [] @ shared_man in
    Cmd.info "validate" ~version ~doc ~sdocs ~man
  in
  Cmd.v info Term.(const Cmd_validate.cmd $ debug $ files)

let script_cmd =
  let open Cmdliner in
  let info =
    let doc = "Run a reference test suite script" in
    let man = [] @ shared_man in
    Cmd.info "script" ~version ~doc ~sdocs ~man
  in
  Cmd.v info
    Term.(
      const Cmd_script.cmd $ profiling $ debug $ optimize $ files
      $ no_exhaustion )

let sym_cmd =
  let open Cmdliner in
  let info =
    let doc = "Run the symbolic interpreter" in
    let man = [] @ shared_man in
    Cmd.info "sym" ~version ~doc ~sdocs ~man
  in
  Cmd.v info
    Term.(
      const Cmd_sym.cmd $ profiling $ debug $ unsafe $ optimize $ workers
      $ no_stop_at_failure $ no_values $ deterministic_result_order $ workspace
      $ files )

let wasm2wat_cmd =
  let open Cmdliner in
  let info =
    let doc =
      "Generate a text format file (.wat) file from a binary format file \
       (.wasm)"
    in
    let man = [] @ shared_man in
    Cmd.info "wasm2wat" ~version ~doc ~sdocs ~man
  in
  Cmd.v info Term.(const Cmd_wasm2wat.cmd $ files)

let cli =
  let open Cmdliner in
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
    [ c_cmd
    ; fmt_cmd
    ; opt_cmd
    ; run_cmd
    ; script_cmd
    ; sym_cmd
    ; validate_cmd
    ; wasm2wat_cmd
    ]

let exit_code =
  let open Cmdliner.Cmd.Exit in
  match Cmdliner.Cmd.eval_value cli with
  | Ok (`Help | `Version) -> ok
  | Ok (`Ok result) -> begin
    match result with
    | Ok () -> ok
    | Error e -> begin
      Format.pp_err "%s" (Result.err_to_string e);
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
      | `Unexpected_token -> 40
      | `Unknown_function _id -> 41
      | `Unknown_global -> 42
      | `Unknown_import -> 43
      | `Unknown_label -> 44
      | `Unknown_local _id -> 45
      | `Unknown_memory _id -> 46
      | `Unknown_module _id -> 47
      | `Unknown_operator -> 48
      | `Unknown_type -> 49
      | `Unsupported_file_extension _ext -> 50
      | `Wrong_memory_id _id -> 51
    end
  end
  | Error e -> (
    match e with `Term -> 122 | `Parse -> cli_error | `Exn -> internal_error )

let () = exit exit_code
