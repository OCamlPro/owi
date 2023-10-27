open Owi

let debug =
  let doc = "debug mode" in
  Cmdliner.Arg.(value & flag & info [ "debug"; "d" ] ~doc)

let file =
  let doc = "source file" in
  Cmdliner.Arg.(required & pos ~rev:true 0 (some string) None & info [] ~doc)

let files =
  let doc = "source files" in
  let parse s = Ok s in
  Cmdliner.Arg.(
    value
    & pos 0
        (list ~sep:' ' (conv (parse, Format.pp_print_string)))
        [] (info [] ~doc) )

let no_exhaustion =
  let doc = "no exhaustion tests" in
  Cmdliner.Arg.(value & flag & info [ "no-exhaustion" ] ~doc)

let no_stop_at_failure =
  let doc = "do not stop when a program failure is encountered" in
  Cmdliner.Arg.(value & flag & info [ "no-stop-at-failure" ] ~doc)

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
  let doc = "number of workers for symbolic execution" in
  Cmdliner.Arg.(value & opt int 4 & info [ "workers"; "w" ] ~doc)

let testsuite =
  let doc = "path to the testsuite directory" in
  Cmdliner.Arg.(value & opt string "test-suite" & info [ "test-suite" ] ~doc)

let copts_t = Cmdliner.Term.(const [])

let sdocs = Cmdliner.Manpage.s_common_options

let shared_man =
  [ `S Cmdliner.Manpage.s_bugs; `P "Email them to <contact@ndrs.fr>." ]

let version = "%%VERSION%%"

let opt_cmd =
  let open Cmdliner in
  let info =
    let doc = "Optimize a module" in
    let man = [] @ shared_man in
    Cmd.info "opt" ~version ~doc ~sdocs ~man
  in
  Cmd.v info Term.(const Cmd_opt.cmd $ debug $ unsafe $ file)

let run_cmd =
  let open Cmdliner in
  let info =
    let doc = "Run the concrete interpreter" in
    let man = [] @ shared_man in
    Cmd.info "run" ~version ~doc ~sdocs ~man
  in
  Cmd.v info
    Term.(const Cmd_run.cmd $ profiling $ debug $ unsafe $ optimize $ files)

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
      $ no_stop_at_failure $ testsuite $ files )

let cli =
  let open Cmdliner in
  let info =
    let doc = "OCaml WebAssembly Interpreter" in
    let sdocs = Manpage.s_common_options in
    let man = [ `S Manpage.s_bugs; `P "Email them to <contact@ndrs.fr>." ] in
    Cmd.info "owi" ~version ~doc ~sdocs ~man
  in
  let default = Term.(ret (const (fun _ -> `Help (`Pager, None)) $ copts_t)) in
  Cmd.group info ~default [ opt_cmd; run_cmd; script_cmd; sym_cmd ]

let main () = exit @@ Cmdliner.Cmd.eval cli

let () = main ()
