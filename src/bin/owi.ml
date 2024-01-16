open Owi

let debug =
  let doc = "debug mode" in
  Cmdliner.Arg.(value & flag & info [ "debug"; "d" ] ~doc)

let file =
  let doc = "source file" in
  Cmdliner.Arg.(required & pos ~rev:true 0 (some string) None & info [] ~doc)

let files =
  let doc = "source files" in
  Cmdliner.Arg.(value & pos_all non_dir_file [] (info [] ~doc))

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
  Cmdliner.Arg.(
    value
    & opt int (Domain.recommended_domain_count ())
    & info [ "workers"; "w" ] ~doc )

let workspace =
  let doc = "path to the workspace directory" in
  Cmdliner.Arg.(value & opt string "owi-out" & info [ "workspace" ] ~doc)

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
    Arg.(value & opt (some string) None & info [ "property"; "p" ] ~doc)
  in
  let includes =
    let doc = "headers path" in
    Arg.(value & opt_all dir [] & info [ "I" ] ~doc)
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
      $ opt_lvl $ includes $ files )

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
  Cmd.v info Term.(const Cmd_fmt.cmd $ inplace $ file)

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
      $ no_stop_at_failure $ workspace $ files )

let cli =
  let open Cmdliner in
  let info =
    let doc = "OCaml WebAssembly Interpreter" in
    let sdocs = Manpage.s_common_options in
    let man = [ `S Manpage.s_bugs; `P "Email them to <contact@ndrs.fr>." ] in
    Cmd.info "owi" ~version ~doc ~sdocs ~man
  in
  let default = Term.(ret (const (fun _ -> `Help (`Pager, None)) $ copts_t)) in
  Cmd.group info ~default
    [ c_cmd; fmt_cmd; opt_cmd; run_cmd; script_cmd; sym_cmd ]

let main () = exit @@ Cmdliner.Cmd.eval cli

let () = main ()
