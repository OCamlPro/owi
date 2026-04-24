open Bos
open Syntax

let resolve_binary name =
  match OS.Cmd.resolve @@ Cmd.v name with
  | Error _ ->
    Fmt.error_msg
      "The `%s` binary was not found, please make sure it is in your path."
      name
  | Ok _ as ok -> ok

let err_output =
  match Logs.Src.level Log.main_src with
  | Some (Logs.Debug | Logs.Info) -> OS.Cmd.err_run_out
  | None | Some _ -> OS.Cmd.err_null

let bitcode_of_input ~workspace ~llvm_as_bin file : Fpath.t Result.t =
  match Fpath.get_ext ~multi:false file with
  | ".bc" -> Ok file
  | ".ll" ->
    let out_bc = Fpath.(workspace // Fpath.base (file -+ ".bc")) in
    let llvm_as_cmd : Cmd.t = Cmd.(llvm_as_bin % p file % "-o" % p out_bc) in
    let+ () =
      match OS.Cmd.run ~err:err_output llvm_as_cmd with
      | Ok _ as v -> v
      | Error (`Msg e) ->
        Log.debug (fun m -> m "llvm-as failed: %s" e);
        Fmt.error_msg
          "llvm-as failed: run with -vv to get the full error message if it was \
           not displayed above"
    in
    out_bc
  | ext ->
    Fmt.error_msg
      "Unsupported file extension `%s` for LLVM command, expected .ll or .bc"
      ext

let compile ~workspace ~entry_point ~out_file (files : Fpath.t list) :
  Fpath.t Result.t =
  let* llvm_as_bin = resolve_binary "llvm-as" in
  let* llc_bin = resolve_binary "llc" in
  let* wasmld_bin = resolve_binary "wasm-ld" in

  let* bc_files =
    list_map (bitcode_of_input ~workspace ~llvm_as_bin) files
  in

  let files_bc = Cmd.of_list (List.map Cmd.p bc_files) in
  let llc_cmd : Cmd.t =
    Cmd.(llc_bin % "-O0" % "-march=wasm32" % "-filetype=obj" %% files_bc)
  in

  let* () =
    Log.bench_fn "llc time" @@ fun () ->
    match OS.Cmd.run ~err:err_output llc_cmd with
    | Ok _ as v -> v
    | Error (`Msg e) ->
      Log.debug (fun m -> m "llc failed: %s" e);
      Fmt.error_msg "llc failed: run with -vv to get the full error message"
  in

  let files_o = Cmd.of_list (List.map (fun file -> Cmd.p Fpath.(file -+ ".o")) bc_files) in

  let out = Option.value ~default:Fpath.(workspace / "a.out.wasm") out_file in

  let* libc = Cmd_utils.find_installed_c_file (Fpath.v "libc.wasm") in
  let* libowi = Cmd_utils.find_installed_c_file (Fpath.v "libowi.wasm") in

  let wasmld_cmd : Cmd.t =
    Cmd.(
      wasmld_bin
      %% of_list
           ( [ "-z"; "stack-size=8388608" ]
           @ ( match entry_point with
             | None -> []
             | Some entry_point ->
               [ Fmt.str "--export=%s" entry_point
               ; Fmt.str "--entry=%s" entry_point
               ] )
           @ [ "--allow-undefined" ]
           @ [ p libc; p libowi ]
           @ [ "-o"; p out ] )
      %% files_o )
  in

  let+ () =
    Log.bench_fn "wasm-ld time" @@ fun () ->
    match OS.Cmd.run ~err:err_output wasmld_cmd with
    | Ok _ as v -> v
    | Error (`Msg e) ->
      Log.debug (fun m -> m "wasm-ld failed: %s" e);
      Fmt.error_msg
        "wasm-ld failed: run with -vv to get the full error message if it was \
         not displayed above"
  in

  out

let cmd ~(symbolic_parameters : Symbolic_parameters.t) ~files ~out_file :
  unit Result.t =
  let* workspace =
    match symbolic_parameters.workspace with
    | Some path -> Ok path
    | None -> OS.Dir.tmp "owi_llvm_%s"
  in
  let* _did_create : bool = OS.Dir.create ~path:true workspace in

  let* source_file =
    compile ~workspace ~entry_point:symbolic_parameters.entry_point ~out_file
      files
  in
  let workspace = Some workspace in

  let parameters = { symbolic_parameters with workspace } in

  Cmd_sym.cmd ~parameters ~source_file
