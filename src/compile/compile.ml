(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

module C = struct
  type metadata =
    { arch : int
    ; property : Fpath.t option
    ; files : Fpath.t list
    }

  let pp_tm fmt Unix.{ tm_year; tm_mon; tm_mday; tm_hour; tm_min; tm_sec; _ } :
    unit =
    Fmt.pf fmt "%04d-%02d-%02dT%02d:%02d:%02dZ" (tm_year + 1900) tm_mon tm_mday
      tm_hour tm_min tm_sec

  let metadata ~workspace arch property files : unit Result.t =
    let out_metadata chan { arch; property; files } =
      let o = Xmlm.make_output ~nl:true ~indent:(Some 2) (`Channel chan) in
      let tag n = (("", n), []) in
      let el n d = `El (tag n, [ `Data d ]) in
      let* spec =
        match property with None -> Ok "" | Some f -> Bos.OS.File.read f
      in
      let file = String.concat " " (List.map Fpath.to_string files) in
      let* hash =
        list_fold_left
          (fun context file ->
            let+ str = Bos.OS.File.read file in
            Digestif.SHA256.feed_string context str )
          Digestif.SHA256.empty files
      in
      let hash = Digestif.SHA256.to_hex (Digestif.SHA256.get hash) in
      let time = Unix.time () |> Unix.localtime in
      let test_metadata =
        `El
          ( tag "test-metadata"
          , [ el "sourcecodelang" "C"
            ; el "producer" "owic"
            ; el "specification" (String.trim spec)
            ; el "programfile" file
            ; el "programhash" hash
            ; el "entryfunction" "main"
            ; el "architecture" (Fmt.str "%dbit" arch)
            ; el "creationtime" (Fmt.str "%a" pp_tm time)
            ] )
      in
      let dtd =
        {xml|<!DOCTYPE test-metadata PUBLIC "+//IDN sosy-lab.org//DTD test-format test-metadata 1.1//EN" "https://sosy-lab.org/test-format/test-metadata-1.1.dtd">|xml}
      in
      Xmlm.output o (`Dtd (Some dtd));
      Xmlm.output_tree Fun.id o test_metadata;
      Ok ()
    in
    let fpath = Fpath.(workspace / "test-suite" / "metadata.xml") in
    let* res =
      Bos.OS.File.with_oc fpath out_metadata { arch; property; files }
    in
    res

  let instrument_files_with_eacsl ~includes (files : Fpath.t list) :
    Fpath.t list Result.t =
    let flags1 =
      let includes =
        String.concat " "
          (List.map
             (fun libpath -> Fmt.str "-I%s" (Fpath.to_string libpath))
             includes )
      in

      let framac_verbosity_level =
        match Logs.Src.level Log.main_src with
        | Some (Logs.Debug | Logs.Info) -> "2"
        | None | Some _ -> "0"
      in

      Bos.Cmd.(
        of_list
          [ "-e-acsl"
          ; "-no-frama-c-stdlib"
          ; "-kernel-warn-key"
          ; "CERT:MSC:38=inactive,attrs:unknown=inactive"
          ; "-verbose"
          ; framac_verbosity_level
          ; String.concat "" [ {|-cpp-extra-args="|}; includes; {|"|} ]
          ] )
    in
    let flags2 = Bos.Cmd.(of_list [ "-then-last"; "-print"; "-ocode" ]) in

    let* framac_bin = Bos.OS.Cmd.resolve @@ Bos.Cmd.v "frama-c" in

    let outs =
      List.map
        (fun file ->
          let file, ext = Fpath.split_ext file in
          let file = Fpath.add_ext ".instrumented" file in
          Fpath.add_ext ext file )
        files
    in

    let framac : Fpath.t -> Fpath.t -> Bos.Cmd.t =
     fun file out -> Bos.Cmd.(framac_bin %% flags1 % p file %% flags2 % p out)
    in

    let err =
      match Logs.Src.level Log.main_src with
      | Some (Logs.Debug | Logs.Info) -> Bos.OS.Cmd.err_run_out
      | None | Some _ -> Bos.OS.Cmd.err_null
    in

    let+ () =
      list_iter
        (fun (file, out) ->
          match Bos.OS.Cmd.run ~err @@ framac file out with
          | Ok _ as v -> v
          | Error (`Msg e) ->
            Log.debug (fun m -> m "frama-c failed: %s" e);
            Fmt.error_msg
              "Frama-C failed: run with -vv to get the full error message if \
               it was not displayed above" )
        (List.combine files outs)
    in

    outs

  let files_to_wasm_file ~arch ~eacsl ~entry_point ~includes ~opt_lvl ~out_file
    ~property ~workspace (files : Fpath.t list) : Fpath.t Result.t =
    let includes = Cmd_utils.c_files_location @ includes in
    let* files =
      if eacsl then instrument_files_with_eacsl ~includes files else Ok files
    in
    let flags =
      let stack_size = 8 * 1024 * 1024 |> string_of_int in
      let includes =
        Bos.Cmd.of_list ~slip:"-I" (List.map Fpath.to_string includes)
      in
      Bos.Cmd.(
        of_list
          ( [ Fmt.str "-O%s" opt_lvl
            ; "--target=wasm32-unknown-unknown"
            ; "-m32"
            ; "-ffreestanding"
            ; "--no-standard-libraries"
            ; "-Wno-everything"
            ; "-flto=thin"
            ]
          (* LINKER FLAGS: *)
          @ ( match entry_point with
            | Some entry_point ->
              [ Fmt.str "-Wl,--entry=%s" entry_point
              ; Fmt.str "-Wl,--export=%s" entry_point
              ]
            | None -> [] )
          @ [ (* TODO: allow this behind a flag, this is slooooow *)
              "-Wl,--lto-O0"
            ; Fmt.str "-Wl,-z,stack-size=%s" stack_size
            ] )
        %% includes )
    in

    let* clang_bin = Bos.OS.Cmd.resolve @@ Bos.Cmd.v "clang" in

    let out = Option.value ~default:Fpath.(workspace / "a.out.wasm") out_file in
    let* libc = Cmd_utils.find_installed_c_file (Fpath.v "libc.wasm") in
    let* libowi = Cmd_utils.find_installed_c_file (Fpath.v "libowi.wasm") in

    let clang : Bos.Cmd.t =
      let files =
        Bos.Cmd.of_list (List.map Fpath.to_string (libc :: libowi :: files))
      in
      Bos.Cmd.(clang_bin %% flags % "-o" % p out %% files)
    in

    let err =
      match Logs.Src.level Log.main_src with
      | Some (Logs.Debug | Logs.Info) -> Bos.OS.Cmd.err_run_out
      | None | Some _ -> Bos.OS.Cmd.err_null
    in

    let* () =
      Log.bench_fn "compiling time" @@ fun () ->
      match Bos.OS.Cmd.run ~err clang with
      | Ok _ as v -> v
      | Error (`Msg msg) ->
        Log.debug (fun m -> m "clang failed: %s" msg);
        Fmt.error_msg
          "clang failed (run with -vv if the full error message is not \
           displayed above)"
    in

    let+ () = metadata ~workspace arch property files in

    out
end

module Cpp = struct
  let files_to_wasm_file ~entry_point ~includes ~(files : Fpath.t list) ~opt_lvl
    ~out_file ~workspace : Fpath.t Result.t =
    let* clangpp_bin = Bos.OS.Cmd.resolve @@ Bos.Cmd.v "clang++" in
    let opt_lvl = Fmt.str "-O%s" opt_lvl in

    let includes = Cmd_utils.c_files_location @ includes in
    let includes = Bos.Cmd.of_list ~slip:"-I" (List.map Bos.Cmd.p includes) in

    let err =
      match Logs.Src.level Log.main_src with
      | Some (Logs.Debug | Logs.Info) -> Bos.OS.Cmd.err_run_out
      | None | Some _ -> Bos.OS.Cmd.err_null
    in
    let* () =
      (* TODO: we use this recursive function in order to be able to use `-o` on
         each file. We could get rid of this if we managed to call the C++
         compiler and the linker in the same step as it is done for C - then
         there would be a single output file and we could use `-o` more easily. *)
      let rec compile_files = function
        | [] -> Ok ()
        | file :: rest -> (
          let out_bc = Fpath.(workspace // Fpath.base (file -+ ".bc")) in
          let clang_cmd =
            Bos.Cmd.(
              clangpp_bin % "-Wno-everything" % opt_lvl % "-emit-llvm"
              % "--target=wasm32" % "-m32" % "-c" %% includes % "-o" % p out_bc
              % p file )
          in
          match Bos.OS.Cmd.run ~err clang_cmd with
          | Ok _ -> compile_files rest
          | Error (`Msg e) ->
            Log.debug (fun m -> m "clang++ failed: %s" e);
            Fmt.error_msg
              "clang++ failed: run with -vv if the error is not displayed above"
          )
      in
      Log.bench_fn "compiling time" (fun () -> compile_files files)
    in

    let* llc_bin = Bos.OS.Cmd.resolve @@ Bos.Cmd.v "llc" in

    let files_bc =
      Bos.Cmd.of_list
      @@ List.map
           (fun file ->
             Fpath.(workspace // Fpath.base (file -+ ".bc")) |> Bos.Cmd.p )
           files
    in

    let llc_cmd : Bos.Cmd.t =
      Bos.Cmd.(
        llc_bin
        %
        (* TODO: configure this ? *)
        "-O0" % "-march=wasm32" % "-filetype=obj" %% files_bc )
    in

    let* () =
      Log.bench_fn "llc time" @@ fun () ->
      match Bos.OS.Cmd.run ~err llc_cmd with
      | Ok _ as v -> v
      | Error (`Msg e) ->
        Log.debug (fun m -> m "llc failed: %s" e);
        Fmt.error_msg
          "llc failed: run with --debug to get the full error message"
    in
    let* wasmld_bin = Bos.OS.Cmd.resolve @@ Bos.Cmd.v "wasm-ld" in

    let files_o =
      List.map
        (fun file -> Fpath.(workspace // Fpath.base (file -+ ".o")) |> Bos.Cmd.p)
        files
    in

    let out =
      Option.value ~default:Fpath.(workspace // v "a.out.wasm") out_file
    in

    let* libc = Cmd_utils.find_installed_c_file (Fpath.v "libc.wasm") in
    let* libowi = Cmd_utils.find_installed_c_file (Fpath.v "libowi.wasm") in
    let wasmld_cmd : Bos.Cmd.t =
      Bos.Cmd.(
        wasmld_bin
        %% of_list
             ( [ "-z"; "stack-size=8388608" ]
             @ ( match entry_point with
               | None -> []
               | Some entry_point ->
                 [ Fmt.str "--export=%s" entry_point
                 ; Fmt.str "--entry=%s" entry_point
                 ] )
             @ files_o
             @ [ p libc; p libowi; "-o"; p out ] ) )
    in

    let+ () =
      Log.bench_fn "wasm_ld time" @@ fun () ->
      match Bos.OS.Cmd.run ~err wasmld_cmd with
      | Ok _ as v -> v
      | Error (`Msg e) ->
        Log.debug (fun m -> m "wasm-ld failed: %s" e);
        Fmt.error_msg
          "wasm-ld failed: run with -vv to get the full error message if it \
           was not displayed above"
    in

    out
end

module Go = struct
  let files_to_wasm_file ~(files : Fpath.t list) ~out_file ~workspace :
    Fpath.t Result.t =
    let* tinygo_bin =
      let name = "tinygo" in
      match Bos.OS.Cmd.resolve @@ Bos.Cmd.v name with
      | Error _ ->
        Fmt.error_msg
          "The `%s` binary was not found, please make sure it is in your path."
          name
      | Ok _ as ok -> ok
    in

    let out = Option.value ~default:Fpath.(workspace / "out.wasm") out_file in
    let tinygo : Bos.Cmd.t =
      Bos.Cmd.(
        tinygo_bin % "build" % "-target" % "wasm" % "-no-debug" % "-opt" % "2"
        % "-panic" % "trap"
        (* initialization time is way too slow otherwise *)
        % "-gc"
        % "leaking"
        (* output and input *)
        % "-o"
        % p out
        %% Bos.Cmd.of_list (List.map p files)
        (* % p libtinygo *) )
    in

    let err =
      match Logs.Src.level Log.main_src with
      | Some (Logs.Debug | Logs.Info) -> Bos.OS.Cmd.err_run_out
      | None | Some _ -> Bos.OS.Cmd.err_null
    in

    let+ () =
      Log.bench_fn "compiling time" @@ fun () ->
      match Bos.OS.Cmd.run ~err tinygo with
      | Ok _ as v -> v
      | Error (`Msg e) ->
        Log.debug (fun m -> m "tinygo failed: %s" e);
        Fmt.error_msg
          "tinygo failed: run with -vv to get the full error message if it was \
           not displayed above"
    in

    out
end

module Haskell = struct
  let files_to_wasm_file ~(files : Fpath.t list) ~out_file ~workspace :
    Fpath.t Result.t =
    let* haskell_bin =
      let name = "wasm32-wasi-ghc" in
      match Bos.OS.Cmd.resolve @@ Bos.Cmd.v name with
      | Error _ ->
        Fmt.error_msg
          "The `%s` binary was not found, please make sure it is in your path."
          name
      | Ok _ as ok -> ok
    in

    let out = Option.value ~default:Fpath.(workspace / "out.wasm") out_file in
    let haskell : Bos.Cmd.t =
      Bos.Cmd.(
        haskell_bin
        (* output and input *)
        % "-o"
        % p out
        %% Bos.Cmd.of_list (List.map p files)
        (* % p libhaskell *) )
    in

    let err =
      match Logs.Src.level Log.main_src with
      | Some (Logs.Debug | Logs.Info) -> Bos.OS.Cmd.err_run_out
      | None | Some _ -> Bos.OS.Cmd.err_null
    in

    let+ () =
      Log.bench_fn "compiling time" @@ fun () ->
      match Bos.OS.Cmd.run ~err haskell with
      | Ok _ as v -> v
      | Error (`Msg e) ->
        Log.debug (fun m -> m "haskell failed: %s" e);
        Fmt.error_msg
          "haskell failed: run with -vv to get the full error message if it \
           was not displayed above"
    in

    out
end

module Llvm = struct
  let resolve_binary name =
    match Bos.OS.Cmd.resolve @@ Bos.Cmd.v name with
    | Error _ ->
      Fmt.error_msg
        "The `%s` binary was not found, please make sure it is in your path."
        name
    | Ok _ as ok -> ok

  let err_output =
    match Logs.Src.level Log.main_src with
    | Some (Logs.Debug | Logs.Info) -> Bos.OS.Cmd.err_run_out
    | None | Some _ -> Bos.OS.Cmd.err_null

  let bitcode_of_input ~workspace ~llvm_as_bin file : Fpath.t Result.t =
    match Fpath.get_ext ~multi:false file with
    | ".bc" -> Ok file
    | ".ll" ->
      let out_bc = Fpath.(workspace // Fpath.base (file -+ ".bc")) in
      let llvm_as_cmd : Bos.Cmd.t =
        Bos.Cmd.(llvm_as_bin % p file % "-o" % p out_bc)
      in
      let+ () =
        match Bos.OS.Cmd.run ~err:err_output llvm_as_cmd with
        | Ok _ as v -> v
        | Error (`Msg e) ->
          Log.debug (fun m -> m "llvm-as failed: %s" e);
          Fmt.error_msg
            "llvm-as failed: run with -vv to get the full error message if it \
             was not displayed above"
      in
      out_bc
    | ext ->
      Fmt.error_msg
        "Unsupported file extension `%s` for LLVM command, expected .ll or .bc"
        ext

  let files_to_wasm_file ~entry_point ~(files : Fpath.t list) ~out_file
    ~workspace : Fpath.t Result.t =
    let* llvm_as_bin = resolve_binary "llvm-as" in
    let* llc_bin = resolve_binary "llc" in
    let* wasmld_bin = resolve_binary "wasm-ld" in

    let* bc_files = list_map (bitcode_of_input ~workspace ~llvm_as_bin) files in

    let files_bc = Bos.Cmd.of_list (List.map Bos.Cmd.p bc_files) in
    let llc_cmd : Bos.Cmd.t =
      Bos.Cmd.(
        llc_bin % "-O0" % "-march=wasm32" % "-mtriple=wasm32-unknown-unknown"
        % "-filetype=obj" %% files_bc )
    in

    let* () =
      Log.bench_fn "llc time" @@ fun () ->
      match Bos.OS.Cmd.run ~err:err_output llc_cmd with
      | Ok _ as v -> v
      | Error (`Msg e) ->
        Log.debug (fun m -> m "llc failed: %s" e);
        Fmt.error_msg "llc failed: run with -vv to get the full error message"
    in

    let files_o =
      Bos.Cmd.of_list
        (List.map (fun file -> Bos.Cmd.p Fpath.(file -+ ".o")) bc_files)
    in

    let out = Option.value ~default:Fpath.(workspace / "a.out.wasm") out_file in

    let* libc = Cmd_utils.find_installed_c_file (Fpath.v "libc.wasm") in
    let* libowi = Cmd_utils.find_installed_c_file (Fpath.v "libowi.wasm") in

    let wasmld_cmd : Bos.Cmd.t =
      Bos.Cmd.(
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
      match Bos.OS.Cmd.run ~err:err_output wasmld_cmd with
      | Ok _ as v -> v
      | Error (`Msg e) ->
        Log.debug (fun m -> m "wasm-ld failed: %s" e);
        Fmt.error_msg
          "wasm-ld failed: run with -vv to get the full error message if it \
           was not displayed above"
    in

    out
end

module Rust = struct
  (* TODO: investigate which parameters makes sense *)
  let files_to_wasm_file ~entry_point ~(files : Fpath.t list) ~includes:_
    ~opt_lvl:_ ~out_file : Fpath.t Result.t =
    let* rustc_bin = Bos.OS.Cmd.resolve @@ Bos.Cmd.v "rustc" in

    let* libowi_sym_rlib =
      Cmd_utils.find_installed_rust_file (Fpath.v "libowi_sym.rlib")
    in

    let* tmp = Bos.OS.Dir.tmp "owi_rust_%s" in
    let out = Option.value ~default:Fpath.(tmp / "a.out.wasm") out_file in

    let rustc_cmd : Bos.Cmd.t =
      Bos.Cmd.(
        rustc_bin % "--target=wasm32-unknown-unknown" % "--edition=2021"
        % "--extern"
        % Fmt.str "owi_sym=%a" Fpath.pp libowi_sym_rlib
        % "-o" % Bos.Cmd.p out
        (* link args parameters must be space separated *)
        % "-C"
        % ( match entry_point with
          | Some entry_point -> Fmt.str "link-args=--entry=%s" entry_point
          | None -> (* TODO: meh *) "" )
        %% Bos.Cmd.of_list (List.map Bos.Cmd.p files) )
    in

    let err =
      match Logs.Src.level Log.main_src with
      | Some (Logs.Debug | Logs.Info) -> Bos.OS.Cmd.err_run_out
      | None | Some _ -> Bos.OS.Cmd.err_null
    in

    (* TODO: does not seem to work, once it does, we can remove the `#![no_main]` in many documentation examples
  let* () = Bos.OS.Env.set_var "RUSTFLAGS" (Some "-Zcrate-attr=no_main") in
  *)
    let+ () =
      Log.bench_fn "compiling time" @@ fun () ->
      match Bos.OS.Cmd.run ~err rustc_cmd with
      | Ok _ as v -> v
      | Error (`Msg e) ->
        Log.debug (fun m -> m "rustc failed: %s" e);
        Fmt.error_msg
          "rustc failed: run with -vv to get the full error message if it was \
           not displayed above"
    in

    out
end

module Wasm = struct
  module Text = struct
    let until_text_validate ~unsafe m =
      if unsafe then Ok m else Text_validate.modul m

    let until_group ~unsafe m =
      let+ m = until_text_validate ~unsafe m in
      Grouped.of_text m

    let until_assign ~unsafe m =
      let* m = until_group ~unsafe m in
      let+ assigned = Assigned.of_grouped m in
      (m, assigned)

    let until_binary ~unsafe m =
      let* m, assigned = until_assign ~unsafe m in
      Rewrite.modul m assigned

    let until_validate ~unsafe m =
      let* m = until_text_validate ~unsafe m in
      let* m = until_binary ~unsafe m in
      if unsafe then Ok m
      else
        let+ () = Binary_validate.modul m in
        m

    let until_concrete_link ~unsafe ~name env m =
      let* modul = until_validate ~unsafe m in
      let* env = Env.Concrete.link_binary_module ~env ~name ~modul in
      let+ modul = Env.Concrete.get_last_module ~env in
      (modul, env)

    let until_symbolic_link ~unsafe ~name env m =
      let* modul = until_validate ~unsafe m in
      let* env = Env.Symbolic.link_binary_module ~env ~name ~modul in
      let+ modul = Env.Symbolic.get_last_module ~env in
      (modul, env)

    let until_abstract_link ~unsafe ~name env m =
      let* modul = until_validate ~unsafe m in
      let* env = Env.Abstract.link_binary_module ~env ~name ~modul in
      let+ modul = Env.Abstract.get_last_module ~env in
      (modul, env)
  end

  module Binary = struct
    let until_validate ~unsafe m =
      if unsafe then Ok m
      else
        let+ () = Binary_validate.modul m in
        m

    let until_concrete_link ~unsafe ~name env m =
      let* modul = until_validate ~unsafe m in
      let* env = Env.Concrete.link_binary_module ~env ~name ~modul in
      let+ modul = Env.Concrete.get_last_module ~env in
      (modul, env)

    let until_symbolic_link ~unsafe ~name env m =
      let* modul = until_validate ~unsafe m in
      let* env = Env.Symbolic.link_binary_module ~env ~name ~modul in
      let+ modul = Env.Symbolic.get_last_module ~env in
      (modul, env)

    let until_abstract_link ~unsafe ~name env m =
      let* modul = until_validate ~unsafe m in
      let* env = Env.Abstract.link_binary_module ~env ~name ~modul in
      let+ modul = Env.Abstract.get_last_module ~env in
      (modul, env)
  end

  module File = struct
    let until_binary ~unsafe filename =
      let* m = Parse.guess_from_file filename in
      match m with
      | Kind.Wat m -> Text.until_binary ~unsafe m
      | Wasm m -> Ok m
      | Wast _ | Extern _ -> assert false

    let until_validate ~unsafe filename =
      let* m = Parse.guess_from_file filename in
      Log.bench_fn "validation time" @@ fun () ->
      match m with
      | Kind.Wat m -> Text.until_validate ~unsafe m
      | Wasm m -> Binary.until_validate ~unsafe m
      | Wast _ | Extern _ -> assert false

    let until_concrete_link ~unsafe ~name env filename =
      let* m = Parse.guess_from_file filename in
      match m with
      | Kind.Wat m -> Text.until_concrete_link ~unsafe ~name env m
      | Wasm m -> Binary.until_concrete_link ~unsafe ~name env m
      | Wast _ | Extern _ -> assert false

    let until_symbolic_link ~unsafe ~name env filename =
      let* m = Parse.guess_from_file filename in
      match m with
      | Kind.Wat m -> Text.until_symbolic_link ~unsafe ~name env m
      | Wasm m -> Binary.until_symbolic_link ~unsafe ~name env m
      | Wast _ | Extern _ -> assert false

    let until_abstract_link ~unsafe ~name env filename =
      let* m = Parse.guess_from_file filename in
      match m with
      | Kind.Wat m -> Text.until_abstract_link ~unsafe ~name env m
      | Wasm m -> Binary.until_abstract_link ~unsafe ~name env m
      | Wast _ | Extern _ -> assert false
  end
end

module Zig = struct
  let files_to_wasm_file ~entry_point ~(files : Fpath.t list) ~includes
    ~out_file ~workspace : Fpath.t Result.t =
    let includes =
      (* TODO: disabled until zig is properly packaged
       Cmd_utils.zig_files_location @
    *)
      includes
    in

    let includes =
      Bos.Cmd.of_list (List.map (fun p -> Fmt.str "-I%a" Fpath.pp p) includes)
    in

    let* zig_bin =
      let name = "zig" in
      match Bos.OS.Cmd.resolve @@ Bos.Cmd.v name with
      | Error _ ->
        Fmt.error_msg
          "The `%s` binary was not found, please make sure it is in your path."
          name
      | Ok _ as ok -> ok
    in

    (* TODO: disabled until zig is properly packaged everywhere
     let* libzig = Cmd_utils.find_installed_zig_file (Fpath.v "libzig.o") in
  *)
    let out = Option.value ~default:Fpath.(workspace / "out.wasm") out_file in
    let entry =
      match entry_point with
      | None -> ""
      | Some entry_point -> Fmt.str "-fentry=%s" entry_point
    in
    let zig : Bos.Cmd.t =
      Bos.Cmd.(
        zig_bin % "build-exe" % "-target" % "wasm32-freestanding"
        % Fmt.str "-femit-bin=%a" Fpath.pp out
        % entry %% includes
        %% Bos.Cmd.of_list (List.map p files)
        (* % p libzig *) )
    in

    let err =
      match Logs.Src.level Log.main_src with
      | Some (Logs.Debug | Logs.Info) -> Bos.OS.Cmd.err_run_out
      | None | Some _ -> Bos.OS.Cmd.err_null
    in

    let+ () =
      Log.bench_fn "compiling time" @@ fun () ->
      match Bos.OS.Cmd.run ~err zig with
      | Ok _ as v -> v
      | Error (`Msg e) ->
        Log.debug (fun m -> m "zig failed: %s" e);
        Fmt.error_msg
          "zig failed: run with -vv to get the full error message if it was \
           not displayed above"
    in

    out
end
