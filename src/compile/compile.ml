(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

module C = struct
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

  let files_to_wasm_file ~eacsl ~entry_point ~includes ~opt_lvl ~out_file
    ~workspace (files : Fpath.t list) : Fpath.t Result.t =
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

    let files =
      Bos.Cmd.of_list (List.map Fpath.to_string (libc :: libowi :: files))
    in
    let clang : Bos.Cmd.t =
      Bos.Cmd.(clang_bin %% flags % "-o" % p out %% files)
    in

    let err =
      match Logs.Src.level Log.main_src with
      | Some (Logs.Debug | Logs.Info) -> Bos.OS.Cmd.err_run_out
      | None | Some _ -> Bos.OS.Cmd.err_null
    in

    let+ () =
      Log.bench_fn "compiling time" @@ fun () ->
      match Bos.OS.Cmd.run ~err clang with
      | Ok _ as v -> v
      | Error (`Msg msg) ->
        Log.debug (fun m -> m "clang failed: %s" msg);
        Fmt.error_msg
          "clang failed (run with -vv if the full error message is not \
           displayed above)"
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
