(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Bos
open Syntax

type metadata =
  { arch : int
  ; property : Fpath.t option
  ; files : Fpath.t list
  }

let eacsl_instrument eacsl debug ~includes (files : Fpath.t list) :
  Fpath.t list Result.t =
  if eacsl then
    let flags1 =
      let includes =
        String.concat " "
          (List.map
             (fun libpath -> Fmt.str "-I%s" (Fpath.to_string libpath))
             includes )
      in
      let framac_verbosity_level = if debug then "2" else "0" in
      Cmd.(
        of_list
          [ "-e-acsl"
          ; "-no-frama-c-stdlib"
          ; "-kernel-warn-key"
          ; "CERT:MSC:38=inactive,unknown-attribute=inactive"
          ; "-verbose"
          ; framac_verbosity_level
          ; String.concat "" [ {|-cpp-extra-args="|}; includes; {|"|} ]
          ] )
    in
    let flags2 = Cmd.(of_list [ "-then-last"; "-print"; "-ocode" ]) in

    let* framac_bin = OS.Cmd.resolve @@ Cmd.v "frama-c" in

    let outs =
      List.map
        (fun file ->
          let file, ext = Fpath.split_ext file in
          let file = Fpath.add_ext ".instrumented" file in
          Fpath.add_ext ext file )
        files
    in

    let framac : Fpath.t -> Fpath.t -> Cmd.t =
     fun file out -> Cmd.(framac_bin %% flags1 % p file %% flags2 % p out)
    in

    let+ () =
      list_iter
        (fun (file, out) ->
          match
            OS.Cmd.run
              ~err:(if debug then OS.Cmd.err_run_out else OS.Cmd.err_null)
            @@ framac file out
          with
          | Ok _ as v -> v
          | Error (`Msg e) ->
            Fmt.error_msg "Frama-C failed: %s"
              ( if debug then e
                else "run with --debug to get the full error message" ) )
        (List.combine files outs)
    in

    outs
  else Ok files

let compile ~workspace ~entry_point ~includes ~opt_lvl ~out_file debug
  (files : Fpath.t list) : Fpath.t Result.t =
  let flags =
    let stack_size = 8 * 1024 * 1024 |> string_of_int in
    let includes = Cmd.of_list ~slip:"-I" (List.map Fpath.to_string includes) in
    Cmd.(
      of_list
        [ Fmt.str "-O%s" opt_lvl
        ; "--target=wasm32"
        ; "-m32"
        ; "-ffreestanding"
        ; "--no-standard-libraries"
        ; "-Wno-everything"
        ; "-flto=thin"
        ; (* LINKER FLAGS: *)
          Fmt.str "-Wl,--entry=%s" entry_point
        ; Fmt.str "-Wl,--export=%s" entry_point
          (* TODO: allow this behind a flag, this is slooooow *)
        ; "-Wl,--lto-O0"
        ; Fmt.str "-Wl,-z,stack-size=%s" stack_size
        ]
      %% includes )
  in

  let* clang_bin = OS.Cmd.resolve @@ Cmd.v "clang" in

  let out = Option.value ~default:Fpath.(workspace / "a.out.wasm") out_file in
  let* libc = Cmd_utils.find_installed_c_file (Fpath.v "libc.wasm") in

  let files = Cmd.of_list (List.map Fpath.to_string (libc :: files)) in
  let clang : Cmd.t = Cmd.(clang_bin %% flags % "-o" % p out %% files) in

  let+ () =
    match
      OS.Cmd.run
        ~err:(if debug then OS.Cmd.err_run_out else OS.Cmd.err_null)
        clang
    with
    | Ok _ as v -> v
    | Error (`Msg e) ->
      Fmt.error_msg "clang failed: %s"
        (if debug then e else "run with --debug to get the full error message")
  in

  out

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
      match property with None -> Ok "" | Some f -> OS.File.read f
    in
    let file = String.concat " " (List.map Fpath.to_string files) in
    let* hash =
      list_fold_left
        (fun context file ->
          let+ str = OS.File.read file in
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
  let* res = OS.File.with_oc fpath out_metadata { arch; property; files } in
  res

let cmd ~debug ~arch ~property ~testcomp:_ ~workspace ~workers ~opt_lvl
  ~includes ~files ~profiling ~unsafe ~optimize ~no_stop_at_failure ~no_value
  ~no_assert_failure_expression_printing ~deterministic_result_order ~fail_mode
  ~concolic ~eacsl ~solver ~profile ~model_format ~(entry_point : string option)
  ~invoke_with_symbols ~out_file ~model_out_file : unit Result.t =
  let* workspace =
    match workspace with
    | Some path -> Ok path
    | None -> Bos.OS.Dir.tmp "owi_c_%s"
  in
  let* _did_create : bool = OS.Dir.create Fpath.(workspace / "test-suite") in
  let entry_point = Option.value entry_point ~default:"main" in

  let includes = Cmd_utils.c_files_location @ includes in
  let* files = eacsl_instrument eacsl debug ~includes files in
  let* modul =
    compile ~workspace ~entry_point ~includes ~opt_lvl ~out_file debug files
  in
  let* () = metadata ~workspace arch property files in
  let files = [ modul ] in
  let entry_point = Some entry_point in
  let workspace = Some workspace in
  (if concolic then Cmd_conc.cmd else Cmd_sym.cmd)
    ~profiling ~debug ~unsafe ~rac:false ~srac:false ~optimize ~workers
    ~no_stop_at_failure ~no_value ~no_assert_failure_expression_printing
    ~deterministic_result_order ~fail_mode ~workspace ~solver ~files ~profile
    ~model_format ~entry_point ~invoke_with_symbols ~model_out_file
