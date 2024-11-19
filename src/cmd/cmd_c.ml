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

let binc_location = List.map Fpath.v C_share_site.Sites.binc

let libc_location = List.map Fpath.v C_share_site.Sites.libc

let find location file : Fpath.t Result.t =
  let* l =
    list_map
      (fun dir ->
        let filename = Fpath.append dir file in
        match Bos.OS.File.exists filename with
        | Ok true -> Ok (Some filename)
        | Ok false -> Ok None
        | Error (`Msg msg) -> Error (`Msg msg) )
      location
  in
  let rec loop = function
    | [] -> Error (`Msg (Fmt.str "can't find file %a" Fpath.pp file))
    | None :: tl -> loop tl
    | Some file :: _tl -> Ok file
  in
  loop l

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
          ; "CERT:MSC:38=inactive"
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

    let framac : Fpath.t -> Fpath.t -> Bos.Cmd.t =
     fun file out -> Cmd.(framac_bin %% flags1 % p file %% flags2 % p out)
    in

    let+ () =
      list_iter
        (fun (file, out) ->
          match OS.Cmd.run @@ framac file out with
          | Ok _ as v -> v
          | Error (`Msg e) ->
            Error
              (`Msg
                (Fmt.str "Frama-C failed: %s"
                   ( if debug then e
                     else "run with --debug to get the full error message" ) )
                ) )
        (List.combine files outs)
    in

    outs
  else Ok files

let compile ~includes ~opt_lvl debug (files : Fpath.t list) : Fpath.t Result.t =
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
          "-Wl,--entry=main"
        ; "-Wl,--export=main"
          (* TODO: allow this behind a flag, this is slooooow *)
        ; "-Wl,--lto-O0"
        ; Fmt.str "-Wl,-z,stack-size=%s" stack_size
        ]
      %% includes )
  in

  let* clang_bin = OS.Cmd.resolve @@ Cmd.v "clang" in

  let out = Fpath.(v "a.out.wasm") in

  let* libc = find binc_location (Fpath.v "libc.wasm") in

  let files = Cmd.of_list (List.map Fpath.to_string (libc :: files)) in
  let clang : Bos.Cmd.t = Cmd.(clang_bin %% flags % "-o" % p out %% files) in

  let+ () =
    match OS.Cmd.run clang with
    | Ok _ as v -> v
    | Error (`Msg e) ->
      Error
        (`Msg
          (Fmt.str "Clang failed: %s"
             ( if debug then e
               else "run with --debug to get the full error message" ) ) )
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
  let* (_exists : bool) = OS.Dir.create ~path:true (Fpath.parent fpath) in
  let* res = OS.File.with_oc fpath out_metadata { arch; property; files } in
  res

let cmd debug arch property _testcomp workspace workers opt_lvl includes files
  profiling unsafe optimize no_stop_at_failure no_values
  no_assert_failure_expression_printing deterministic_result_order fail_mode
  concolic eacsl solver : unit Result.t =
  let workspace = Fpath.v workspace in
  let includes = libc_location @ includes in
  let* (_exists : bool) = OS.Dir.create ~path:true workspace in
  let* files = eacsl_instrument eacsl debug ~includes files in
  let* modul = compile ~includes ~opt_lvl debug files in
  let* () = metadata ~workspace arch property files in
  let workspace = Fpath.(workspace / "test-suite") in
  let files = [ modul ] in
  (if concolic then Cmd_conc.cmd else Cmd_sym.cmd)
    profiling debug unsafe false false optimize workers no_stop_at_failure
    no_values no_assert_failure_expression_printing deterministic_result_order
    fail_mode workspace solver files
