(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Bos
open Syntax

type metadata =
  { arch : int
  ; property : string option
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

let compile ~includes ~opt_lvl (files : Fpath.t list) : Fpath.t Result.t =
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

  let+ () = OS.Cmd.run clang in

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
      match property with None -> Ok "" | Some f -> OS.File.read @@ Fpath.v f
    in
    let file = String.concat " " (List.map Fpath.to_string files) in
    let* hash =
      list_fold_left
        (fun context file ->
          match Bos.OS.File.read file with
          | Error (`Msg msg) -> Error (`Msg msg)
          | Ok str -> Ok (Digestif.SHA256.feed_string context str) )
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
  deterministic_result_order concolic solver : unit Result.t =
  if debug then Logs.set_level (Some Debug);
  let workspace = Fpath.v workspace in
  let includes = libc_location @ includes in
  let* (_exists : bool) = OS.Dir.create ~path:true workspace in
  let* modul = compile ~includes ~opt_lvl files in
  let* () = metadata ~workspace arch property files in
  let workspace = Fpath.(workspace / "test-suite") in
  let files = [ modul ] in
  (if concolic then Cmd_conc.cmd else Cmd_sym.cmd)
    profiling debug unsafe optimize workers no_stop_at_failure no_values
    deterministic_result_order workspace solver files
