(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Bos
open Syntax

type deps =
  { clang : flags:Cmd.t -> out:Fpath.t -> Fpath.t -> Cmd.t
  ; opt : Fpath.t -> Cmd.t
  ; llc : bc:Fpath.t -> obj:Fpath.t -> Cmd.t
  ; ld : flags:Cmd.t -> out:Fpath.t -> Fpath.t list -> Cmd.t
  }

type metadata =
  { arch : int
  ; property : string option
  ; files : Fpath.t list
  }

let clang bin ~flags ~out file : Bos.Cmd.t =
  Cmd.(bin %% flags % "-o" % p out % p file)

let opt bin file : Bos.Cmd.t = Cmd.(bin % "-O1" % "-o" % p file % p file)

let llc bin ~bc ~obj : Bos.Cmd.t =
  let flags = Cmd.of_list [ "-O1"; "-march=wasm32"; "-filetype=obj"; "-o" ] in
  Cmd.(bin %% flags % p obj % p bc)

let ld bin ~flags ~out files : Bos.Cmd.t =
  let files = List.fold_left (fun acc f -> Cmd.(acc % p f)) Cmd.empty files in
  Cmd.(bin %% flags % "-o" % p out %% files % p C_share.libc)

let check_dependencies () : deps Result.t =
  let* clang_bin = OS.Cmd.resolve @@ Cmd.v "clang" in
  let* opt_bin = OS.Cmd.resolve @@ Cmd.v "opt" in
  let* llc_bin = OS.Cmd.resolve @@ Cmd.v "llc" in
  let* ld_bin = OS.Cmd.resolve @@ Cmd.v "wasm-ld" in
  Ok
    { clang = clang clang_bin
    ; opt = opt opt_bin
    ; llc = llc llc_bin
    ; ld = ld ld_bin
    }

let pre_patterns : (Re2.t * string) array =
  Array.map
    (fun (regex, template) -> (Re2.create_exn regex, template))
    [| ( "void\\s+reach_error\\(\\)\\s*\\{.*\\}"
       , "void reach_error() { owi_assert(0); }" )
       (* ugly: Hack to solve duplicate errors on compilation *)
       (* ; ("void\\s+(assert|assume)\\(", "void old_\\1(") *)
    |]

let patch_with_regex ~patterns (data : string) : string =
  Array.fold_left
    (fun data (regex, template) -> Re2.rewrite_exn regex ~template data)
    data patterns

let patch ~src ~dst : unit Result.t =
  let* data = OS.File.read src in
  let data = patch_with_regex ~patterns:pre_patterns data in
  let data =
    String.concat "\n"
      [ "#define __attribute__(x)"
      ; "#define __extension__"
      ; "#define __restrict"
      ; "#define __inline"
      ; "#include <owi.h>"
      ; data
      ]
  in
  OS.File.write dst data

let copy ~src ~dst : Fpath.t Result.t =
  let* data = OS.File.read src in
  let+ () = OS.File.write dst data in
  dst

let instrument_file ?(skip = false) ~includes ~workspace file : Fpath.t Result.t
    =
  let dst = Fpath.(workspace // base (file -+ ".c")) in
  if skip then copy ~src:file ~dst
  else begin
    Logs.app (fun m -> m "instrumenting %a" Fpath.pp file);
    let* () = patch ~src:file ~dst in
    let pypath =
      Format.asprintf "%a"
        (Format.pp_list
           ~pp_sep:(fun fmt () -> Format.pp_char fmt ':')
           Fpath.pp )
        C_share.py_location
    in
    let+ () = OS.Env.set_var "PYTHONPATH" (Some pypath) in
    begin
      try
        Py.initialize ();
        C_instrumentor.instrument dst includes;
        Py.finalize ()
      with Py.E (errtype, errvalue) ->
        let pp = Py.Object.format in
        Logs.warn (fun m -> m "instrumentor: %a: %a" pp errtype pp errvalue)
    end;
    dst
  end

let compile ~deps ~includes ~opt_lvl file : Fpath.t Result.t =
  Logs.app (fun m -> m "compiling %a" Fpath.pp file);
  let cflags =
    let includes = Cmd.of_list ~slip:"-I" (List.map Fpath.to_string includes) in
    let warnings =
      Cmd.of_list
        [ "-Wno-int-conversion"
        ; "-Wno-pointer-sign"
        ; "-Wno-string-plus-int"
        ; "-Wno-implicit-function-declaration"
        ; "-Wno-incompatible-library-redeclaration"
        ; "-Wno-incompatible-function-pointer-types"
        ; "-Wno-incompatible-pointer-types"
        ]
    in
    Cmd.(
      of_list
        [ "-O" ^ opt_lvl; "-g"; "-emit-llvm"; "--target=wasm32"; "-m32"; "-c" ]
      %% warnings %% includes )
  in
  let bc = Fpath.(file -+ ".bc") in
  let obj = Fpath.(file -+ ".o") in
  let* () = OS.Cmd.run @@ deps.clang ~flags:cflags ~out:bc file in
  let* () = OS.Cmd.run @@ deps.opt bc in
  let+ () = OS.Cmd.run @@ deps.llc ~bc ~obj in
  obj

let link ~deps ~workspace files : Fpath.t Result.t =
  let ldflags ~entry =
    let stack_size = 8 * (1024 * 1024) in
    Cmd.(
      of_list
        [ "-z"; "stack-size=" ^ string_of_int stack_size; "--export=" ^ entry ] )
  in
  let wasm = Fpath.(workspace / "a.out.wasm") in
  let+ () =
    OS.Cmd.run @@ deps.ld ~flags:(ldflags ~entry:"_start") ~out:wasm files
  in
  wasm

let pp_tm fmt Unix.{ tm_year; tm_mon; tm_mday; tm_hour; tm_min; tm_sec; _ } :
  unit =
  Format.pp fmt "%04d-%02d-%02dT%02d:%02d:%02dZ" (tm_year + 1900) tm_mon tm_mday
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
          ; el "architecture" (Format.sprintf "%dbit" arch)
          ; el "creationtime" (Format.asprintf "%a" pp_tm time)
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

let cmd debug arch property testcomp workspace workers opt_lvl includes files
  profiling unsafe optimize no_stop_at_failure no_values
  deterministic_result_order concolic : unit Result.t =
  if debug then Logs.set_level (Some Debug);
  let workspace = Fpath.v workspace in
  let includes = C_share.lib_location @ includes in
  let* (deps : deps) = check_dependencies () in
  let* (_exists : bool) = OS.Dir.create ~path:true workspace in
  (* skip instrumentation if not in test-comp mode *)
  let skip = (not testcomp) || Sys.getenv_opt "RUNNER_OS" = Some "macOS" in
  let* (nfiles : Fpath.t list) =
    list_map (instrument_file ~skip ~includes ~workspace) files
  in
  let* (objects : Fpath.t list) =
    list_map (compile ~deps ~includes ~opt_lvl) nfiles
  in
  let* modul = link ~deps ~workspace objects in
  let* () = metadata ~workspace arch property files in
  let files = [ modul ] in
  let workspace = Fpath.(workspace / "test-suite") in
  if concolic then
    Cmd_conc.cmd profiling debug unsafe optimize workers no_stop_at_failure
      no_values deterministic_result_order workspace files
  else
    Cmd_sym.cmd profiling debug unsafe optimize workers no_stop_at_failure
      no_values deterministic_result_order workspace files
