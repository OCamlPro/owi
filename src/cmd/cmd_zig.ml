(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Bos
open Syntax

let zig_files_location = List.map Fpath.v Share.Sites.zig_files

(* TODO: share this with C/C++ *)
let find location file : Fpath.t Result.t =
  let* l =
    list_map
      (fun dir ->
        let filename = Fpath.append dir file in
        match OS.File.exists filename with
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

let compile ~includes debug (files : Fpath.t list) : Fpath.t Result.t =
  let includes =
    Cmd.of_list (List.map (fun p -> Fmt.str "-I%a" Fpath.pp p) includes)
  in

  let* zig_bin = OS.Cmd.resolve @@ Cmd.v "zig" in

  let* libzig = find zig_files_location (Fpath.v "libzig.o") in

  match files with
  | [] -> assert false
  | [ file ] -> begin
    let out = Fpath.set_ext ".wasm" file |> Fpath.filename |> Fpath.v in
    let entry = Fmt.str "-fentry=_start" in
    let zig : Cmd.t =
      Cmd.(
        zig_bin % "build-exe" % "-target" % "wasm32-freestanding" % entry
        %% includes % p file % p libzig )
    in

    let+ () =
      match OS.Cmd.run zig with
      | Ok _ as v -> v
      | Error (`Msg e) ->
        Error
          (`Msg
             (Fmt.str "zig failed: %s"
                ( if debug then e
                  else "run with --debug to get the full error message" ) ) )
    in

    out
  end
  | _ -> Fmt.failwith "TODO"

let cmd ~debug ~workers ~includes ~files ~profiling ~unsafe ~optimize
  ~no_stop_at_failure ~no_value ~no_assert_failure_expression_printing
  ~deterministic_result_order ~fail_mode ~concolic ~solver ~profile
  ~model_output_format : unit Result.t =
  let includes = zig_files_location @ includes in
  let* modul = compile ~includes debug files in
  let workspace = Fpath.v "owi-out" in
  let files = [ modul ] in
  (if concolic then Cmd_conc.cmd else Cmd_sym.cmd)
    ~profiling ~debug ~unsafe ~rac:false ~srac:false ~optimize ~workers
    ~no_stop_at_failure ~no_value ~no_assert_failure_expression_printing
    ~deterministic_result_order ~fail_mode ~workspace ~solver ~files ~profile
    ~model_output_format
