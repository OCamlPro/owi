(** Parse a Wasm module, simplify it, then try to reparse the output produced by
    printing the simplified module. If it succeed, it will also print the
    reparsed module to stdout *)

open Owi

let filename = Fpath.v Sys.argv.(1)

let m =
  match Parse.Text.Module.from_file filename with
  | Ok m -> m
  | Error e -> Result.failwith e

let m =
  match Compile.Text.until_binary ~unsafe:false m with
  | Ok m -> m
  | Error e -> Result.failwith e

let s = Format.asprintf "%a@\n" Binary.Pp.modul m

let m =
  match Parse.Text.Module.from_string s with
  | Ok m -> m
  | Error e -> Result.failwith e

let m =
  match Compile.Text.until_binary ~unsafe:false m with
  | Ok m -> m
  | Error e -> Result.failwith e

let () = Format.pp_std "%a@\n" Binary.Pp.modul m
