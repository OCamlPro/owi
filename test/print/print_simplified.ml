(** Parse a Wasm module, simplify it, then try to reparse the output produced by
    printing the simplified module. If it succeed, it will also print the
    reparsed module to stdout *)

open Owi

let m =
  match Parse.module_from_file ~filename:Sys.argv.(1) with
  | Ok m -> m
  | Error msg -> failwith msg

let m =
  match Compile.until_simplify m with Ok m -> m | Error msg -> failwith msg

let s = Format.asprintf "%a@\n" Simplified.Pp.modul m

let m =
  match Parse.module_from_string s with Ok m -> m | Error msg -> failwith msg

let m =
  match Compile.until_simplify m with Ok m -> m | Error msg -> failwith msg

let () = Format.printf "%a@\n" Simplified.Pp.modul m
