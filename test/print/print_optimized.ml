open Owi

let m =
  match Parse.module_from_file ~filename:Sys.argv.(1) with
  | Ok m -> m
  | Error msg -> failwith msg

let m =
  match Compile.until_simplify m with Ok m -> m | Error msg -> failwith msg

let () = Format.printf "%a@\n" Types.Simplified.Pp.modul (Optimize.modul m)
