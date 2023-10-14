open Owi

let m =
  match Parse.Module.from_file ~filename:Sys.argv.(1) with
  | Ok m -> m
  | Error msg -> failwith msg

let m =
  match Compile.until_simplify ~unsafe:false m with
  | Ok m -> m
  | Error msg -> failwith msg

let () = Format.printf "%a@\n" Simplified.Pp.modul (Optimize.modul m)
