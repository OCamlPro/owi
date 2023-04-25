open Owi

let modul_interpret m opti =
  match
    Compile.until_interpret Link.empty_state ~optimize:opti ~name:None m
  with
  | Ok st -> Some st
  | Error _ -> None

let check_optimized m =
  let st = modul_interpret m false in
  let st_o = modul_interpret m true in
  match (st, st_o) with
  | Some _, Some _ | None, None -> (*print_endline "true";*) true
  | _, _ -> (*print_endline "false";*) false

let is_optimized m = Crowbar.check (check_optimized m)

let () =
  print_newline ();
  print_endline "Owi fuzzing ...";
  Crowbar.add_test ~name:"Optimize fuzzing" [ Gen.modul ] (fun m ->
    Env.reset ();
    Format.printf "%a@\n" Owi.Types.Symbolic.Pp.modul m;
    is_optimized m )
