let ( let* ) o f = match o with Ok v -> f v | Error _e as e -> e

let ( let+ ) o f = match o with Ok v -> Ok (f v) | Error _e as e -> e

let full_report runs output_dir reference_name =
  let output_dir = Fpath.(output_dir // v "results-report/") in

  let* (_existed : bool) =
    Bos.OS.Dir.create ~path:true ~mode:0o755 output_dir
  in

  Html.make runs output_dir reference_name;
  let+ () = Pie_results.make runs output_dir reference_name in
  Time_distribution.make runs output_dir reference_name
