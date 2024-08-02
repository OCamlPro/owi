let () =
  if Array.length Sys.argv < 3 then
    Format.ksprintf failwith "usage: %s <FILE> <FILE>" Sys.argv.(0)

let file1 = Fpath.v Sys.argv.(1)

let file2 = Fpath.v Sys.argv.(2)

open Report

let report1 = Parse.from_file file1

let report2 = Parse.from_file file2

let count_all1 = Runs.count_all report1

let count_all2 = Runs.count_all report2

let count_nothing1 = Runs.count_nothing report1

let count_nothing2 = Runs.count_nothing report2

let count_reached1 = Runs.count_reached report1

let count_reached2 = Runs.count_reached report2

let count_timeout1 = Runs.count_timeout report1

let count_timeout2 = Runs.count_timeout report2

let count_other1 = Runs.count_other report1

let count_other2 = Runs.count_other report2

let count_killed1 = Runs.count_killed report1

let count_killed2 = Runs.count_killed report2

let reached1 = Runs.keep_reached report1

let reached2 = Runs.keep_reached report2

let reached_files1 = Runs.files reached1

let reached_files2 = Runs.files reached2

let reached_tbl1 =
  let tbl = Hashtbl.create 512 in
  List.iter (fun file -> Hashtbl.replace tbl file ()) reached_files1;
  tbl

let reached_tbl2 =
  let tbl = Hashtbl.create 512 in
  List.iter (fun file -> Hashtbl.replace tbl file ()) reached_files2;
  tbl

let reached_common =
  let tbl = Hashtbl.create 512 in
  List.iter
    (fun file ->
      if Hashtbl.mem reached_tbl2 file then Hashtbl.replace tbl file () )
    reached_files1;
  tbl

let count_common = Hashtbl.length reached_common

let reached_common_1 =
  Runs.keep_if (fun run -> Hashtbl.mem reached_common run.Run.file) reached1

let reached_common_2 =
  Runs.keep_if (fun run -> Hashtbl.mem reached_common run.Run.file) reached2

let reached_only_1 =
  Runs.keep_if
    (fun run -> not @@ Hashtbl.mem reached_common run.Run.file)
    reached1

let reached_only_2 =
  Runs.keep_if
    (fun run -> not @@ Hashtbl.mem reached_common run.Run.file)
    reached2

let report1_found_by_2_not_by_1 =
  Runs.keep_if
    (fun run ->
      let f = run.Run.file in
      Hashtbl.mem reached_tbl2 f && (not @@ Hashtbl.mem reached_tbl1 f) )
    report1

let report2_found_by_1_not_by_2 =
  Runs.keep_if
    (fun run ->
      let f = run.Run.file in
      Hashtbl.mem reached_tbl1 f && (not @@ Hashtbl.mem reached_tbl2 f) )
    report2

let () =
  if count_all1 <> count_all2 then
    Format.printf
      "WARNING: runs don't have the same total of runs (tool 1 has %d and tool \
       2 has %d)@\n"
      count_all1 count_all2;
  Format.printf "tool1 had %03d reached and tool2 had %03d reached@\n"
    count_reached1 count_reached2;
  Format.printf "tool1 had %03d timeout and tool2 had %03d timeout@\n"
    count_timeout1 count_timeout2;
  Format.printf "tool1 had %03d nothing and tool2 had %03d nothing@\n"
    count_nothing1 count_nothing2;
  Format.printf "tool1 had %03d other   and tool2 had %03d other@\n"
    count_other1 count_other2;
  Format.printf "tool1 had %03d killed  and tool2 had %03d killed@\n@\n"
    count_killed1 count_killed2;
  Format.printf "tools have %03d reached tasks in common@\n@\n"
    (Hashtbl.length reached_common);
  Format.printf "tool1 had %03d tasks tool2 did not found@\n"
    (count_reached1 - count_common);
  Format.printf "tool2 had %03d tasks tool1 did not found@\n@\n"
    (count_reached2 - count_common);
  Format.printf
    "on      commonly  reached tasks, tool 1 took %04f sec. (mean %04f, median \
     %04f, min %04f, max %04f) and tool 2 took %06f sec. (mean %04f, median \
     %04f, min %04f, max %04f)@\n"
    (Runs.sum_clock reached_common_1)
    (Runs.mean_clock reached_common_1)
    (Runs.median_clock reached_common_1)
    (Runs.min_clock reached_common_1)
    (Runs.max_clock reached_common_1)
    (Runs.sum_clock reached_common_2)
    (Runs.mean_clock reached_common_2)
    (Runs.median_clock reached_common_2)
    (Runs.min_clock reached_common_2)
    (Runs.max_clock reached_common_2);
  Format.printf
    "on *not commonly* reached tasks, tool 1 took %04f sec. (mean %04f, median \
     %04f, min %04f, max %04f) and tool 2 took %06f sec. (mean %04f, median \
     %04f, min %04f, max %04f)@\n\
     @\n"
    (Runs.sum_clock reached_only_1)
    (Runs.mean_clock reached_only_1)
    (Runs.median_clock reached_only_1)
    (Runs.min_clock reached_only_1)
    (Runs.max_clock reached_only_1)
    (Runs.sum_clock reached_only_2)
    (Runs.mean_clock reached_only_2)
    (Runs.median_clock reached_only_2)
    (Runs.min_clock reached_only_2)
    (Runs.max_clock reached_only_2);
  Format.printf
    "among tasks reached only by tool 1, tool 2 replied %03d nothing, %03d \
     timeout, %02d other and %02d killed@\n"
    (Runs.count_nothing report2_found_by_1_not_by_2)
    (Runs.count_timeout report2_found_by_1_not_by_2)
    (Runs.count_other report2_found_by_1_not_by_2)
    (Runs.count_killed report2_found_by_1_not_by_2);
  Format.printf
    "among tasks reached only by tool 2, tool 1 replied %03d nothing, %03d \
     timeout, %02d other and %02d killed@\n"
    (Runs.count_nothing report1_found_by_2_not_by_1)
    (Runs.count_timeout report1_found_by_2_not_by_1)
    (Runs.count_other report1_found_by_2_not_by_1)
    (Runs.count_killed report1_found_by_2_not_by_1);
  Format.printf "tasks solved only by tool 1:@\n  @[<v>%a@]@\n"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       Fpath.pp )
    (Runs.files reached_only_1);
  Format.printf "tasks solved only by tool 2:@\n  @[<v>%a@]@\n"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       Fpath.pp )
    (Runs.files reached_only_2)
