type t = Run.t list

let empty = []

let add hd tl = hd :: tl

let count_all runs = List.length runs

let count_nothing runs =
  List.fold_left
    (fun count r -> match r.Run.res with Nothing _ -> succ count | _ -> count)
    0 runs

let count_reached runs =
  List.fold_left
    (fun count r -> match r.Run.res with Reached _ -> succ count | _ -> count)
    0 runs

let count_timeout runs =
  List.fold_left
    (fun count r -> match r.Run.res with Timeout _ -> succ count | _ -> count)
    0 runs

let count_other runs =
  List.fold_left
    (fun count r -> match r.Run.res with Other _ -> succ count | _ -> count)
    0 runs

let count_killed runs =
  List.fold_left
    (fun count r -> match r.Run.res with Killed _ -> succ count | _ -> count)
    0 runs

let keep_nothing = List.filter Run.is_nothing

let keep_reached = List.filter Run.is_reached

let keep_timeout = List.filter Run.is_timeout

let keep_other = List.filter Run.is_other

let keep_killed = List.filter Run.is_killed

let min_clock runs =
  match runs with
  | [] -> 0.
  | hd :: runs ->
    List.fold_left
      (fun current_min r ->
        let clock = Run.clock r in
        min clock current_min )
      (Run.clock hd) runs

let max_clock runs =
  List.fold_left
    (fun current_max r ->
      let clock = Run.clock r in
      max clock current_max )
    0. runs

let sum_clock runs = List.fold_left (fun sum r -> Run.clock r +. sum) 0. runs

let mean_clock runs = sum_clock runs /. (count_all runs |> float_of_int)

let sum_utime runs = List.fold_left (fun sum r -> Run.utime r +. sum) 0. runs

let mean_utime runs = sum_utime runs /. (count_all runs |> float_of_int)

let sum_stime runs = List.fold_left (fun sum r -> Run.stime r +. sum) 0. runs

let mean_stime runs = sum_stime runs /. (count_all runs |> float_of_int)

let to_distribution ~max_time runs =
  List.init max_time (fun i ->
      List.fold_left
        (fun count r ->
          let clock = Run.clock r |> int_of_float in
          if clock = i then count +. 1. else count )
        0. runs )

let pp_quick_results fmt results =
  let nothing = ref 0 in
  let reached = ref 0 in
  let timeout = ref 0 in
  let killed = ref 0 in
  let other = ref 0 in
  List.iter
    (fun result ->
      match result.Run.res with
      | Nothing _ -> incr nothing
      | Reached _ -> incr reached
      | Timeout _ -> incr timeout
      | Killed _ -> incr killed
      | Other _ -> incr other )
    results;
  Format.fprintf fmt
    "Nothing: %6i    Reached: %6i    Timeout: %6i    Other: %6i    Killed: %6i"
    !nothing !reached !timeout !other !killed

let map = List.map
