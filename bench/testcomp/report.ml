let () =
  if Array.length Sys.argv < 2 then
    Format.ksprintf failwith "usage: %s <results file>" Sys.argv.(0)

let file = Sys.argv.(1)

let chan = open_in file

let runs = ref []

let parse_file () =
  try
    while true do
      let run = input_line chan in
      let result = input_line chan in
      let summary = input_line chan in
      runs := (run, result, summary) :: !runs
    done
  with End_of_file -> runs := List.rev !runs

let () = Fun.protect ~finally:(fun () -> close_in chan) parse_file

let runs = !runs

type run_result =
  | Nothing
  | Killed
  | Reached of float * float * float
  | Timeout of float * float * float
  | Bad of int * float * float * float

type run =
  { i : int
  ; res : run_result
  ; file : Fpath.t
  }

let ( let* ) o f = match o with Ok v -> f v | Error _ as e -> e

let ksprintf = Format.ksprintf

let error = Result.error

let parse_float s =
  match float_of_string_opt s with
  | None -> ksprintf error "malformed float %S" s
  | Some f -> Ok f

let parse_time t1 t2 t3 =
  let* t1 = parse_float t1 in
  let* t2 = parse_float t2 in
  let* t3 = parse_float t3 in
  Ok (t1, t2, t3)

let parse_int s =
  match int_of_string_opt s with
  | None -> ksprintf error "malformed int %S" s
  | Some i -> Ok i

let rm_empty_str l = List.filter (fun s -> s <> "") l

let parse_run (run, result, _summary) =
  let* counter, file =
    match String.split_on_char ' ' run |> rm_empty_str with
    | [ "Run"; counter; file ] -> Ok (counter, Fpath.v file)
    | _ -> ksprintf error "malformed run: %S" run
  in
  let* i =
    match String.split_on_char '/' counter |> rm_empty_str with
    | [ i; _total ] -> parse_int i
    | _ -> ksprintf error "malformed counter: %S" counter
  in
  let* res =
    match String.split_on_char ' ' result |> rm_empty_str with
    | [ "Reached"; "in"; t1; t2; t3 ] ->
      let* t1, t2, t3 = parse_time t1 t2 t3 in
      Ok (Reached (t1, t2, t3))
    | [ "Timeout"; "in"; t1; t2; t3 ] ->
      let* t1, t2, t3 = parse_time t1 t2 t3 in
      Ok (Timeout (t1, t2, t3))
    | [ "Other"; n; "in"; t1; t2; t3 ] ->
      let* n = parse_int n in
      let* t1, t2, t3 = parse_time t1 t2 t3 in
      Ok (Bad (n, t1, t2, t3))
    | [ "Nothing" ] -> Ok Nothing
    | [ "Killed" ] -> Ok Killed
    | _ -> ksprintf error "malformed result: %S" result
  in

  Ok { i; res; file }

let list_map f l =
  let err = ref None in
  try
    Ok
      (List.map
         (fun v ->
           match f v with
           | Error _e as e ->
             err := Some e;
             raise Exit
           | Ok v -> v )
         l )
  with Exit -> Option.get !err

let runs =
  match list_map parse_run runs with Ok runs -> runs | Error s -> failwith s

let count_total = List.length runs

let count_nothing =
  List.fold_left
    (fun count r -> match r.res with Nothing -> succ count | _ -> count)
    0 runs

let count_reached =
  List.fold_left
    (fun count r -> match r.res with Reached _ -> succ count | _ -> count)
    0 runs

let count_timeout =
  List.fold_left
    (fun count r -> match r.res with Timeout _ -> succ count | _ -> count)
    0 runs

let count_bad =
  List.fold_left
    (fun count r -> match r.res with Bad _ -> succ count | _ -> count)
    0 runs

let count_killed =
  List.fold_left
    (fun count r -> match r.res with Killed -> succ count | _ -> count)
    0 runs

let () =
  let fmt = Format.std_formatter in
  let pp = Format.fprintf in
  pp fmt "total = %d@\n" count_total;
  pp fmt "nothing = %d@\n" count_nothing;
  pp fmt "reached = %d@\n" count_reached;
  pp fmt "timeout = %d@\n" count_timeout;
  pp fmt "bad = %d@\n" count_bad;
  pp fmt "killed = %d@\n" count_killed

let () =
  let open Gnuplot in
  let gp = create () in

  let max_time =
    List.fold_left
      (fun current_max r ->
        match r.res with
        | Reached (t, _, _) | Timeout (t, _, _) | Bad (_, t, _, _) ->
          max t current_max
        | Killed | Nothing -> current_max )
      0. runs
    |> int_of_float |> ( + ) 5
  in

  let to_distrib times =
    List.init max_time (fun i ->
        List.fold_left
          (fun count x ->
            let x = int_of_float x in
            if x = i then count +. 1. else count )
          0. times )
  in

  let weight = 1 in
  let fill = `Solid in

  let times_reached =
    List.filter_map
      (fun r -> match r.res with Reached (t1, _, _) -> Some t1 | _ -> None)
      runs
    |> to_distrib
    |> Series.histogram ~title:"Owi (reached)"
         ~color:(`Rgb (0x7A, 0xA9, 0x5C))
         ~weight ~fill
  in

  let times_timeout =
    List.filter_map
      (fun r -> match r.res with Timeout (t1, _, _) -> Some t1 | _ -> None)
      runs
    |> to_distrib
    |> Series.histogram ~title:"Owi (timeout)"
         ~color:(`Rgb (0xA7, 0x00, 0x1E))
         ~weight ~fill
  in

  let times_bad =
    List.filter_map
      (fun r -> match r.res with Bad (_, t1, _, _) -> Some t1 | _ -> None)
      runs
    |> to_distrib
    |> Series.histogram ~title:"Owi (bad)"
         ~color:(`Rgb (0xC0, 0xE2, 0xE9))
         ~weight ~fill
  in

  let title = "Distribution of execution times" in
  let output = Output.create (`Png "distrib.png") in
  let use_grid = true in
  let fill = `Solid in

  let min_time = ~-.(float_of_int weight) /. 2. in

  let range = Range.X (min_time, float_of_int max_time) in
  let labels = Labels.create ~x:"time (s)" ~y:"number of tests" () in
  let custom = [ ("set style histogram rowstacked", "") ] in

  plot_many ~output ~title ~use_grid ~fill ~range ~labels ~custom gp
    [ times_reached; times_timeout; times_bad ];

  close gp
