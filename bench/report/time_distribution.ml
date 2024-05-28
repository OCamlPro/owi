let weight = 1

let fill = `Solid

let use_grid = true

let min_time = ~-.(float_of_int weight) /. 2.

let labels = Gnuplot.Labels.create ~x:"time (s)" ~y:"number of tests" ()

let title = "Distribution of execution times"

let custom = [ ("set style histogram rowstacked", "") ]

let make runs output_dir reference_name =
  let gp = Gnuplot.create () in

  let max_time = Runs.max_clock runs |> int_of_float |> ( + ) 5 in

  let mk_time_distrib title color runs =
    Runs.to_distribution ~max_time runs
    |> Gnuplot.Series.histogram ~title ~color ~weight ~fill
  in

  let times_reached =
    Runs.keep_reached runs
    |> mk_time_distrib "Reached" (Color.to_rgb Color.reached)
  in

  let times_timeout =
    Runs.keep_timeout runs
    |> mk_time_distrib "Timeout" (Color.to_rgb Color.timeout)
  in

  let times_other =
    Runs.keep_other runs |> mk_time_distrib "Other" (Color.to_rgb Color.other)
  in

  let times_nothing =
    Runs.keep_nothing runs
    |> mk_time_distrib "Nothing" (Color.to_rgb Color.nothing)
  in

  let times_killed =
    Runs.keep_killed runs
    |> mk_time_distrib "Killed" (Color.to_rgb Color.killed)
  in

  let output =
    Gnuplot.Output.create
      (`Png
        Fpath.(
          output_dir
          // v
               (Format.sprintf "results_%s_time_distribution.png" reference_name)
          |> to_string ) )
  in

  let range = Gnuplot.Range.X (min_time, float_of_int max_time) in

  let count_killed = Runs.count_killed runs in
  let count_other = Runs.count_other runs in
  let count_timeout = Runs.count_timeout runs in
  let count_reached = Runs.count_reached runs in
  let count_nothing = Runs.count_nothing runs in

  let data =
    [ (times_reached, count_reached)
    ; (times_timeout, count_timeout)
    ; (times_nothing, count_nothing)
    ; (times_other, count_other)
    ; (times_killed, count_killed)
    ]
    |> List.filter_map (function _, 0 -> None | times, _n -> Some times)
  in

  Gnuplot.plot_many ~output ~title ~use_grid ~fill ~range ~labels ~custom gp
    data;

  Gnuplot.close gp
