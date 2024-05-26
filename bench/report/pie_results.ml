open Bos

let string_of_float f = Format.sprintf "%f" f

let output_format = "SVG"

let w = 400 |> string_of_int

let h = 400 |> string_of_int

let legend_color = Color.to_string Color.dark

let legend_size = 10 |> string_of_int

let title = "Owi results"

let title_color = Color.to_string Color.dark

let title_size = 15 |> string_of_int

let background_color = Color.to_string Color.white

let ratio_w_h =
  (* must be between 0 and 1 *)
  1. |> string_of_float

let pie_line_width = 1 |> string_of_int

let pie_line_color = Color.to_string Color.lines

let percent_explode =
  (* must be between 0 and 1 *)
  0. |> string_of_float

let percent_extrusion =
  (* must be between 0 and 1 *)
  0. |> string_of_float

let margin = 10. |> string_of_float

let mk_value (n, color, ratio_explode, name) =
  Format.sprintf "%d%s:%f:%s" n (Color.to_string color) ratio_explode name

let make runs output_dir =
  let out = Fpath.(output_dir // v "results_owi_count.svg") in

  let flags =
    [ "-o"
    ; Fpath.to_string out
    ; "-f"
    ; output_format
    ; "-w"
    ; w
    ; "-h"
    ; h
    ; "-l"
    ; legend_color
    ; "-L"
    ; legend_size
    ; "-t"
    ; title
    ; "-T"
    ; title_color
    ; "-b"
    ; background_color
    ; "-r"
    ; ratio_w_h
    ; "-c"
    ; pie_line_width
    ; "-C"
    ; pie_line_color
    ; "-d"
    ; percent_explode
    ; "-e"
    ; percent_extrusion
    ; "-m"
    ; margin
    ; "-s"
    ; title_size
    ]
    |> Cmd.of_list
  in

  let count_killed = Runs.count_killed runs in
  let count_other = Runs.count_other runs in
  let count_timeout = Runs.count_timeout runs in
  let count_reached = Runs.count_reached runs in
  let count_nothing = Runs.count_nothing runs in

  let values =
    [ (count_killed, Color.killed, 0., Format.sprintf "Killed (%d)" count_killed)
    ; (count_other, Color.other, 0., Format.sprintf "Other (%d)" count_other)
    ; ( count_timeout
      , Color.timeout
      , 0.
      , Format.sprintf "Timeout (%d)" count_timeout )
    ; ( count_reached
      , Color.reached
      , 0.
      , Format.sprintf "Reached (%d)" count_reached )
    ; ( count_nothing
      , Color.nothing
      , 0.
      , Format.sprintf "Nothing (%d)" count_nothing )
    ]
    |> List.filter_map (fun ((count, _, _, _) as v) ->
           if count = 0 then None else Some v )
    |> List.sort (fun (c1, _, _, _) (c2, _, _, _) -> compare c2 c1)
    |> List.map mk_value |> Cmd.of_list
  in

  let pie = Cmd.(v "pie" %% flags %% values) in

  OS.Cmd.run pie
