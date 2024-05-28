let make runs output_dir =
  let gen_time =
    let t = Unix.localtime @@ Unix.gettimeofday () in
    Format.sprintf "%d-%02d-%02d at %02dh%02dm%02ds" (1900 + t.tm_year)
      (1 + t.tm_mon) t.tm_mday t.tm_hour t.tm_min t.tm_sec
  in

  let open Tyxml in
  let open Html in
  let title = txt "🐌 Benchmarks results" in

  let mk_details summary_txt runs =
    div
      [ details
          (summary [ txt summary_txt ])
          ~a:[]
          [ ul
              (Runs.map
                 (fun run ->
                   li
                     [ Format.kasprintf txt "run %d: " run.Run.i
                     ; code [ Format.kasprintf txt "%a" Fpath.pp run.file ]
                     ] )
                 runs )
          ]
      ]
  in

  let reached = Runs.keep_reached runs in
  let killed = Runs.keep_killed runs in
  let nothing = Runs.keep_nothing runs in
  let other = Runs.keep_other runs in
  let timeout = Runs.keep_timeout runs in

  let body =
    div
      [ p
          [ Format.ksprintf txt
              "Here are the benchmarks results for Owi, generated on the %s."
              gen_time
          ]
      ; h2 [ txt "Score" ]
      ; div
          [ img
              ~a:[ a_width 500; a_class [] ]
              ~src:"results_owi_count.svg" ~alt:"Score made by Owi" ()
          ]
      ; h2 [ txt "Distribution of execution times" ]
      ; div
          [ img
              ~a:[ a_width 500; a_class [] ]
              ~src:"results_owi_time_distribution.png"
              ~alt:"Distribution of execution times" ()
          ]
      ; h2 [ txt "Statistics" ]
      ; div
          [ p
              [ Format.ksprintf txt "Sum of clock times (reached): %f"
                  (Runs.sum_clock reached)
              ]
          ; p
              [ Format.ksprintf txt "Mean of clock times (reached): %f"
                  (Runs.mean_clock reached)
              ]
          ; p
              [ Format.ksprintf txt "Sum of utime times (reached): %f"
                  (Runs.sum_utime reached)
              ]
          ; p
              [ Format.ksprintf txt "Mean of utime times (reached): %f"
                  (Runs.mean_utime reached)
              ]
          ; p
              [ Format.ksprintf txt "Sum of stime times (reached): %f"
                  (Runs.sum_stime reached)
              ]
          ; p
              [ Format.ksprintf txt "Mean of stime times (reached): %f"
                  (Runs.mean_stime reached)
              ]
          ]
      ; h2 [ txt "Details of results" ]
      ; mk_details "Killed runs" killed
      ; mk_details "Other runs" other
      ; mk_details "Nothing runs" nothing
      ; mk_details "Reached runs" reached
      ; mk_details "Timeout runs" timeout
      ]
  in

  let style_file = Fpath.v "style.css" in

  let styles =
    List.map
      (fun href -> link ~rel:[ `Stylesheet ] ~href ())
      [ Fpath.to_string style_file ]
  in
  let meta = [ meta ~a:[ a_name "charset"; a_content "utf-8" ] () ] in
  let head = head (Html.title title) (meta @ styles) in
  let body = Html.body [ main [ h1 [ title ]; body ] ] in

  let html = html ~a:[ a_lang "en" ] head body in

  let index = Fpath.(output_dir // v "index.html") in
  let index_chan = open_out (Fpath.to_string index) in
  let fmt = Format.formatter_of_out_channel index_chan in

  Format.fprintf fmt "%a@\n" (Tyxml.Html.pp ~indent:false ()) html;

  close_out index_chan;

  let style =
    {css|body{
    color: #444;
    background-color: #EEE;
    margin: 40px auto;
    margin-left: 25%;
    line-height: 1.6em;
    font-size: 18px;
    padding: 0;

}
code {
  font-size: 12px;
}|css}
  in

  let chan = open_out Fpath.(output_dir // style_file |> to_string) in
  let fmt = Format.formatter_of_out_channel chan in
  Format.fprintf fmt "%s@\n" style;
  close_out chan
