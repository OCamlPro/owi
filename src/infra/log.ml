let main_src = Logs.Src.create "owi" ~doc:"Owi's main logs"

let bench_src = Logs.Src.create "owi-bench" ~doc:"Owi's benchmark logs"

include (val Logs.src_log main_src : Logs.LOG)

let is_bench_enabled () =
  match Logs.Src.level bench_src with None -> false | Some _ -> true

let is_debug_enabled () =
  match Logs.Src.level main_src with
  | Some Logs.Debug -> true
  | None | Some _ -> false

let bench_fn str fn =
  match Logs.Src.level bench_src with
  | None -> fn ()
  | Some _ ->
    let c = Mtime_clock.counter () in
    let r = fn () in
    Logs.info ~src:bench_src (fun m ->
      m "%s : %a" str Mtime.Span.pp (Mtime_clock.count c) );
    r

let bench f =
  match Logs.Src.level bench_src with
  | None -> ()
  | Some _ -> Logs.info ~src:bench_src f

let setup style_renderer level ~bench =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.Src.set_level main_src level;
  Logs.Src.set_level bench_src (if bench then Some Logs.Info else None);
  Logs.set_reporter (Logs.format_reporter ())
