let main_src = Logs.Src.create "owi" ~doc:"Owi's main logs"

let bench_src = Logs.Src.create "owi-bench" ~doc:"Owi's benchmark logs"

include (val Logs.src_log main_src : Logs.LOG)

module Bench_log = (val Logs.src_log bench_src : Logs.LOG)

let bench_enabled = ref false

let bench_fn str fn =
  if !bench_enabled then (
    let c = Mtime_clock.counter () in
    let r = fn () in
    Bench_log.info (fun m ->
      m "%s : %a" str Mtime.Span.pp (Mtime_clock.count c) );
    r )
  else fn ()

let bench f = if !bench_enabled then Bench_log.info f else ()

let setup style_renderer level ~with_timings =
  bench_enabled := with_timings;
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs.format_reporter ())
