let main_src = Logs.Src.create "owi" ~doc:"Owi's main logs"

let bench_src = Logs.Src.create "owi-bench" ~doc:"Owi's benchmark logs"

module Main_log = (val Logs.src_log main_src : Logs.LOG)

include Main_log

module Bench_log = (val Logs.src_log bench_src : Logs.LOG)

let bench_fn str fn =
  let c = Mtime_clock.counter () in
  let r = fn () in
  Bench_log.info (fun m -> m "%s : %a" str Mtime.Span.pp (Mtime_clock.count c));
  r

let bench = Bench_log.info

let setup style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs.format_reporter ())
