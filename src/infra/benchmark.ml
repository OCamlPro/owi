type stats = { solver_time : Mtime.Span.t Atomic.t }

let handle_time_span atomic_span f =
  if Log.is_bench_enabled () then
    let counter = Mtime_clock.counter () in
    let res = f () in
    let span = Mtime_clock.count counter in
    let rec atomic_set () =
      let curr = Atomic.get atomic_span in
      let success =
        Atomic.compare_and_set atomic_span curr (Mtime.Span.add curr span)
      in
      if success then res else atomic_set ()
    in
    atomic_set ()
  else f ()

let with_utime f =
  if Log.is_bench_enabled () then
    let before = (Unix.times ()).tms_utime in
    let r = f () in
    let after = (Unix.times ()).tms_utime in
    (r, Some (after -. before))
  else (f (), None)
