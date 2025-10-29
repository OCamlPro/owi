type stats = { solver_time : Mtime.Span.t Atomic.t }

let handle_solver_time stats f =
  if Log.is_bench_enabled () then
    let counter = Mtime_clock.counter () in
    let res = f () in
    let solver_time = Mtime_clock.count counter in
    let rec atomic_set () =
      let curr = Atomic.get stats.solver_time in
      let success =
        Atomic.compare_and_set stats.solver_time curr
          (Mtime.Span.add curr solver_time)
      in
      if success then res else atomic_set ()
    in
    atomic_set ()
  else f ()
