include Logs.LOG

val main_src : Logs.src

val is_bench_enabled : unit -> bool

val bench_fn : string -> (unit -> 'a) -> 'a

val bench : 'a Logs.log

val setup : Fmt.style_renderer option -> Logs.level option -> bench:bool -> unit
