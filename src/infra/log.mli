include Logs.LOG

val bench_src : Logs.Src.t

val main_src : Logs.Src.t

val is_debug_enabled : unit -> bool

val is_bench_enabled : unit -> bool

val bench_fn : string -> (unit -> 'a) -> 'a

val bench : 'a Logs.log

val setup : Fmt.style_renderer option -> Logs.level option -> bench:bool -> unit
