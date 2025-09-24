include Logs.LOG

val bench_fn : string -> (unit -> 'a) -> 'a

val bench : 'a Logs.log

val setup : Fmt.style_renderer option -> Logs.level option -> unit
