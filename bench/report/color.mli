type t

val reached : t

val other : t

val nothing : t

val timeout : t

val killed : t

val lines : t

val dark : t

val white : t

val to_string : t -> string

val to_rgb : t -> [> `Rgb of int * int * int ]
