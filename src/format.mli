include module type of Stdlib.Format

val pp : formatter -> ('a, formatter, unit) format -> 'a

val pp_nothing : formatter -> unit -> unit

val pp_space : formatter -> unit -> unit

val pp_bool : formatter -> bool -> unit

val pp_int : formatter -> int -> unit

val pp_flush : formatter -> unit -> unit

val pp_list :
     ?pp_sep:(formatter -> unit -> unit)
  -> (formatter -> 'a -> unit)
  -> formatter
  -> 'a list
  -> unit

val pp_array :
     ?pp_sep:(formatter -> unit -> unit)
  -> (formatter -> 'a -> unit)
  -> formatter
  -> 'a array
  -> unit

val pp_iter :
     ?pp_sep:(formatter -> unit -> unit)
  -> (('a -> unit) -> 'b -> unit)
  -> (formatter -> 'a -> unit)
  -> formatter
  -> 'b
  -> unit

val pp_string : formatter -> string -> unit

val pp_option :
     ?none:(formatter -> unit -> unit)
  -> (formatter -> 'a -> unit)
  -> formatter
  -> 'a option
  -> unit
