type formatter = Stdlib.Format.formatter

val pp : formatter -> ('a, formatter, unit) format -> 'a

val pp_err : ('a, formatter, unit) format -> 'a

val pp_std : ('a, formatter, unit) format -> 'a

val pp_nothing : formatter -> unit -> unit

val pp_space : formatter -> unit -> unit

val pp_bool : formatter -> bool -> unit

val pp_char : formatter -> char -> unit

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

val pp_newline : formatter -> unit -> unit

val sprintf : ('a, unit, string) format -> 'a

val ksprintf : (string -> 'a) -> ('b, unit, string, 'a) format4 -> 'b

val asprintf : ('a, formatter, unit, string) format4 -> 'a

val kasprintf : (string -> 'a) -> ('b, formatter, unit, 'a) format4 -> 'b
