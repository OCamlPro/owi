type ('a, 'conf) cond = 'conf -> 'a option

type fuzz_conf =
  | Symbolic
  | Concrete

type 'a condgen = ('a, fuzz_conf) cond

val return : 'a -> 'b -> 'a option

val bind : ('a, 'conf) cond -> ('a -> ('b, 'conf) cond) -> ('b, 'conf) cond

val ( let* ) : ('a, 'b) cond -> ('a -> ('c, 'b) cond) -> ('c, 'b) cond

val run : ('a, 'conf) cond -> 'conf -> 'a option

val lift1 : ('a -> 'b) -> ('a, 'c) cond -> ('b, 'c) cond

val ( let+ ) : ('a, 'c) cond -> ('a -> 'b) -> ('b, 'c) cond

val flatten : ('a, 'conf) cond list -> 'conf -> 'a list option

val ( *@* ) :
  ('a list, 'conf) cond -> ('a list, 'conf) cond -> ('a list, 'conf) cond

val ( *@ ) : ('a list, 'conf) cond -> 'a list -> ('a list, 'conf) cond

val ( **::** ) :
  ('a, 'conf) cond -> ('a list, 'conf) cond -> ('a list, 'conf) cond

val ( **:: ) : ('a, 'conf) cond -> 'a list -> ('a list, 'conf) cond

val not_symbolic : 'a -> fuzz_conf -> 'a option

type 'conf configurator = { configure : 'a. ('a, 'conf) cond -> 'a }

val with_unwrapping_configurator :
  ('conf configurator -> 'b) -> ('b, 'conf) cond
