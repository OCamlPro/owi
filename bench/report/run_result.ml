type t =
  | Nothing of Rusage.t
  | Killed of Rusage.t
  | Reached of Rusage.t
  | Timeout of Rusage.t
  | Other of int * Rusage.t

let is_nothing = function Nothing _ -> true | _ -> false

let is_killed = function Killed _ -> true | _ -> false

let is_reached = function Reached _ -> true | _ -> false

let is_timeout = function Timeout _ -> true | _ -> false

let is_other = function Other _ -> true | _ -> false

let pp fmt = function
  | Timeout t ->
    Format.fprintf fmt "Timeout in %g %g %g" t.clock t.utime t.stime
  | Nothing t ->
    Format.fprintf fmt "Nothing in %g %g %g" t.clock t.utime t.stime
  | Reached t ->
    Format.fprintf fmt "Reached in %g %g %g" t.clock t.utime t.stime
  | Other (code, t) ->
    Format.fprintf fmt "Other %i in %g %g %g" code t.clock t.utime t.stime
  | Killed t -> Format.fprintf fmt "Killed in %g %g %g" t.clock t.utime t.stime
