type t =
  | Nothing of Rusage.t
  | Signaled of Rusage.t * int
  | Stopped of Rusage.t * int
  | Reached of Rusage.t
  | Timeout of Rusage.t
  | Other of Rusage.t * int

let is_nothing = function Nothing _ -> true | _ -> false

let is_killed = function Signaled _ | Stopped _ -> true | _ -> false

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
  | Other (t, code) ->
    Format.fprintf fmt "Other %i in %g %g %g" code t.clock t.utime t.stime
  | Signaled (t, code) ->
    Format.fprintf fmt "Signaled %i in %g %g %g" code t.clock t.utime t.stime
  | Stopped (t, code) ->
    Format.fprintf fmt "Stopped %i in %g %g %g" code t.clock t.utime t.stime
