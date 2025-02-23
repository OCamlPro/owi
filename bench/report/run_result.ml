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
  | Timeout t -> Format.fprintf fmt "Timeout in %a" Rusage.pp t
  | Nothing t -> Format.fprintf fmt "Nothing in %a" Rusage.pp t
  | Reached t -> Format.fprintf fmt "Reached in %a" Rusage.pp t
  | Other (t, code) -> Format.fprintf fmt "Other %i in %a" code Rusage.pp t
  | Signaled (t, code) ->
    Format.fprintf fmt "Signaled %i in %a" code Rusage.pp t
  | Stopped (t, code) -> Format.fprintf fmt "Stopped %i in %a" code Rusage.pp t
