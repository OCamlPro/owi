type t =
  { i : int
  ; res : Run_result.t
  ; file : Fpath.t
  }

let clock { res; _ } =
  match res with
  | Run_result.Reached t | Timeout t | Nothing t | Killed t | Other (_, t) ->
    t.clock

let is_reached { res; _ } = Run_result.is_reached res

let is_timeout { res; _ } = Run_result.is_timeout res

let is_nothing { res; _ } = Run_result.is_nothing res

let is_killed { res; _ } = Run_result.is_killed res

let is_other { res; _ } = Run_result.is_other res

let pp_header total fmt (i, file) =
  Format.fprintf fmt "Run %d/%d: %a" i total Fpath.pp file
