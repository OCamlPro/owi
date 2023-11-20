type Runtime_events.User.tag += Solver_check

type Runtime_events.User.tag += Solver_check_true

type Runtime_events.User.tag += Solver_check_false

type Runtime_events.User.tag += Solver_model

val check : Runtime_events.Type.span Runtime_events.User.t

val check_true : Runtime_events.Type.span Runtime_events.User.t

val check_false : Runtime_events.Type.span Runtime_events.User.t

val model : Runtime_events.Type.span Runtime_events.User.t

val with_ev :
  Runtime_events.Type.span Runtime_events.User.t -> (unit -> 'a) -> 'a
