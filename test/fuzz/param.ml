(* TODO: OCaml Arg module could not be used (conflict with crowbar)
   https://github.com/OCamlPro/owi/pull/28#discussion_r1231983824 *)

let debug = false

let optimize_fuzzing = false

let reference_fuzzing = false

let symbolic_fuzzing = false

let full_symbolic_fuzzing = true

let initial_fuel = 100

let allow_partial_timeout = true

let max_time_execution = 0.01 (* seconds *)
