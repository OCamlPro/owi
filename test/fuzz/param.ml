(* TODO: OCaml Arg module could not be used (conflict with crowbar)
   https://github.com/OCamlPro/owi/pull/28#discussion_r1231983824 *)

let debug = false

let optimize_fuzzing = true

let reference_fuzzing = false

let symbolic_fuzzing = true

let initial_fuel = 100

let allow_partial_timeout = true

let max_time_execution = 0.01 (* seconds *)

let save_modules = true (* Set to false to disable saving modules *)

let output_dir = "generated_modules" (* Directory to save modules *)