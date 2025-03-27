(* TODO: OCaml Arg module could not be used (conflict with crowbar)
   https://github.com/OCamlPro/owi/pull/28#discussion_r1231983824 *)

(** What kind of differential fuzzing should be performed. *)
let optimize_fuzzing = true

let reference_fuzzing = false

let symbolic_fuzzing = true

(** Enable debug mode, will slow down things. *)
let debug = false

(** Initial size of the fuel. The bigger it is, the bigger the generated
    programs will be. *)
let initial_fuel = 100

(** Maximum execution time for a module on a given interpreter. *)
let max_time_execution = 2. (* seconds *)

(** Do not error if one of the two execution led to a timeout but not the other.
*)
let allow_partial_timeout = true

(** Save generated modules in files *)
let save_modules = false

(** Directory used to save generated modules *)
let output_dir = Fpath.v "generated_modules"
