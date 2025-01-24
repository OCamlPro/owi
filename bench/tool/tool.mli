type t

val to_short_name : t -> string

val to_reference_name : t -> string

val mk_owi :
     concolic:bool
  -> workers:int
  -> optimisation_level:int
  -> solver:Smtml.Solver_type.t
  -> t

val mk_klee : unit -> t

val fork_and_run_on_file :
     i:int
  -> fmt:Format.formatter
  -> output_dir:Fpath.t
  -> file:Fpath.t
  -> tool:t
  -> timeout:float
  -> (Report.Run_result.t, Rresult.R.msg) Result.t
