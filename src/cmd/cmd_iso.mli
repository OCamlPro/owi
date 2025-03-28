val cmd :
     debug:bool
  -> deterministic_result_order:bool
  -> fail_mode:Cmd_sym.fail_mode
  -> files:Fpath.t list
  -> model_output_format:Cmd_utils.model_output_format
  -> no_assert_failure_expression_printing:bool
  -> no_stop_at_failure:bool
  -> no_value:bool
  -> solver:Smtml.Solver_type.t
  -> unsafe:bool
  -> workers:int
  -> workspace:Fpath.t
  -> unit Result.t
