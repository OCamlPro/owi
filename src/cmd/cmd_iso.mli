val cmd :
     deterministic_result_order:bool
  -> fail_mode:Symbolic_parameters.fail_mode
  -> exploration_strategy:Symbolic_parameters.Exploration_strategy.t
  -> files:Fpath.t list
  -> model_format:Model.output_format
  -> no_assert_failure_expression_printing:bool
  -> no_stop_at_failure:bool
  -> no_value:bool
  -> solver:Smtml.Solver_type.t
  -> unsafe:bool
  -> workers:int
  -> workspace:Fpath.t option
  -> model_out_file:Fpath.t option
  -> with_breadcrumbs:bool
  -> unit Result.t
